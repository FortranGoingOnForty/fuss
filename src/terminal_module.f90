module terminal_module
    implicit none

    ! Shared temp file for reducing disk I/O
    character(len=*), parameter :: FUSS_TEMP = '/tmp/fuss_tmp.txt'

contains

    subroutine enter_alternate_screen()
        ! Switch to alternate screen buffer (like vim, less, htop)
        ! This preserves the main terminal content
        print '(A)', achar(27) // '[?1049h'
    end subroutine enter_alternate_screen

    subroutine exit_alternate_screen()
        ! Return to main screen buffer
        ! Restores terminal to state before enter_alternate_screen()
        print '(A)', achar(27) // '[?1049l'
    end subroutine exit_alternate_screen

    subroutine clear_screen()
        ! ANSI escape code to clear screen and move cursor to top
        ! In alternate screen buffer, we just need to home cursor and clear
        print '(A)', achar(27) // '[H' // achar(27) // '[2J'
    end subroutine clear_screen

    subroutine enable_raw_mode()
        integer :: status
        ! Use stty cbreak mode (processes newlines correctly) instead of raw
        call execute_command_line('stty cbreak -echo < /dev/tty', exitstat=status)
    end subroutine enable_raw_mode

    subroutine disable_raw_mode()
        integer :: status
        ! Restore terminal
        call execute_command_line('stty sane < /dev/tty', exitstat=status)
    end subroutine disable_raw_mode

    subroutine flush_stdin()
        integer :: status
        ! Flush any buffered input from stdin
        ! Use a simple bash read with very short timeout to drain buffer without blocking
        call execute_command_line('while read -t 0.001 -n 1 < /dev/tty 2>/dev/null; do :; done', &
                                  exitstat=status)
    end subroutine flush_stdin

    subroutine wait_for_key(key)
        character(len=1), intent(out) :: key
        ! Flush any buffered input before waiting for keypress
        ! This prevents accidental double-inputs after long operations
        call flush_stdin()
        call read_key(key)
    end subroutine wait_for_key

    subroutine read_key_with_timeout(key, timeout_ms, timed_out)
        ! Read a key with timeout (in milliseconds)
        ! If timeout occurs, timed_out is set to .true. and key is set to null
        character(len=1), intent(out) :: key
        integer, intent(in) :: timeout_ms
        logical, intent(out) :: timed_out
        integer :: status
        character(len=256) :: cmd
        integer :: unit_num, iostat

        timed_out = .false.
        key = achar(0)

        ! Use bash read with timeout to read a single character
        ! Timeout is in fractional seconds
        write(cmd, '(A,F6.3,A)') 'bash -c "read -t ', real(timeout_ms)/1000.0, &
            ' -n 1 -s key < /dev/tty && echo -n $key" > ' // FUSS_TEMP // ' 2>/dev/null'

        call execute_command_line(trim(cmd), exitstat=status)

        if (status /= 0) then
            ! Timeout occurred (read returned non-zero)
            timed_out = .true.
            return
        end if

        ! Read the character from temp file
        open(newunit=unit_num, file=FUSS_TEMP, status='old', action='read', iostat=iostat)
        if (iostat == 0) then
            read(unit_num, '(A1)', iostat=iostat) key
            close(unit_num, status='delete')
            if (iostat /= 0) then
                ! Empty file means timeout
                timed_out = .true.
                key = achar(0)
            end if
        else
            timed_out = .true.
        end if

        ! Handle arrow keys and escape sequences
        if (.not. timed_out .and. key == achar(27)) then
            ! Detected ESC - try to read next char quickly
            write(cmd, '(A)') 'bash -c "read -t 0.05 -n 1 -s key < /dev/tty && echo -n $key" > ' // &
                              FUSS_TEMP // ' 2>/dev/null'
            call execute_command_line(trim(cmd), exitstat=status)

            if (status == 0) then
                open(newunit=unit_num, file=FUSS_TEMP, status='old', action='read', iostat=iostat)
                if (iostat == 0) then
                    read(unit_num, '(A1)', iostat=iostat) key
                    close(unit_num, status='delete')

                    ! Check for arrow keys (ESC [ A/B/C/D)
                    if (key == '[') then
                        ! Read final character
                        write(cmd, '(A)') 'bash -c "read -t 0.05 -n 1 -s key < /dev/tty && echo -n $key" > ' // &
                                          FUSS_TEMP // ' 2>/dev/null'
                        call execute_command_line(trim(cmd), exitstat=status)
                        if (status == 0) then
                            open(newunit=unit_num, file=FUSS_TEMP, status='old', action='read', iostat=iostat)
                            if (iostat == 0) then
                                read(unit_num, '(A1)', iostat=iostat) key
                                close(unit_num, status='delete')
                            end if
                        end if
                    else if (key >= 'a' .and. key <= 'z') then
                        ! Alt-letter: encode as control char
                        key = achar(1 + ichar(key) - ichar('a'))
                    end if
                end if
            else
                ! Just ESC alone
                key = achar(27)
            end if
        end if
    end subroutine read_key_with_timeout

    subroutine read_key(key)
        character(len=1), intent(out) :: key
        character(len=3) :: escape_seq
        integer :: iostat, tty_unit

        ! Open /dev/tty for reading
        open(newunit=tty_unit, file='/dev/tty', status='old', action='read', iostat=iostat)
        if (iostat /= 0) then
            key = 'q'  ! If we can't open tty, quit
            return
        end if

        ! Read one character
        read(tty_unit, '(A1)', iostat=iostat, advance='no') key

        ! Check for escape sequence (arrow keys or alt-key combos)
        if (key == achar(27)) then
            ! Read just the first character after ESC
            read(tty_unit, '(A1)', iostat=iostat, advance='no') escape_seq(1:1)

            if (escape_seq(1:1) == '[') then
                ! Arrow key sequence: ESC[A/B/C/D - need to read one more char
                read(tty_unit, '(A1)', iostat=iostat, advance='no') escape_seq(2:2)
                key = escape_seq(2:2)  ! Return A, B, C, or D
            else if (escape_seq(1:1) >= 'a' .and. escape_seq(1:1) <= 'z') then
                ! Alt-letter sequence: ESC followed by letter
                ! Encode as ASCII control characters (1-26 for alt-a through alt-z)
                ! This keeps us in valid ASCII range [0-127]
                ! e.g., alt-g returns achar(7) = ASCII BEL
                key = achar(1 + ichar(escape_seq(1:1)) - ichar('a'))
            else if (iostat /= 0) then
                ! Just ESC key alone (no following character or timeout)
                key = achar(27)
            end if
        end if

        close(tty_unit)
    end subroutine read_key

    subroutine read_line(prompt, line)
        character(len=*), intent(in) :: prompt
        character(len=*), intent(out) :: line
        integer :: status, iostat

        ! Show prompt
        print '(A)', trim(prompt)

        ! Temporarily restore canonical mode for line input
        call execute_command_line('stty icanon echo < /dev/tty', exitstat=status)

        ! Read line from terminal
        read(*, '(A)', iostat=iostat) line

        ! Restore cbreak mode
        call execute_command_line('stty cbreak -echo < /dev/tty', exitstat=status)
    end subroutine read_line

    subroutine get_terminal_height(height)
        integer, intent(out) :: height
        integer :: iostat, unit_num, status
        character(len=256) :: env_val

        height = 24  ! Default fallback

        ! Try method 1: Use stty size to get terminal dimensions
        call execute_command_line('stty size < /dev/tty 2>/dev/null | cut -d" " -f1 > ' // FUSS_TEMP // '', &
                                  exitstat=status)

        if (status == 0) then
            open(newunit=unit_num, file=FUSS_TEMP, status='old', action='read', iostat=iostat)
            if (iostat == 0) then
                read(unit_num, *, iostat=iostat) height
                close(unit_num, status='delete')
                ! Sanity check
                if (height >= 10 .and. height <= 200) return
            end if
        end if

        ! Try method 2: tput lines
        call execute_command_line('tput lines < /dev/tty > ' // FUSS_TEMP // ' 2>/dev/null', exitstat=status)

        if (status == 0) then
            open(newunit=unit_num, file=FUSS_TEMP, status='old', action='read', iostat=iostat)
            if (iostat == 0) then
                read(unit_num, *, iostat=iostat) height
                close(unit_num, status='delete')
                ! Sanity check
                if (height >= 10 .and. height <= 200) return
            end if
        end if

        ! Try method 3: $LINES environment variable
        call get_environment_variable('LINES', env_val, status=iostat)
        if (iostat == 0 .and. len_trim(env_val) > 0) then
            read(env_val, *, iostat=iostat) height
            if (iostat == 0 .and. height >= 10 .and. height <= 200) return
        end if

        ! Fallback
        height = 24
    end subroutine get_terminal_height

end module terminal_module
