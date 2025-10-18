module terminal_module
    implicit none

contains

    subroutine clear_screen()
        ! ANSI escape code to clear screen and move cursor to top
        print '(A)', achar(27) // '[2J' // achar(27) // '[H'
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

        ! Check for escape sequence (arrow keys)
        if (key == achar(27)) then
            read(tty_unit, '(A2)', iostat=iostat, advance='no') escape_seq
            if (escape_seq(1:1) == '[') then
                key = escape_seq(2:2)  ! Return A, B, C, or D
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

end module terminal_module
