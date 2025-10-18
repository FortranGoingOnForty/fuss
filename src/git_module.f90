module git_module
    use iso_fortran_env, only: error_unit
    use types_module
    implicit none

contains

    subroutine get_dirty_files(files, n_files)
        type(file_entry), allocatable, intent(out) :: files(:)
        integer, intent(out) :: n_files
        integer :: iostat, unit_num, status_code
        character(len=1024) :: line
        character(len=512) :: file_path
        character(len=2) :: git_status
        integer :: max_files
        type(file_entry), allocatable :: temp_files(:)

        max_files = 1000
        allocate(temp_files(max_files))
        n_files = 0

        ! Execute git status
        call execute_command_line('git status --porcelain > /tmp/fuss_git_status.txt', exitstat=status_code)

        if (status_code /= 0) then
            write(error_unit, '(A)') 'Error: Not a git repository or git command failed'
            allocate(files(0))
            return
        end if

        ! Read git status output
        open(newunit=unit_num, file='/tmp/fuss_git_status.txt', status='old', action='read', iostat=iostat)

        if (iostat /= 0) then
            allocate(files(0))
            return
        end if

        do
            read(unit_num, '(A)', iostat=iostat) line
            if (iostat /= 0) exit

            if (len_trim(line) > 3) then
                ! Parse git status line (format: "XY filename")
                git_status = line(1:2)
                file_path = adjustl(line(4:))

                ! Skip if path is empty
                if (len_trim(file_path) == 0) cycle

                ! Check if this is a directory entry (ending with /)
                if (len_trim(file_path) > 0) then
                    if (file_path(len_trim(file_path):len_trim(file_path)) == '/') then
                        ! Directory entry - expand it to find all files inside
                        call expand_directory(file_path, git_status, temp_files, n_files, max_files)
                        cycle
                    end if
                end if

                n_files = n_files + 1
                if (n_files > max_files) then
                    max_files = max_files * 2
                    call resize_array(temp_files, max_files)
                end if

                temp_files(n_files)%status = git_status
                temp_files(n_files)%path = trim(file_path)
                ! Column 1 = staged status, Column 2 = unstaged status
                temp_files(n_files)%is_untracked = (git_status == '??')
                temp_files(n_files)%is_staged = (git_status(1:1) /= ' ' .and. git_status(1:1) /= '?')
                temp_files(n_files)%is_unstaged = (git_status(2:2) /= ' ' .and. .not. temp_files(n_files)%is_untracked)
            end if
        end do

        close(unit_num, status='delete')

        ! Copy to output array
        allocate(files(n_files))
        if (n_files > 0) files(1:n_files) = temp_files(1:n_files)
        deallocate(temp_files)
    end subroutine get_dirty_files

    subroutine get_all_files(files, n_files)
        type(file_entry), allocatable, intent(out) :: files(:)
        integer, intent(out) :: n_files
        integer :: iostat, unit_num, status_code, i
        character(len=1024) :: line
        type(file_entry), allocatable :: dirty_files(:), temp_files(:)
        integer :: n_dirty, max_files
        logical :: is_dirty_file

        ! First get dirty files
        call get_dirty_files(dirty_files, n_dirty)

        ! Get all files using find
        call execute_command_line('find . -type f ! -path "*/\.git/*" > /tmp/fuss_all_files.txt', exitstat=status_code)

        if (status_code /= 0) then
            allocate(files(n_dirty))
            if (n_dirty > 0) files = dirty_files
            n_files = n_dirty
            if (allocated(dirty_files)) deallocate(dirty_files)
            return
        end if

        open(newunit=unit_num, file='/tmp/fuss_all_files.txt', status='old', action='read', iostat=iostat)

        if (iostat /= 0) then
            allocate(files(n_dirty))
            if (n_dirty > 0) files = dirty_files
            n_files = n_dirty
            if (allocated(dirty_files)) deallocate(dirty_files)
            return
        end if

        max_files = 1000
        allocate(temp_files(max_files))
        n_files = 0

        do
            read(unit_num, '(A)', iostat=iostat) line
            if (iostat /= 0) exit

            if (len_trim(line) > 0) then
                ! Remove leading "./"
                if (len(line) >= 2) then
                    if (line(1:2) == './') line = line(3:)
                end if

                ! Skip if path is empty after trimming
                if (len_trim(line) == 0) cycle

                n_files = n_files + 1
                if (n_files > max_files) then
                    max_files = max_files * 2
                    call resize_array(temp_files, max_files)
                end if

                ! Check if file is dirty and get status
                is_dirty_file = .false.
                temp_files(n_files)%status = '  '  ! Initialize as clean
                temp_files(n_files)%is_staged = .false.
                temp_files(n_files)%is_unstaged = .false.
                temp_files(n_files)%is_untracked = .false.
                do i = 1, n_dirty
                    if (trim(dirty_files(i)%path) == trim(line)) then
                        is_dirty_file = .true.
                        temp_files(n_files)%status = dirty_files(i)%status
                        temp_files(n_files)%is_staged = dirty_files(i)%is_staged
                        temp_files(n_files)%is_unstaged = dirty_files(i)%is_unstaged
                        temp_files(n_files)%is_untracked = dirty_files(i)%is_untracked
                        exit
                    end if
                end do

                temp_files(n_files)%path = trim(line)
            end if
        end do

        close(unit_num, status='delete')

        allocate(files(n_files))
        if (n_files > 0) files(1:n_files) = temp_files(1:n_files)
        deallocate(temp_files)
        if (allocated(dirty_files)) deallocate(dirty_files)
    end subroutine get_all_files

    subroutine resize_array(array, new_size)
        type(file_entry), allocatable, intent(inout) :: array(:)
        integer, intent(in) :: new_size
        type(file_entry), allocatable :: temp(:)
        integer :: old_size

        old_size = size(array)
        allocate(temp(old_size))
        temp = array
        deallocate(array)
        allocate(array(new_size))
        array(1:old_size) = temp
        deallocate(temp)
    end subroutine resize_array

    subroutine expand_directory(dir_path, git_status, files, n_files, max_files)
        character(len=*), intent(in) :: dir_path, git_status
        type(file_entry), allocatable, intent(inout) :: files(:)
        integer, intent(inout) :: n_files, max_files
        integer :: iostat, unit_num, status_code
        character(len=1024) :: line, command
        character(len=512) :: dir_no_slash

        ! Remove trailing slash
        dir_no_slash = dir_path(1:len_trim(dir_path)-1)

        ! Use find to list all files in this directory
        write(command, '(A,A,A)') 'find "', trim(dir_no_slash), '" -type f > /tmp/fuss_expand_dir.txt'
        call execute_command_line(trim(command), exitstat=status_code)

        if (status_code /= 0) return

        open(newunit=unit_num, file='/tmp/fuss_expand_dir.txt', status='old', action='read', iostat=iostat)
        if (iostat /= 0) return

        do
            read(unit_num, '(A)', iostat=iostat) line
            if (iostat /= 0) exit

            if (len_trim(line) > 0) then
                ! Remove leading "./" if present
                if (len(line) >= 2) then
                    if (line(1:2) == './') line = line(3:)
                end if

                if (len_trim(line) == 0) cycle

                n_files = n_files + 1
                if (n_files > max_files) then
                    max_files = max_files * 2
                    call resize_array(files, max_files)
                end if

                files(n_files)%status = git_status
                files(n_files)%path = trim(line)
                files(n_files)%is_untracked = (git_status == '??')
                files(n_files)%is_staged = (git_status(1:1) /= ' ' .and. git_status(1:1) /= '?')
                files(n_files)%is_unstaged = (git_status(2:2) /= ' ' .and. .not. files(n_files)%is_untracked)
            end if
        end do

        close(unit_num, status='delete')
    end subroutine expand_directory

    subroutine git_add_file(filepath)
        character(len=*), intent(in) :: filepath
        character(len=1024) :: command
        integer :: status

        write(command, '(A,A,A)') 'git add "', trim(filepath), '"'
        call execute_command_line(trim(command), exitstat=status)

        ! Show feedback at bottom of screen
        if (status == 0) then
            print '(A)', 'Staged: ' // trim(filepath)
        else
            print '(A)', 'Failed to stage: ' // trim(filepath)
        end if

        ! Brief pause to show message
        call execute_command_line('sleep 0.5', exitstat=status)
    end subroutine git_add_file

end module git_module
