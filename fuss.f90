program fuss
    use iso_fortran_env, only: error_unit
    implicit none

    ! Type definitions at program level
    type :: file_entry
        character(len=512) :: path
        character(len=2) :: status
        logical :: is_dirty
    end type file_entry

    type :: tree_node
        character(len=256) :: name
        logical :: is_file
        logical :: is_dirty
        type(tree_node), allocatable :: children(:)
        integer :: n_children
    end type tree_node

    ! Main program variables
    logical :: show_all
    character(len=:), allocatable :: root_path

    ! Parse command line arguments
    call parse_arguments(show_all)

    ! Get current directory
    call get_current_dir(root_path)

    ! Build and display tree
    call build_and_display_tree(root_path, show_all)

contains

    subroutine parse_arguments(show_all)
        logical, intent(out) :: show_all
        integer :: i, nargs
        character(len=256) :: arg

        show_all = .false.
        nargs = command_argument_count()

        do i = 1, nargs
            call get_command_argument(i, arg)
            if (trim(arg) == '--all') then
                show_all = .true.
            end if
        end do
    end subroutine parse_arguments

    subroutine get_current_dir(path)
        character(len=:), allocatable, intent(out) :: path
        character(len=1024) :: buffer
        integer :: status

        call execute_command_line('pwd', exitstat=status)
        call execute_command_line('pwd > /tmp/fuss_pwd.txt', exitstat=status)

        open(unit=99, file='/tmp/fuss_pwd.txt', status='old', action='read')
        read(99, '(A)') buffer
        close(99, status='delete')

        path = trim(buffer)
    end subroutine get_current_dir

    subroutine build_and_display_tree(root_path, show_all)
        character(len=*), intent(in) :: root_path
        logical, intent(in) :: show_all
        type(file_entry), allocatable :: files(:)
        integer :: n_files

        ! Get files from git or filesystem
        if (show_all) then
            call get_all_files(files, n_files)
        else
            call get_dirty_files(files, n_files)
        end if

        ! Display the tree
        if (n_files > 0) then
            print '(A)', '.'
            call display_tree(files, n_files)
        else
            print '(A)', 'No files to display'
        end if
    end subroutine build_and_display_tree

    subroutine get_dirty_files(files, n_files)
        type(file_entry), allocatable, intent(out) :: files(:)
        integer, intent(out) :: n_files
        integer :: iostat, unit_num, status_code
        character(len=1024) :: line
        character(len=512) :: file_path
        character(len=2) :: git_status
        integer :: i, max_files
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

            if (len_trim(line) > 0) then
                n_files = n_files + 1
                if (n_files > max_files) then
                    max_files = max_files * 2
                    call resize_array(temp_files, max_files)
                end if

                ! Parse git status line (format: "XY filename")
                git_status = line(1:2)
                file_path = adjustl(line(4:))

                temp_files(n_files)%status = git_status
                temp_files(n_files)%path = trim(file_path)
                temp_files(n_files)%is_dirty = .true.
            end if
        end do

        close(unit_num, status='delete')

        ! Copy to output array
        allocate(files(n_files))
        files(1:n_files) = temp_files(1:n_files)
        deallocate(temp_files)
    end subroutine get_dirty_files

    subroutine get_all_files(files, n_files)
        type(file_entry), allocatable, intent(out) :: files(:)
        integer, intent(out) :: n_files
        integer :: iostat, unit_num, status_code, i, j
        character(len=1024) :: line
        type(file_entry), allocatable :: dirty_files(:), temp_files(:)
        integer :: n_dirty, max_files
        logical :: is_dirty_file

        ! First get dirty files
        call get_dirty_files(dirty_files, n_dirty)

        max_files = 1000
        allocate(temp_files(max_files))
        n_files = 0

        ! Get all files using find
        call execute_command_line('find . -type f ! -path "*/\.git/*" > /tmp/fuss_all_files.txt', exitstat=status_code)

        if (status_code /= 0) then
            files = dirty_files
            n_files = n_dirty
            return
        end if

        open(newunit=unit_num, file='/tmp/fuss_all_files.txt', status='old', action='read', iostat=iostat)

        if (iostat /= 0) then
            files = dirty_files
            n_files = n_dirty
            return
        end if

        do
            read(unit_num, '(A)', iostat=iostat) line
            if (iostat /= 0) exit

            if (len_trim(line) > 0) then
                ! Remove leading "./"
                if (line(1:2) == './') line = line(3:)

                n_files = n_files + 1
                if (n_files > max_files) then
                    max_files = max_files * 2
                    call resize_array(temp_files, max_files)
                end if

                ! Check if file is dirty
                is_dirty_file = .false.
                do i = 1, n_dirty
                    if (trim(dirty_files(i)%path) == trim(line)) then
                        is_dirty_file = .true.
                        temp_files(n_files)%status = dirty_files(i)%status
                        exit
                    end if
                end do

                temp_files(n_files)%path = trim(line)
                temp_files(n_files)%is_dirty = is_dirty_file
            end if
        end do

        close(unit_num, status='delete')

        allocate(files(n_files))
        files(1:n_files) = temp_files(1:n_files)
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

    subroutine display_tree(files, n_files)
        type(file_entry), intent(in) :: files(:)
        integer, intent(in) :: n_files
        type(tree_node) :: root
        integer :: i

        ! Initialize root
        root%name = '.'
        root%is_file = .false.
        root%is_dirty = .false.
        root%n_children = 0
        allocate(root%children(0))

        ! Build tree
        do i = 1, n_files
            call add_to_tree(root, files(i)%path, files(i)%is_dirty)
        end do

        ! Print tree
        call print_tree_node(root, '', .true., .true.)
    end subroutine display_tree

    recursive subroutine add_to_tree(node, path, is_dirty)
        type(tree_node), intent(inout) :: node
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_dirty

        integer :: slash_pos
        character(len=512) :: first_part, rest
        integer :: i
        logical :: found
        type(tree_node), allocatable :: temp_children(:)

        ! Find first slash
        slash_pos = index(path, '/')

        if (slash_pos == 0) then
            ! This is a file in current directory
            found = .false.
            do i = 1, node%n_children
                if (trim(node%children(i)%name) == trim(path)) then
                    found = .true.
                    node%children(i)%is_dirty = node%children(i)%is_dirty .or. is_dirty
                    exit
                end if
            end do

            if (.not. found) then
                ! Add new file
                allocate(temp_children(node%n_children + 1))
                if (node%n_children > 0) then
                    temp_children(1:node%n_children) = node%children
                end if
                temp_children(node%n_children + 1)%name = trim(path)
                temp_children(node%n_children + 1)%is_file = .true.
                temp_children(node%n_children + 1)%is_dirty = is_dirty
                temp_children(node%n_children + 1)%n_children = 0
                allocate(temp_children(node%n_children + 1)%children(0))

                call move_alloc(temp_children, node%children)
                node%n_children = node%n_children + 1
            end if
        else
            ! Split path
            first_part = path(1:slash_pos-1)
            rest = path(slash_pos+1:)

            ! Find or create subdirectory
            found = .false.
            do i = 1, node%n_children
                if (trim(node%children(i)%name) == trim(first_part)) then
                    found = .true.
                    call add_to_tree(node%children(i), rest, is_dirty)
                    exit
                end if
            end do

            if (.not. found) then
                ! Add new directory
                allocate(temp_children(node%n_children + 1))
                if (node%n_children > 0) then
                    temp_children(1:node%n_children) = node%children
                end if
                temp_children(node%n_children + 1)%name = trim(first_part)
                temp_children(node%n_children + 1)%is_file = .false.
                temp_children(node%n_children + 1)%is_dirty = .false.
                temp_children(node%n_children + 1)%n_children = 0
                allocate(temp_children(node%n_children + 1)%children(0))

                call move_alloc(temp_children, node%children)
                node%n_children = node%n_children + 1

                call add_to_tree(node%children(node%n_children), rest, is_dirty)
            end if
        end if
    end subroutine add_to_tree

    recursive subroutine print_tree_node(node, prefix, is_last, is_root)
        type(tree_node), intent(in) :: node
        character(len=*), intent(in) :: prefix
        logical, intent(in) :: is_last, is_root

        character(len=1024) :: line, new_prefix
        integer :: i
        character(len=10) :: branch_char, extension_char, cross_mark, vertical_char

        ! Unicode box drawing characters using char() with selected_char_kind
        ! We'll use simple ASCII fallback since gfortran has issues with achar > 127

        ! ASCII tree characters
        if (is_last) then
            branch_char = '└──'
        else
            branch_char = '├──'
        end if
        vertical_char = '│'
        cross_mark = '✗'

        ! Don't print root node
        if (.not. is_root) then
            line = prefix // trim(branch_char) // ' ' // trim(node%name)
            if (node%is_dirty) then
                line = trim(line) // ' ' // trim(cross_mark)
            end if
            print '(A)', trim(line)
        end if

        ! Print children
        do i = 1, node%n_children
            if (is_root) then
                new_prefix = ''
            else
                if (is_last) then
                    new_prefix = prefix // '    '
                else
                    new_prefix = prefix // trim(vertical_char) // '   '
                end if
            end if

            call print_tree_node(node%children(i), new_prefix, i == node%n_children, .false.)
        end do
    end subroutine print_tree_node

end program fuss
