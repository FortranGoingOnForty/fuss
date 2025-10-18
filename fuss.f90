program fuss
    use iso_fortran_env, only: error_unit
    implicit none

    ! Tree node using linked list structure (first-child, next-sibling)
    type :: tree_node
        character(len=256) :: name
        logical :: is_file
        logical :: is_dirty
        type(tree_node), pointer :: first_child => null()
        type(tree_node), pointer :: next_sibling => null()
    end type tree_node

    type :: file_entry
        character(len=512) :: path
        character(len=2) :: status
        logical :: is_dirty
    end type file_entry

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

                n_files = n_files + 1
                if (n_files > max_files) then
                    max_files = max_files * 2
                    call resize_array(temp_files, max_files)
                end if

                temp_files(n_files)%status = git_status
                temp_files(n_files)%path = trim(file_path)
                temp_files(n_files)%is_dirty = .true.
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
            ! If find fails, just return dirty files
            allocate(files(n_dirty))
            if (n_dirty > 0) files = dirty_files
            n_files = n_dirty
            if (allocated(dirty_files)) deallocate(dirty_files)
            return
        end if

        open(newunit=unit_num, file='/tmp/fuss_all_files.txt', status='old', action='read', iostat=iostat)

        if (iostat /= 0) then
            ! If open fails, just return dirty files
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

                ! Check if file is dirty
                is_dirty_file = .false.
                temp_files(n_files)%status = '  '  ! Initialize as clean
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

    subroutine display_tree(files, n_files)
        type(file_entry), intent(in) :: files(:)
        integer, intent(in) :: n_files
        type(tree_node), pointer :: root
        integer :: i

        ! Create root
        allocate(root)
        root%name = '.'
        root%is_file = .false.
        root%is_dirty = .false.
        root%first_child => null()
        root%next_sibling => null()

        ! Build tree
        do i = 1, n_files
            call add_to_tree(root, files(i)%path, files(i)%is_dirty)
        end do

        ! Sort tree (directories first, then alphabetically)
        call sort_tree(root)

        ! Print tree
        call print_tree_node(root, '', .true., .true.)

        ! Cleanup
        call free_tree(root)
    end subroutine display_tree

    recursive subroutine sort_tree(node)
        type(tree_node), pointer :: node
        type(tree_node), pointer :: child

        if (.not. associated(node)) return

        ! Sort children of this node
        call sort_children(node)

        ! Recursively sort all children
        child => node%first_child
        do while (associated(child))
            call sort_tree(child)
            child => child%next_sibling
        end do
    end subroutine sort_tree

    subroutine sort_children(node)
        type(tree_node), pointer :: node
        type(tree_node), pointer :: sorted_head, sorted_tail
        type(tree_node), pointer :: current, next_node, insert_pos, prev
        logical :: inserted

        if (.not. associated(node%first_child)) return
        if (.not. associated(node%first_child%next_sibling)) return

        ! Build sorted list
        sorted_head => null()
        sorted_tail => null()

        current => node%first_child
        do while (associated(current))
            next_node => current%next_sibling

            ! Insert current into sorted list
            if (.not. associated(sorted_head)) then
                ! First element
                sorted_head => current
                sorted_tail => current
                current%next_sibling => null()
            else
                ! Find insertion point: directories before files, alphabetical
                inserted = .false.
                prev => null()
                insert_pos => sorted_head

                do while (associated(insert_pos))
                    if (should_insert_before(current, insert_pos)) then
                        ! Insert before insert_pos
                        current%next_sibling => insert_pos
                        if (associated(prev)) then
                            prev%next_sibling => current
                        else
                            sorted_head => current
                        end if
                        inserted = .true.
                        exit
                    end if
                    prev => insert_pos
                    insert_pos => insert_pos%next_sibling
                end do

                if (.not. inserted) then
                    ! Insert at end
                    sorted_tail%next_sibling => current
                    sorted_tail => current
                    current%next_sibling => null()
                end if
            end if

            current => next_node
        end do

        node%first_child => sorted_head
    end subroutine sort_children

    function should_insert_before(a, b) result(before)
        type(tree_node), pointer, intent(in) :: a, b
        logical :: before

        ! Pure alphabetical sorting (like tree command)
        before = (trim(a%name) < trim(b%name))
    end function should_insert_before

    recursive subroutine add_to_tree(node, path, is_dirty)
        type(tree_node), pointer, intent(in) :: node
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_dirty

        integer :: slash_pos
        character(len=512) :: first_part, rest
        type(tree_node), pointer :: child, new_child

        ! Find first slash
        slash_pos = index(path, '/')

        if (slash_pos == 0) then
            ! This is a file in current directory - add as child
            child => node%first_child

            ! Check if already exists
            do while (associated(child))
                if (trim(child%name) == trim(path)) then
                    child%is_dirty = child%is_dirty .or. is_dirty
                    return
                end if
                if (.not. associated(child%next_sibling)) exit
                child => child%next_sibling
            end do

            ! Add new child
            allocate(new_child)
            new_child%name = trim(path)
            new_child%is_file = .true.
            new_child%is_dirty = is_dirty
            new_child%first_child => null()
            new_child%next_sibling => null()

            if (.not. associated(node%first_child)) then
                node%first_child => new_child
            else
                child%next_sibling => new_child
            end if
        else
            ! Split path
            first_part = path(1:slash_pos-1)
            rest = path(slash_pos+1:)

            ! Find or create subdirectory
            child => node%first_child
            do while (associated(child))
                if (trim(child%name) == trim(first_part)) then
                    call add_to_tree(child, rest, is_dirty)
                    return
                end if
                if (.not. associated(child%next_sibling)) exit
                child => child%next_sibling
            end do

            ! Create new directory
            allocate(new_child)
            new_child%name = trim(first_part)
            new_child%is_file = .false.
            new_child%is_dirty = .false.
            new_child%first_child => null()
            new_child%next_sibling => null()

            if (.not. associated(node%first_child)) then
                node%first_child => new_child
            else
                child%next_sibling => new_child
            end if

            call add_to_tree(new_child, rest, is_dirty)
        end if
    end subroutine add_to_tree

    recursive subroutine print_tree_node(node, prefix, is_last, is_root)
        type(tree_node), pointer, intent(in) :: node
        character(len=*), intent(in) :: prefix
        logical, intent(in) :: is_last, is_root

        character(len=1024) :: line
        character(len=:), allocatable :: new_prefix
        type(tree_node), pointer :: child
        integer :: n_children, i

        ! UTF-8 box-drawing characters (like tree command)
        character(len=*), parameter :: branch_last = '└──'
        character(len=*), parameter :: branch_mid = '├──'
        character(len=*), parameter :: vertical = '│'
        character(len=*), parameter :: cross_mark = ' ✗'

        ! Count children first
        n_children = 0
        child => node%first_child
        do while (associated(child))
            n_children = n_children + 1
            child => child%next_sibling
        end do

        ! Don't print root node
        if (.not. is_root) then
            ! Build line with appropriate branch character
            if (is_last) then
                line = prefix // branch_last // ' ' // trim(node%name)
            else
                line = prefix // branch_mid // ' ' // trim(node%name)
            end if
            if (node%is_dirty) then
                line = trim(line) // cross_mark
            end if
            print '(A)', trim(line)
        end if

        ! Print children
        i = 0
        child => node%first_child
        do while (associated(child))
            i = i + 1

            if (is_root) then
                new_prefix = ''
            else
                ! Build new prefix with proper indentation
                if (is_last) then
                    new_prefix = prefix // '    '
                else
                    new_prefix = prefix // vertical // '   '
                end if
            end if

            call print_tree_node(child, new_prefix, i == n_children, .false.)
            child => child%next_sibling
        end do
    end subroutine print_tree_node

    recursive subroutine free_tree(node)
        type(tree_node), pointer :: node
        type(tree_node), pointer :: child, next_child

        if (.not. associated(node)) return

        ! Free all children
        child => node%first_child
        do while (associated(child))
            next_child => child%next_sibling
            call free_tree(child)
            child => next_child
        end do

        ! Free this node
        deallocate(node)
        nullify(node)
    end subroutine free_tree

end program fuss
