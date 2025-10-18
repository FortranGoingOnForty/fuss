program fuss
    use iso_fortran_env, only: error_unit
    implicit none

    ! Tree node using linked list structure (first-child, next-sibling)
    type :: tree_node
        character(len=256) :: name
        logical :: is_file
        logical :: is_staged
        logical :: is_unstaged
        type(tree_node), pointer :: first_child => null()
        type(tree_node), pointer :: next_sibling => null()
    end type tree_node

    type :: file_entry
        character(len=512) :: path
        character(len=2) :: status
        logical :: is_staged
        logical :: is_unstaged
    end type file_entry

    type :: selectable_item
        character(len=512) :: path
        logical :: is_staged
        logical :: is_unstaged
        logical :: is_file
    end type selectable_item

    ! Main program variables
    logical :: show_all, interactive
    character(len=:), allocatable :: root_path

    ! Parse command line arguments
    call parse_arguments(show_all, interactive)

    ! Get current directory
    call get_current_dir(root_path)

    ! Build and display tree
    if (interactive) then
        call interactive_mode(show_all)
    else
        call build_and_display_tree(root_path, show_all)
    end if

contains

    subroutine parse_arguments(show_all, interactive)
        logical, intent(out) :: show_all, interactive
        integer :: i, nargs
        character(len=256) :: arg

        show_all = .false.
        interactive = .false.
        nargs = command_argument_count()

        do i = 1, nargs
            call get_command_argument(i, arg)
            if (trim(arg) == '--all' .or. trim(arg) == '-a') then
                show_all = .true.
            else if (trim(arg) == '-i' .or. trim(arg) == '--interactive') then
                interactive = .true.
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
                temp_files(n_files)%is_staged = (git_status(1:1) /= ' ' .and. git_status(1:1) /= '?')
                temp_files(n_files)%is_unstaged = (git_status(2:2) /= ' ')
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

                ! Check if file is dirty and get status
                is_dirty_file = .false.
                temp_files(n_files)%status = '  '  ! Initialize as clean
                temp_files(n_files)%is_staged = .false.
                temp_files(n_files)%is_unstaged = .false.
                do i = 1, n_dirty
                    if (trim(dirty_files(i)%path) == trim(line)) then
                        is_dirty_file = .true.
                        temp_files(n_files)%status = dirty_files(i)%status
                        temp_files(n_files)%is_staged = dirty_files(i)%is_staged
                        temp_files(n_files)%is_unstaged = dirty_files(i)%is_unstaged
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
                files(n_files)%is_staged = (git_status(1:1) /= ' ' .and. git_status(1:1) /= '?')
                files(n_files)%is_unstaged = (git_status(2:2) /= ' ')
            end if
        end do

        close(unit_num, status='delete')
    end subroutine expand_directory

    subroutine display_tree(files, n_files)
        type(file_entry), intent(in) :: files(:)
        integer, intent(in) :: n_files
        type(tree_node), pointer :: root
        integer :: i

        ! Create root
        allocate(root)
        root%name = '.'
        root%is_file = .false.
        root%is_staged = .false.
        root%is_unstaged = .false.
        root%first_child => null()
        root%next_sibling => null()

        ! Build tree
        do i = 1, n_files
            call add_to_tree(root, files(i)%path, files(i)%is_staged, files(i)%is_unstaged)
        end do

        ! Sort tree (directories first, then alphabetically)
        call sort_tree(root)

        ! Print tree
        call print_tree_node(root, '', .true., .true.)

        ! Cleanup
        call free_tree(root)
    end subroutine display_tree

    subroutine interactive_mode(show_all)
        logical, intent(in) :: show_all
        type(file_entry), allocatable :: files(:)
        type(selectable_item), allocatable :: items(:)
        integer :: n_files, n_items, selected, i, status
        character(len=1) :: key
        logical :: running

        ! Get files
        if (show_all) then
            call get_all_files(files, n_files)
        else
            call get_dirty_files(files, n_files)
        end if

        if (n_files == 0) then
            print '(A)', 'No files to display'
            return
        end if

        ! Build flat list of items for navigation
        call build_item_list(files, n_files, items, n_items)

        ! Initialize selection
        selected = 1
        running = .true.

        ! Enable raw terminal mode
        call enable_raw_mode()

        ! Main interactive loop
        do while (running)
            ! Clear screen and redraw
            call clear_screen()
            call draw_interactive_tree(files, n_files, items, n_items, selected)

            ! Read key
            call read_key(key)

            ! Handle input
            select case (key)
            case ('j', 'B')  ! j or down arrow
                if (selected < n_items) selected = selected + 1
            case ('k', 'A')  ! k or up arrow
                if (selected > 1) selected = selected - 1
            case (achar(10), achar(13), ' ')  ! Enter or Space
                if (items(selected)%is_file .and. items(selected)%is_unstaged) then
                    call git_add_file(items(selected)%path)
                    ! Refresh files after git add
                    if (show_all) then
                        call get_all_files(files, n_files)
                    else
                        call get_dirty_files(files, n_files)
                    end if
                    call build_item_list(files, n_files, items, n_items)
                    if (selected > n_items .and. n_items > 0) selected = n_items
                    if (n_items == 0) running = .false.
                end if
            case ('q', 'Q')  ! Quit
                running = .false.
            end select
        end do

        ! Restore terminal
        call disable_raw_mode()

        ! Final display
        call clear_screen()
        call build_and_display_tree('', show_all)
    end subroutine interactive_mode

    subroutine build_item_list(files, n_files, items, n_items)
        type(file_entry), intent(in) :: files(:)
        integer, intent(in) :: n_files
        type(selectable_item), allocatable, intent(out) :: items(:)
        integer, intent(out) :: n_items
        type(tree_node), pointer :: root
        type(selectable_item), allocatable :: temp_items(:)
        integer :: i, max_items

        ! Build the tree first
        allocate(root)
        root%name = '.'
        root%is_file = .false.
        root%is_staged = .false.
        root%is_unstaged = .false.
        root%first_child => null()
        root%next_sibling => null()

        do i = 1, n_files
            call add_to_tree(root, files(i)%path, files(i)%is_staged, files(i)%is_unstaged)
        end do

        call sort_tree(root)

        ! Collect items from tree in traversal order
        max_items = 1000
        allocate(temp_items(max_items))
        n_items = 0

        ! Traverse tree and collect all items (files and directories)
        call collect_items_from_tree(root, '', temp_items, n_items, max_items)

        ! Copy to output
        allocate(items(n_items))
        if (n_items > 0) items(1:n_items) = temp_items(1:n_items)
        deallocate(temp_items)

        call free_tree(root)
    end subroutine build_item_list

    recursive subroutine collect_items_from_tree(node, parent_path, items, n_items, max_items)
        type(tree_node), pointer, intent(in) :: node
        character(len=*), intent(in) :: parent_path
        type(selectable_item), allocatable, intent(inout) :: items(:)
        integer, intent(inout) :: n_items, max_items
        type(tree_node), pointer :: child
        character(len=512) :: full_path

        ! Skip root node
        if (len_trim(parent_path) > 0 .or. trim(node%name) /= '.') then
            ! Build full path
            if (len_trim(parent_path) == 0) then
                full_path = trim(node%name)
            else
                full_path = trim(parent_path) // '/' // trim(node%name)
            end if

            ! Add this item
            n_items = n_items + 1
            if (n_items > max_items) then
                ! Resize array
                call resize_item_array(items, max_items)
            end if

            items(n_items)%path = trim(full_path)
            items(n_items)%is_file = node%is_file
            items(n_items)%is_staged = node%is_staged
            items(n_items)%is_unstaged = node%is_unstaged
        else
            full_path = ''
        end if

        ! Recursively add children
        child => node%first_child
        do while (associated(child))
            call collect_items_from_tree(child, full_path, items, n_items, max_items)
            child => child%next_sibling
        end do
    end subroutine collect_items_from_tree

    subroutine resize_item_array(items, max_items)
        type(selectable_item), allocatable, intent(inout) :: items(:)
        integer, intent(inout) :: max_items
        type(selectable_item), allocatable :: temp_items(:)
        integer :: old_size

        old_size = max_items
        allocate(temp_items(old_size))
        temp_items = items(1:old_size)
        deallocate(items)
        max_items = max_items * 2
        allocate(items(max_items))
        items(1:old_size) = temp_items
        deallocate(temp_items)
    end subroutine resize_item_array

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

    subroutine draw_interactive_tree(files, n_files, items, n_items, selected)
        type(file_entry), intent(in) :: files(:)
        integer, intent(in) :: n_files, n_items, selected
        type(selectable_item), intent(in) :: items(:)
        type(tree_node), pointer :: root
        integer :: i, item_idx

        ! Build tree
        allocate(root)
        root%name = '.'
        root%is_file = .false.
        root%is_staged = .false.
        root%is_unstaged = .false.
        root%first_child => null()
        root%next_sibling => null()

        do i = 1, n_files
            call add_to_tree(root, files(i)%path, files(i)%is_staged, files(i)%is_unstaged)
        end do

        call sort_tree(root)

        ! Print tree with selection highlighting
        item_idx = 0
        print '(A)', '.'
        call print_interactive_node(root, '', .true., .true., items, &
                                   selected, item_idx)

        ! Print help
        print '(A)', ''
        print '(A)', achar(27) // '[32m↑' // achar(27) // '[0m=staged ' // &
                     achar(27) // '[31m✗' // achar(27) // '[0m=unstaged | ' // &
                     'j/↓: down | k/↑: up | Space: git add | q: quit'

        call free_tree(root)
    end subroutine draw_interactive_tree

    recursive subroutine print_interactive_node(node, prefix, is_last, &
                                               is_root, items, selected, item_idx)
        type(tree_node), pointer, intent(in) :: node
        character(len=*), intent(in) :: prefix
        logical, intent(in) :: is_last, is_root
        type(selectable_item), intent(in) :: items(:)
        integer, intent(in) :: selected
        integer, intent(inout) :: item_idx

        character(len=1024) :: line
        character(len=:), allocatable :: new_prefix
        type(tree_node), pointer :: child
        integer :: n_children, i
        logical :: is_selected

        character(len=*), parameter :: branch_last = '└──'
        character(len=*), parameter :: branch_mid = '├──'
        character(len=*), parameter :: vertical = '│'
        character(len=*), parameter :: highlight_on = achar(27) // '[7m'
        character(len=*), parameter :: highlight_off = achar(27) // '[0m'
        character(len=1), parameter :: ESC = achar(27)

        ! Build colored marks as character arrays
        character(len=50) :: mark_unstaged
        character(len=50) :: mark_staged

        ! Initialize colored marks with explicit ESC characters
        write(mark_unstaged, '(A,A,A,A,A)') ESC, '[31m', ' ✗', ESC, '[0m'
        write(mark_staged, '(A,A,A,A,A)') ESC, '[32m', ' ↑', ESC, '[0m'

        ! Count children first
        n_children = 0
        child => node%first_child
        do while (associated(child))
            n_children = n_children + 1
            child => child%next_sibling
        end do

        ! Don't print root node
        if (.not. is_root) then
            ! Increment item index for all nodes (files and directories)
            item_idx = item_idx + 1
            is_selected = (item_idx == selected)

            ! Build line with appropriate branch character (exactly like print_tree_node)
            if (is_last) then
                line = prefix // branch_last // ' '
            else
                line = prefix // branch_mid // ' '
            end if

            ! Add name with highlighting if selected
            if (is_selected) then
                line = trim(line) // highlight_on // trim(node%name)
                ! Show both indicators if file has both staged and unstaged changes
                if (node%is_staged) then
                    line = trim(line) // trim(mark_staged)
                end if
                if (node%is_unstaged) then
                    line = trim(line) // trim(mark_unstaged)
                end if
                line = trim(line) // highlight_off
            else
                line = trim(line) // trim(node%name)
                ! Show both indicators if file has both staged and unstaged changes
                if (node%is_staged) then
                    line = trim(line) // trim(mark_staged)
                end if
                if (node%is_unstaged) then
                    line = trim(line) // trim(mark_unstaged)
                end if
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
                if (is_last) then
                    new_prefix = prefix // '    '
                else
                    new_prefix = prefix // vertical // '   '
                end if
            end if

            call print_interactive_node(child, new_prefix, i == n_children, &
                                       .false., items, selected, item_idx)
            child => child%next_sibling
        end do
    end subroutine print_interactive_node

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

    recursive subroutine add_to_tree(node, path, is_staged, is_unstaged)
        type(tree_node), pointer, intent(in) :: node
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_staged, is_unstaged

        integer :: slash_pos, iostat
        character(len=512) :: first_part, rest
        type(tree_node), pointer :: child, new_child
        logical :: is_directory

        ! Find first slash
        slash_pos = index(path, '/')

        if (slash_pos == 0) then
            ! This could be a file or a directory in current directory
            child => node%first_child

            ! Check if already exists
            do while (associated(child))
                if (trim(child%name) == trim(path)) then
                    child%is_staged = child%is_staged .or. is_staged
                    child%is_unstaged = child%is_unstaged .or. is_unstaged
                    return
                end if
                if (.not. associated(child%next_sibling)) exit
                child => child%next_sibling
            end do

            ! Check if this is a directory (simple check if path exists as dir)
            inquire(file=trim(path), exist=is_directory, iostat=iostat)
            if (iostat /= 0) is_directory = .false.

            ! If the name matches common directory patterns or actually is a dir, treat as directory
            ! Otherwise treat as file
            if (is_directory) then
                call execute_command_line('test -d "' // trim(path) // '"', exitstat=iostat)
                is_directory = (iostat == 0)
            else
                is_directory = .false.
            end if

            ! Add new child
            allocate(new_child)
            new_child%name = trim(path)
            new_child%is_file = .not. is_directory
            new_child%is_staged = is_staged
            new_child%is_unstaged = is_unstaged
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
                    call add_to_tree(child, rest, is_staged, is_unstaged)
                    return
                end if
                if (.not. associated(child%next_sibling)) exit
                child => child%next_sibling
            end do

            ! Create new directory
            allocate(new_child)
            new_child%name = trim(first_part)
            new_child%is_file = .false.
            new_child%is_staged = .false.
            new_child%is_unstaged = .false.
            new_child%first_child => null()
            new_child%next_sibling => null()

            if (.not. associated(node%first_child)) then
                node%first_child => new_child
            else
                child%next_sibling => new_child
            end if

            call add_to_tree(new_child, rest, is_staged, is_unstaged)
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
        character(len=1), parameter :: ESC = achar(27)

        ! Build colored marks as character arrays
        character(len=50) :: mark_unstaged
        character(len=50) :: mark_staged

        ! Initialize colored marks with explicit ESC characters
        write(mark_unstaged, '(A,A,A,A,A)') ESC, '[31m', ' ✗', ESC, '[0m'
        write(mark_staged, '(A,A,A,A,A)') ESC, '[32m', ' ↑', ESC, '[0m'

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
            ! Show both indicators if file has both staged and unstaged changes
            if (node%is_staged) then
                line = trim(line) // trim(mark_staged)
            end if
            if (node%is_unstaged) then
                line = trim(line) // trim(mark_unstaged)
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
