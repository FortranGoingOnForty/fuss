program fuss
    use iso_fortran_env, only: error_unit
    use types_module
    use git_module
    use tree_module
    use display_module
    use terminal_module
    implicit none

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

    subroutine interactive_mode(show_all)
        logical, intent(in) :: show_all
        type(file_entry), allocatable :: files(:)
        type(selectable_item), allocatable :: items(:)
        integer :: n_files, n_items, selected, i, status
        character(len=1) :: key
        logical :: running
        character(len=256) :: repo_name, branch_name
        integer :: term_height, viewport_offset, visible_items

        ! Get repo and branch info
        call get_repo_info(repo_name, branch_name)

        ! Get terminal height
        call get_terminal_height(term_height)

        ! DEBUG: Show terminal height
        ! print '(A,I0)', 'DEBUG: Terminal height detected: ', term_height

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

        ! Calculate visible items accurately
        ! Fixed UI elements that take screen space:
        !   Line 1: repo:branch (e.g., "fuss:trunk")
        !   Line 2: blank line after repo
        !   Line 3: "." root
        !   Lines 4 to N-3: tree items (VIEWPORT)
        !   Line N-2: blank line before help
        !   Line N-1: help legend (↑=staged ✗=modified ✗=untracked)
        !   Line N: help controls (j/k/↓/↑: navigate | ...)
        ! Total fixed: 6 lines (2 + 1 + 3)
        visible_items = term_height - 6
        if (visible_items < 3) visible_items = 3  ! Absolute minimum
        if (visible_items > n_items) visible_items = n_items  ! Don't exceed total items

        ! Initialize selection and viewport at TOP of tree
        selected = 1
        viewport_offset = 1
        running = .true.

        ! Enable raw terminal mode
        call enable_raw_mode()

        ! Main interactive loop
        do while (running)
            ! Center viewport on selection - keeps highlighted item in middle of screen
            viewport_offset = selected - visible_items / 2

            ! Clamp viewport to valid range
            if (viewport_offset < 1) viewport_offset = 1
            if (viewport_offset > n_items - visible_items + 1 .and. n_items > visible_items) then
                viewport_offset = n_items - visible_items + 1
            end if

            ! Clear screen and redraw
            call clear_screen()
            call draw_interactive_tree(files, n_files, items, n_items, selected, &
                                       repo_name, branch_name, viewport_offset, visible_items)

            ! Read key
            call read_key(key)

            ! Handle input
            select case (key)
            case ('j', 'B')  ! j or down arrow
                if (selected < n_items) selected = selected + 1
            case ('k', 'A')  ! k or up arrow
                if (selected > 1) selected = selected - 1
            case ('a')  ! Stage file (lowercase to avoid conflict with arrow A)
                if (items(selected)%is_file .and. (items(selected)%is_unstaged .or. items(selected)%is_untracked)) then
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
            case ('u')  ! Unstage file (lowercase)
                if (items(selected)%is_file .and. items(selected)%is_staged) then
                    call git_unstage_file(items(selected)%path)
                    ! Refresh files after git unstage
                    if (show_all) then
                        call get_all_files(files, n_files)
                    else
                        call get_dirty_files(files, n_files)
                    end if
                    call build_item_list(files, n_files, items, n_items)
                    if (selected > n_items .and. n_items > 0) selected = n_items
                end if
            case ('m')  ! Commit (lowercase)
                call commit_prompt()
                ! Refresh files after commit
                if (show_all) then
                    call get_all_files(files, n_files)
                else
                    call get_dirty_files(files, n_files)
                end if
                call build_item_list(files, n_files, items, n_items)
                if (selected > n_items .and. n_items > 0) selected = n_items
            case ('s')  ! Show git status (lowercase)
                call show_status_view()
            case ('p')  ! Push (lowercase)
                call push_prompt()
                ! Refresh files after push
                if (show_all) then
                    call get_all_files(files, n_files)
                else
                    call get_dirty_files(files, n_files)
                end if
                call build_item_list(files, n_files, items, n_items)
                if (selected > n_items .and. n_items > 0) selected = n_items
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
        root%is_untracked = .false.
        root%first_child => null()
        root%next_sibling => null()

        do i = 1, n_files
            call add_to_tree(root, files(i)%path, files(i)%is_staged, files(i)%is_unstaged, files(i)%is_untracked)
        end do

        call sort_tree(root)

        ! Collect items from tree in traversal order
        max_items = 1000
        allocate(temp_items(max_items))
        n_items = 0

        ! Traverse tree and collect all items
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
                call resize_item_array(items, max_items)
            end if

            items(n_items)%path = trim(full_path)
            items(n_items)%is_file = node%is_file
            items(n_items)%is_staged = node%is_staged
            items(n_items)%is_unstaged = node%is_unstaged
            items(n_items)%is_untracked = node%is_untracked
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

    subroutine commit_prompt()
        character(len=512) :: commit_msg
        logical :: success
        character(len=1) :: key

        ! Clear screen for commit prompt
        call clear_screen()
        print '(A)', achar(27) // '[1mGit Commit' // achar(27) // '[0m'
        print '(A)', ''

        ! Read commit message
        call read_line('Commit message: ', commit_msg)

        ! Execute commit if message is not empty
        if (len_trim(commit_msg) > 0) then
            call git_commit_with_message(commit_msg, success)

            ! Wait for keypress to continue
            call read_key(key)
        end if
    end subroutine commit_prompt

    subroutine show_status_view()
        ! Use less for scrollable, searchable git status view
        call show_git_status_paged()
    end subroutine show_status_view

    subroutine push_prompt()
        logical :: success
        character(len=1) :: key

        ! Clear screen for push prompt
        call clear_screen()
        print '(A)', achar(27) // '[1mGit Push' // achar(27) // '[0m'
        print '(A)', ''
        print '(A)', 'Pushing to remote...'
        print '(A)', ''

        ! Execute push
        call git_push(success)

        ! Wait for keypress to continue
        call read_key(key)
    end subroutine push_prompt

end program fuss
