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

        ! Mark files with incoming changes
        call mark_incoming_changes(files, n_files)

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
        type(tree_node), pointer :: tree_root

        ! Initialize tree pointer
        tree_root => null()

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

        ! Mark files with incoming changes
        call mark_incoming_changes(files, n_files)

        if (n_files == 0) then
            print '(A)', 'No files to display'
            return
        end if

        ! Build flat list of items for navigation
        call build_item_list(files, n_files, items, n_items, tree_root)

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
            call draw_interactive_tree(tree_root, items, n_items, selected, &
                                       repo_name, branch_name, viewport_offset, visible_items)

            ! Read key
            call read_key(key)

            ! Handle input
            select case (key)
            case ('j', 'B')  ! j or down arrow - navigate to next sibling (skip nested items)
                call navigate_down(items, n_items, selected)
            case ('k', 'A')  ! k or up arrow - navigate to previous sibling (skip nested items)
                call navigate_up(items, n_items, selected)
            case ('D')  ! Left arrow - navigate to parent directory
                call navigate_left(items, n_items, selected, tree_root)
            case ('C')  ! Right arrow - enter directory
                call navigate_right(items, n_items, selected, tree_root)
            case (' ')  ! Space bar - toggle expand/collapse
                if (.not. items(selected)%is_file .and. associated(items(selected)%node)) then
                    ! Toggle the expanded state
                    items(selected)%node%is_expanded = .not. items(selected)%node%is_expanded
                    ! Rebuild item list to reflect change
                    call rebuild_item_list_from_tree(tree_root, items, n_items)
                    ! Adjust selection if needed
                    if (selected > n_items .and. n_items > 0) selected = n_items
                end if
            case ('a')  ! Stage file or directory (lowercase to avoid conflict with arrow A)
                ! Check if it's a directory - stage all files in it
                if (.not. items(selected)%is_file) then
                    call git_stage_directory(items(selected)%path)
                    ! Refresh files after staging directory
                    if (show_all) then
                        call get_all_files(files, n_files)
                    else
                        call get_dirty_files(files, n_files)
                    end if
                    call mark_incoming_changes(files, n_files)
                    call build_item_list(files, n_files, items, n_items, tree_root)
                    if (selected > n_items .and. n_items > 0) selected = n_items
                    if (n_items == 0) running = .false.
                ! Otherwise it's a file - stage individual file
                else if (items(selected)%is_file .and. (items(selected)%is_unstaged .or. items(selected)%is_untracked)) then
                    call git_add_file(items(selected)%path)
                    ! Refresh files after git add
                    if (show_all) then
                        call get_all_files(files, n_files)
                    else
                        call get_dirty_files(files, n_files)
                    end if
                    call mark_incoming_changes(files, n_files)
                    call build_item_list(files, n_files, items, n_items, tree_root)
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
                    call mark_incoming_changes(files, n_files)
                    call build_item_list(files, n_files, items, n_items, tree_root)
                    if (selected > n_items .and. n_items > 0) selected = n_items
                end if
            case ('S')  ! Stage all (Shift+S to avoid conflict with up arrow 'A')
                call git_stage_all()
                ! Refresh files after staging all
                if (show_all) then
                    call get_all_files(files, n_files)
                else
                    call get_dirty_files(files, n_files)
                end if
                call mark_incoming_changes(files, n_files)
                call build_item_list(files, n_files, items, n_items, tree_root)
                if (selected > n_items .and. n_items > 0) selected = n_items
                if (n_items == 0) running = .false.
            case ('U')  ! Unstage all (Shift+U)
                call git_unstage_all()
                ! Refresh files after unstaging all
                if (show_all) then
                    call get_all_files(files, n_files)
                else
                    call get_dirty_files(files, n_files)
                end if
                call mark_incoming_changes(files, n_files)
                call build_item_list(files, n_files, items, n_items, tree_root)
                if (selected > n_items .and. n_items > 0) selected = n_items
            case ('m')  ! Commit (lowercase)
                call commit_prompt()
                ! Refresh files after commit
                if (show_all) then
                    call get_all_files(files, n_files)
                else
                    call get_dirty_files(files, n_files)
                end if
                call mark_incoming_changes(files, n_files)
                call build_item_list(files, n_files, items, n_items, tree_root)
                if (selected > n_items .and. n_items > 0) selected = n_items
            case ('M')  ! Amend last commit (Shift+m)
                call amend_commit_prompt()
                ! Refresh files after amend commit
                if (show_all) then
                    call get_all_files(files, n_files)
                else
                    call get_dirty_files(files, n_files)
                end if
                call mark_incoming_changes(files, n_files)
                call build_item_list(files, n_files, items, n_items, tree_root)
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
                call mark_incoming_changes(files, n_files)
                call build_item_list(files, n_files, items, n_items, tree_root)
                if (selected > n_items .and. n_items > 0) selected = n_items
            case ('t')  ! Tag (lowercase)
                call tag_prompt()
            case ('b')  ! Switch branch
                call branch_switch_prompt()
                ! Refresh files after branch switch
                if (show_all) then
                    call get_all_files(files, n_files)
                else
                    call get_dirty_files(files, n_files)
                end if
                call mark_incoming_changes(files, n_files)
                call build_item_list(files, n_files, items, n_items, tree_root)
                if (selected > n_items .and. n_items > 0) selected = n_items
                if (n_items == 0) running = .false.
                ! Update branch name display
                call get_repo_info(repo_name, branch_name)
            case ('n')  ! Create new branch
                call branch_create_prompt()
                ! Refresh files after branch creation
                if (show_all) then
                    call get_all_files(files, n_files)
                else
                    call get_dirty_files(files, n_files)
                end if
                call mark_incoming_changes(files, n_files)
                call build_item_list(files, n_files, items, n_items, tree_root)
                if (selected > n_items .and. n_items > 0) selected = n_items
                if (n_items == 0) running = .false.
                ! Update branch name display
                call get_repo_info(repo_name, branch_name)
            case ('R')  ! Delete branch (Shift+r, since 'r' is used for delete file)
                call branch_delete_prompt()
                ! No need to refresh files or update branch name (stays on current branch)
            case ('f')  ! Git fetch
                call git_fetch()
                ! Refresh files after fetch and include files with incoming changes
                if (show_all) then
                    call get_all_files(files, n_files)
                    call mark_incoming_changes(files, n_files)
                else
                    ! In non-all mode, add files that only have incoming changes
                    call get_dirty_files(files, n_files)
                    call add_incoming_files(files, n_files)
                end if
                call build_item_list(files, n_files, items, n_items, tree_root)
                if (selected > n_items .and. n_items > 0) selected = n_items
            case ('d')  ! Git diff with less
                if (items(selected)%is_file) then
                    call git_diff_file(items(selected)%path, items(selected)%has_incoming)
                end if
            case ('r')  ! Remove/delete file
                if (items(selected)%is_file) then
                    call delete_prompt(items(selected)%path, items(selected)%is_untracked)
                    ! Refresh files after delete
                    if (show_all) then
                        call get_all_files(files, n_files)
                    else
                        call get_dirty_files(files, n_files)
                    end if
                    call mark_incoming_changes(files, n_files)
                    call build_item_list(files, n_files, items, n_items, tree_root)
                    if (selected > n_items .and. n_items > 0) selected = n_items
                    if (n_items == 0) running = .false.
                end if
            case ('x', 'X')  ! Discard changes
                if (items(selected)%is_file .and. (items(selected)%is_staged .or. items(selected)%is_unstaged .or. items(selected)%is_untracked)) then
                    call discard_prompt(items(selected)%path, items(selected)%is_staged, items(selected)%is_untracked)
                    ! Refresh files after discard
                    if (show_all) then
                        call get_all_files(files, n_files)
                    else
                        call get_dirty_files(files, n_files)
                    end if
                    call mark_incoming_changes(files, n_files)
                    call build_item_list(files, n_files, items, n_items, tree_root)
                    if (selected > n_items .and. n_items > 0) selected = n_items
                    if (n_items == 0) running = .false.
                end if
            case ('l')  ! Git pull
                call git_pull()
                ! Refresh files after pull (incoming indicators will automatically clear)
                if (show_all) then
                    call get_all_files(files, n_files)
                    call mark_incoming_changes(files, n_files)
                else
                    call get_dirty_files(files, n_files)
                    call add_incoming_files(files, n_files)
                end if
                call build_item_list(files, n_files, items, n_items, tree_root)
                if (selected > n_items .and. n_items > 0) selected = n_items
                ! Note: After successful pull, git diff will show no upstream differences
                ! so has_incoming will be .false. for all files automatically
            case ('z')  ! Stash push (save changes)
                call stash_push_prompt()
                ! Refresh files after stash
                if (show_all) then
                    call get_all_files(files, n_files)
                else
                    call get_dirty_files(files, n_files)
                end if
                call mark_incoming_changes(files, n_files)
                call build_item_list(files, n_files, items, n_items, tree_root)
                if (selected > n_items .and. n_items > 0) selected = n_items
                if (n_items == 0) running = .false.
            case ('Z')  ! Stash pop/apply (restore changes)
                call stash_pop_apply_prompt()
                ! Refresh files after stash pop/apply
                if (show_all) then
                    call get_all_files(files, n_files)
                else
                    call get_dirty_files(files, n_files)
                end if
                call mark_incoming_changes(files, n_files)
                call build_item_list(files, n_files, items, n_items, tree_root)
                if (selected > n_items .and. n_items > 0) selected = n_items
            case ('q', 'Q')  ! Quit
                running = .false.
            end select
        end do

        ! Restore terminal
        call disable_raw_mode()

        ! Free the tree
        if (associated(tree_root)) then
            call free_tree(tree_root)
        end if

        ! Final display
        call clear_screen()
        call build_and_display_tree('', show_all)
    end subroutine interactive_mode

    subroutine build_item_list(files, n_files, items, n_items, tree_root)
        type(file_entry), intent(in) :: files(:)
        integer, intent(in) :: n_files
        type(selectable_item), allocatable, intent(out) :: items(:)
        integer, intent(out) :: n_items
        type(tree_node), pointer, intent(inout) :: tree_root
        type(selectable_item), allocatable :: temp_items(:)
        integer :: i, max_items
        character(len=512), allocatable :: collapsed_paths(:)
        integer :: n_collapsed, max_collapsed

        ! Save collapsed state from old tree if it exists
        n_collapsed = 0
        max_collapsed = 100
        allocate(collapsed_paths(max_collapsed))
        if (associated(tree_root)) then
            call collect_collapsed_paths(tree_root, '', collapsed_paths, n_collapsed, max_collapsed)
            call free_tree(tree_root)
        end if

        ! Build new tree
        allocate(tree_root)
        tree_root%name = '.'
        tree_root%is_file = .false.
        tree_root%is_staged = .false.
        tree_root%is_unstaged = .false.
        tree_root%is_untracked = .false.
        tree_root%has_incoming = .false.
        tree_root%is_expanded = .true.  ! Root is always expanded
        tree_root%first_child => null()
        tree_root%next_sibling => null()

        do i = 1, n_files
            call add_to_tree(tree_root, files(i)%path, files(i)%is_staged, files(i)%is_unstaged, files(i)%is_untracked, files(i)%has_incoming, files(i)%is_gitignored)
        end do

        call sort_tree(tree_root)

        ! Restore collapsed state to new tree (sort first for binary search optimization)
        if (n_collapsed > 0) then
            call quicksort_collapsed_paths(collapsed_paths, 1, n_collapsed)
            call restore_collapsed_state(tree_root, '', collapsed_paths, n_collapsed)
        end if
        deallocate(collapsed_paths)

        ! Collect items from tree in traversal order
        max_items = 1000
        allocate(temp_items(max_items))
        n_items = 0

        ! Traverse tree and collect all items
        call collect_items_from_tree(tree_root, '', 0, temp_items, n_items, max_items)

        ! Copy to output
        allocate(items(n_items))
        if (n_items > 0) items(1:n_items) = temp_items(1:n_items)
        deallocate(temp_items)

        ! Don't free tree - it's kept alive for expand/collapse operations
    end subroutine build_item_list

    subroutine rebuild_item_list_from_tree(tree_root, items, n_items)
        type(tree_node), pointer, intent(in) :: tree_root
        type(selectable_item), allocatable, intent(out) :: items(:)
        integer, intent(out) :: n_items
        type(selectable_item), allocatable :: temp_items(:)
        integer :: max_items

        ! Collect items from existing tree
        max_items = 1000
        allocate(temp_items(max_items))
        n_items = 0

        ! Traverse tree and collect all items
        call collect_items_from_tree(tree_root, '', 0, temp_items, n_items, max_items)

        ! Copy to output
        allocate(items(n_items))
        if (n_items > 0) items(1:n_items) = temp_items(1:n_items)
        deallocate(temp_items)
    end subroutine rebuild_item_list_from_tree

    recursive subroutine collect_collapsed_paths(node, parent_path, collapsed_paths, n_collapsed, max_collapsed)
        type(tree_node), pointer, intent(in) :: node
        character(len=*), intent(in) :: parent_path
        character(len=512), allocatable, intent(inout) :: collapsed_paths(:)
        integer, intent(inout) :: n_collapsed, max_collapsed
        type(tree_node), pointer :: child
        character(len=512) :: full_path

        ! Build full path for this node
        if (len_trim(parent_path) == 0) then
            full_path = trim(node%name)
        else
            full_path = trim(parent_path) // '/' // trim(node%name)
        end if

        ! If this is a collapsed directory, save its path
        if (.not. node%is_file .and. .not. node%is_expanded) then
            n_collapsed = n_collapsed + 1
            if (n_collapsed > max_collapsed) then
                ! Resize array
                call resize_path_array(collapsed_paths, max_collapsed)
            end if
            collapsed_paths(n_collapsed) = trim(full_path)
        end if

        ! Recursively check children
        child => node%first_child
        do while (associated(child))
            call collect_collapsed_paths(child, full_path, collapsed_paths, n_collapsed, max_collapsed)
            child => child%next_sibling
        end do
    end subroutine collect_collapsed_paths

    subroutine resize_path_array(paths, max_size)
        character(len=512), allocatable, intent(inout) :: paths(:)
        integer, intent(inout) :: max_size
        character(len=512), allocatable :: temp_paths(:)
        integer :: old_size

        old_size = max_size
        allocate(temp_paths(old_size))
        temp_paths = paths(1:old_size)
        deallocate(paths)
        max_size = max_size * 2
        allocate(paths(max_size))
        paths(1:old_size) = temp_paths
        deallocate(temp_paths)
    end subroutine resize_path_array

    ! ========== Performance Optimization: Binary Search for Collapsed Paths ==========
    recursive subroutine quicksort_collapsed_paths(arr, low, high)
        character(len=512), intent(inout) :: arr(:)
        integer, intent(in) :: low, high
        integer :: pivot_idx

        if (low < high) then
            call partition_collapsed_paths(arr, low, high, pivot_idx)
            call quicksort_collapsed_paths(arr, low, pivot_idx - 1)
            call quicksort_collapsed_paths(arr, pivot_idx + 1, high)
        end if
    end subroutine quicksort_collapsed_paths

    subroutine partition_collapsed_paths(arr, low, high, pivot_idx)
        character(len=512), intent(inout) :: arr(:)
        integer, intent(in) :: low, high
        integer, intent(out) :: pivot_idx
        character(len=512) :: pivot, temp
        integer :: i, j

        pivot = trim(arr(high))
        i = low - 1

        do j = low, high - 1
            if (trim(arr(j)) <= pivot) then
                i = i + 1
                temp = arr(i)
                arr(i) = arr(j)
                arr(j) = temp
            end if
        end do

        temp = arr(i + 1)
        arr(i + 1) = arr(high)
        arr(high) = temp

        pivot_idx = i + 1
    end subroutine partition_collapsed_paths

    function binary_search_path(paths, n, target) result(index)
        character(len=512), intent(in) :: paths(:)
        integer, intent(in) :: n
        character(len=*), intent(in) :: target
        integer :: index
        integer :: low, high, mid
        character(len=512) :: target_trimmed, mid_val

        index = -1
        if (n == 0) return

        target_trimmed = trim(target)
        low = 1
        high = n

        do while (low <= high)
            mid = low + (high - low) / 2
            mid_val = trim(paths(mid))

            if (mid_val == target_trimmed) then
                index = mid
                return
            else if (mid_val < target_trimmed) then
                low = mid + 1
            else
                high = mid - 1
            end if
        end do
    end function binary_search_path

    recursive subroutine restore_collapsed_state(node, parent_path, collapsed_paths, n_collapsed)
        type(tree_node), pointer, intent(inout) :: node
        character(len=*), intent(in) :: parent_path
        character(len=512), intent(in) :: collapsed_paths(:)
        integer, intent(in) :: n_collapsed
        type(tree_node), pointer :: child
        character(len=512) :: full_path
        integer :: idx

        ! Build full path for this node
        if (len_trim(parent_path) == 0) then
            full_path = trim(node%name)
        else
            full_path = trim(parent_path) // '/' // trim(node%name)
        end if

        ! Check if this directory should be collapsed using binary search (O(log n) vs O(n))
        if (.not. node%is_file) then
            idx = binary_search_path(collapsed_paths, n_collapsed, full_path)
            if (idx > 0) then
                node%is_expanded = .false.
            end if
        end if

        ! Recursively restore for children
        child => node%first_child
        do while (associated(child))
            call restore_collapsed_state(child, full_path, collapsed_paths, n_collapsed)
            child => child%next_sibling
        end do
    end subroutine restore_collapsed_state

    recursive subroutine collect_items_from_tree(node, parent_path, depth, items, n_items, max_items)
        type(tree_node), pointer, intent(in) :: node
        character(len=*), intent(in) :: parent_path
        integer, intent(in) :: depth
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
            items(n_items)%has_incoming = node%has_incoming
            items(n_items)%is_gitignored = node%is_gitignored
            items(n_items)%depth = depth  ! Track nesting depth
            items(n_items)%node => node  ! Store pointer to tree node
        else
            full_path = ''
        end if

        ! Recursively add children only if this node is expanded (or if it's root)
        if (node%is_expanded) then
            child => node%first_child
            do while (associated(child))
                call collect_items_from_tree(child, full_path, depth + 1, items, n_items, max_items)
                child => child%next_sibling
            end do
        end if
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

    ! ========== Navigation Functions for New Navigation Model ==========

    subroutine navigate_down(items, n_items, selected)
        type(selectable_item), intent(in) :: items(:)
        integer, intent(in) :: n_items
        integer, intent(inout) :: selected
        integer :: current_depth, i

        if (n_items == 0) return

        current_depth = items(selected)%depth

        ! Search forward for next item at same depth
        do i = selected + 1, n_items
            if (items(i)%depth == current_depth) then
                selected = i
                return
            end if
        end do

        ! No item found - wrap to beginning
        do i = 1, selected - 1
            if (items(i)%depth == current_depth) then
                selected = i
                return
            end if
        end do
        ! If we get here, we're the only item at this depth, so stay put
    end subroutine navigate_down

    subroutine navigate_up(items, n_items, selected)
        type(selectable_item), intent(in) :: items(:)
        integer, intent(in) :: n_items
        integer, intent(inout) :: selected
        integer :: current_depth, i

        if (n_items == 0) return

        current_depth = items(selected)%depth

        ! Search backward for previous item at same depth
        do i = selected - 1, 1, -1
            if (items(i)%depth == current_depth) then
                selected = i
                return
            end if
        end do

        ! No item found - wrap to end
        do i = n_items, selected + 1, -1
            if (items(i)%depth == current_depth) then
                selected = i
                return
            end if
        end do
        ! If we get here, we're the only item at this depth, so stay put
    end subroutine navigate_up

    subroutine navigate_right(items, n_items, selected, tree_root)
        type(selectable_item), allocatable, intent(inout) :: items(:)
        integer, intent(inout) :: n_items
        integer, intent(inout) :: selected
        type(tree_node), pointer, intent(in) :: tree_root
        integer :: i, target_depth

        if (n_items == 0) return
        if (items(selected)%is_file) return  ! Can't enter a file

        ! We're on a directory
        if (.not. items(selected)%node%is_expanded) then
            ! Directory is collapsed - expand it
            items(selected)%node%is_expanded = .true.
            ! Rebuild item list
            call rebuild_item_list_from_tree(tree_root, items, n_items)
            ! Adjust selection if needed
            if (selected > n_items .and. n_items > 0) selected = n_items
        end if

        ! Now move to first child (next item with depth+1)
        target_depth = items(selected)%depth + 1
        do i = selected + 1, n_items
            if (items(i)%depth == target_depth) then
                selected = i
                return
            end if
        end do
        ! No children - stay on directory
    end subroutine navigate_right

    subroutine navigate_left(items, n_items, selected, tree_root)
        type(selectable_item), allocatable, intent(inout) :: items(:)
        integer, intent(inout) :: n_items
        integer, intent(inout) :: selected
        type(tree_node), pointer, intent(in) :: tree_root
        integer :: i, target_depth

        if (n_items == 0) return

        ! Move to parent (previous item with depth-1)
        target_depth = items(selected)%depth - 1
        if (target_depth < 0) return  ! Already at root level

        ! Search backward for parent
        do i = selected - 1, 1, -1
            if (items(i)%depth == target_depth) then
                selected = i
                return
            end if
        end do
    end subroutine navigate_left

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

    subroutine amend_commit_prompt()
        character(len=512) :: commit_msg, last_commit_msg
        logical :: success
        character(len=1) :: key

        ! Clear screen for amend commit prompt
        call clear_screen()
        print '(A)', achar(27) // '[1mGit Commit --amend' // achar(27) // '[0m'
        print '(A)', ''

        ! Get the last commit message as default
        call get_last_commit_message(last_commit_msg)

        ! Show the last commit message
        if (len_trim(last_commit_msg) > 0) then
            print '(A)', achar(27) // '[2mLast commit message:' // achar(27) // '[0m'
            print '(A)', '  ' // trim(last_commit_msg)
            print '(A)', ''
        end if

        ! Read new commit message
        call read_line('New commit message (empty to keep): ', commit_msg)

        ! If no message provided, keep the old one
        if (len_trim(commit_msg) == 0) then
            commit_msg = last_commit_msg
        end if

        ! Execute amend if we have a message
        if (len_trim(commit_msg) > 0) then
            call git_commit_amend(commit_msg, success)

            ! Wait for keypress to continue
            call read_key(key)
        else
            print '(A)', 'No commit message provided. Amend cancelled.'
            print '(A)', 'Press any key to continue...'
            call read_key(key)
        end if
    end subroutine amend_commit_prompt

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

    subroutine tag_prompt()
        character(len=512) :: tag_name, tag_message
        logical :: success, push_tag
        character(len=1) :: key
        integer :: status

        ! Clear screen for tag prompt
        call clear_screen()
        print '(A)', achar(27) // '[1mGit Tag' // achar(27) // '[0m'
        print '(A)', ''

        ! Fetch tags from remote to ensure list is up to date
        print '(A)', 'Fetching tags from remote...'
        call execute_command_line('git fetch --tags --quiet 2>&1', exitstat=status)
        print '(A)', ''

        ! Show existing tags in compact format
        print '(A)', achar(27) // '[2mExisting tags:' // achar(27) // '[0m'
        call execute_command_line('git tag --sort=-version:refname | head -10 | column -c 80 2>/dev/null || git tag --sort=-version:refname | head -10', exitstat=status)
        print '(A)', ''

        ! Read tag name
        call read_line('Tag name: ', tag_name)

        ! Execute tag if name is not empty
        if (len_trim(tag_name) > 0) then
            ! Read tag message (optional)
            call read_line('Tag message (enter for none): ', tag_message)

            call git_tag(tag_name, tag_message, success)

            if (success) then
                ! Ask if user wants to push the tag
                print '(A)', ''
                print '(A)', 'Push tag to origin? (y/n)'
                call read_key(key)

                if (key == 'y' .or. key == 'Y') then
                    call git_push_tag(tag_name, push_tag)
                end if
            end if

            ! Wait for keypress to continue
            print '(A)', 'Press any key to continue...'
            call read_key(key)
        end if
    end subroutine tag_prompt

    subroutine branch_switch_prompt()
        logical :: success

        ! Clear screen for branch switch
        call clear_screen()
        print '(A)', achar(27) // '[1mSwitch Branch' // achar(27) // '[0m'
        print '(A)', ''

        ! Call git branch switch with fzf
        call git_switch_branch(success)
    end subroutine branch_switch_prompt

    subroutine delete_prompt(filepath, is_untracked)
        character(len=*), intent(in) :: filepath
        logical, intent(in) :: is_untracked
        logical :: deleted
        character(len=1) :: key

        ! Clear screen for delete prompt
        call clear_screen()
        print '(A)', achar(27) // '[1mDelete File' // achar(27) // '[0m'
        print '(A)', ''

        ! Execute delete with confirmation
        call git_delete_file(filepath, is_untracked, deleted)

        ! Wait for keypress to continue
        call read_key(key)
    end subroutine delete_prompt

    subroutine discard_prompt(filepath, is_staged, is_untracked)
        character(len=*), intent(in) :: filepath
        logical, intent(in) :: is_staged, is_untracked
        logical :: discarded
        character(len=1) :: key

        ! Clear screen for discard prompt
        call clear_screen()
        print '(A)', achar(27) // '[1mDiscard Changes' // achar(27) // '[0m'
        print '(A)', ''

        ! Execute discard with confirmation
        call git_discard_changes(filepath, is_staged, is_untracked, discarded)

        ! Wait for keypress to continue
        call read_key(key)
    end subroutine discard_prompt

    subroutine stash_push_prompt()
        character(len=512) :: stash_msg
        logical :: success
        character(len=1) :: key

        ! Clear screen for stash prompt
        call clear_screen()
        print '(A)', achar(27) // '[1mGit Stash (Save)' // achar(27) // '[0m'
        print '(A)', ''

        ! Read stash message (optional)
        call read_line('Stash message (optional): ', stash_msg)

        ! Execute stash push
        call git_stash_push(stash_msg, success)

        ! Wait for keypress to continue
        call read_key(key)
    end subroutine stash_push_prompt

    subroutine stash_pop_apply_prompt()
        logical :: success
        character(len=1) :: key

        ! Clear screen for stash pop/apply prompt
        call clear_screen()
        print '(A)', achar(27) // '[1mGit Stash (Pop/Apply)' // achar(27) // '[0m'
        print '(A)', ''

        ! Execute stash pop/apply with fzf selection
        call git_stash_pop_apply(success)

        ! Wait for keypress to continue
        call read_key(key)
    end subroutine stash_pop_apply_prompt

    subroutine branch_create_prompt()
        character(len=512) :: branch_name
        logical :: success
        character(len=1) :: key

        ! Clear screen for branch creation
        call clear_screen()
        print '(A)', achar(27) // '[1mCreate New Branch' // achar(27) // '[0m'
        print '(A)', ''

        ! Read branch name
        call read_line('New branch name: ', branch_name)

        ! Create branch if name is not empty
        if (len_trim(branch_name) > 0) then
            call git_create_branch(branch_name, success)
            ! Wait for keypress to continue
            call read_key(key)
        end if
    end subroutine branch_create_prompt

    subroutine branch_delete_prompt()
        logical :: success
        character(len=1) :: key

        ! Clear screen for branch deletion
        call clear_screen()
        print '(A)', achar(27) // '[1mDelete Branch' // achar(27) // '[0m'
        print '(A)', ''

        ! Call git branch delete with fzf
        call git_delete_branch(success)

        ! Wait for keypress to continue
        call read_key(key)
    end subroutine branch_delete_prompt

end program fuss
