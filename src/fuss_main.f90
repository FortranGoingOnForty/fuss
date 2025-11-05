program fuss
    use iso_fortran_env, only: error_unit
    use types_module
    use git_module
    use tree_module
    use display_module
    use terminal_module
    use cache_module
    implicit none

    ! Main program variables
    logical :: show_all, interactive
    character(len=:), allocatable :: root_path

    ! Initialize caches for performance optimization
    call init_caches()

    ! Parse command line arguments
    call parse_arguments(show_all, interactive)

    ! Get current directory
    call get_current_dir(root_path)

    ! Build and display tree
    if (interactive) then
        call interactive_mode(show_all)
    else
        call build_and_display_tree(show_all)
    end if

    ! Ensure terminal is always restored (safety cleanup)
    call cleanup_terminal()

contains

    subroutine parse_arguments(show_all, interactive)
        logical, intent(out) :: show_all, interactive
        integer :: i, nargs
        character(len=256) :: arg
        logical :: print_only

        show_all = .false.
        interactive = .true.  ! Interactive is now the default
        print_only = .false.
        nargs = command_argument_count()

        do i = 1, nargs
            call get_command_argument(i, arg)
            if (trim(arg) == '--help' .or. trim(arg) == '-h') then
                call print_help()
                stop
            else if (trim(arg) == '--version' .or. trim(arg) == '-v') then
                call print_version()
                stop
            else if (trim(arg) == '--all' .or. trim(arg) == '-a') then
                show_all = .true.
            else if (trim(arg) == '-i' .or. trim(arg) == '--interactive') then
                interactive = .true.
            else if (trim(arg) == '-p' .or. trim(arg) == '--print') then
                print_only = .true.
            else
                print '(A)', 'Error: Unknown option: ' // trim(arg)
                print '(A)', 'Run ''fuss --help'' for usage information'
                stop 1
            end if
        end do

        ! If print_only is set, disable interactive mode
        if (print_only) then
            interactive = .false.
        end if
    end subroutine parse_arguments

    subroutine print_version()
        print '(A)', 'fuss v1.0.0'
        print '(A)', ''
        print '(A)', 'A git staging tool. Written in Fortran, for some reason.'
        print '(A)', 'https://github.com/FortranGoingOnForty/fuss'
    end subroutine print_version

    subroutine print_help()
        print '(A)', 'fuss - git staging with a tree view'
        print '(A)', ''
        print '(A)', 'USAGE:'
        print '(A)', '  fuss [OPTIONS]'
        print '(A)', ''
        print '(A)', 'OPTIONS:'
        print '(A)', '  -h, --help       Show this'
        print '(A)', '  -v, --version    Show version'
        print '(A)', '  -p, --print      Print tree and exit (non-interactive)'
        print '(A)', '  -a, --all        Show all files, not just dirty'
        print '(A)', ''
        print '(A)', 'KEYS:'
        print '(A)', '  j/k, ↑/↓         Navigate up/down'
        print '(A)', '  ←/→              Parent/child directory'
        print '(A)', '  space            Toggle directory'
        print '(A)', '  a/u              Stage/unstage file'
        print '(A)', '  S/U              Stage/unstage all'
        print '(A)', '  m                Commit'
        print '(A)', '  M                Amend commit'
        print '(A)', '  p/l/f            Push/pull/fetch'
        print '(A)', '  d                Diff'
        print '(A)', '  c                View file'
        print '(A)', '  w                Blame'
        print '(A)', '  h                History'
        print '(A)', '  L                Reflog'
        print '(A)', '  y                Cherry-pick'
        print '(A)', '  v                Revert commit'
        print '(A)', '  x                Discard changes'
        print '(A)', '  b                Switch branch'
        print '(A)', '  n                New branch'
        print '(A)', '  R                Delete branch'
        print '(A)', '  G                Merge branch'
        print '(A)', '  O                Reset'
        print '(A)', '  I                Interactive rebase'
        print '(A)', '  z/Z              Stash/pop'
        print '(A)', '  t                Tag'
        print '(A)', '  r                Delete file'
        print '(A)', '  s                Status'
        print '(A)', '  .                Toggle dotfiles'
        print '(A)', '  q                Quit'
        print '(A)', ''
    end subroutine print_help

    subroutine get_current_dir(path)
        character(len=:), allocatable, intent(out) :: path
        character(len=1024) :: buffer
        integer :: status

        call execute_command_line('pwd > /tmp/fuss_tmp.txt', exitstat=status)

        open(unit=99, file='/tmp/fuss_tmp.txt', status='old', action='read')
        read(99, '(A)') buffer
        close(99, status='delete')

        path = trim(buffer)
    end subroutine get_current_dir

    subroutine build_and_display_tree(show_all)
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

    subroutine cleanup_terminal()
        ! Emergency cleanup - restores terminal to normal state
        ! Call this before any exit or when calling external programs
        call disable_raw_mode()
        call exit_alternate_screen()
    end subroutine cleanup_terminal

    subroutine interactive_mode(show_all)
        logical, intent(in) :: show_all
        type(file_entry), allocatable :: files(:)
        type(selectable_item), allocatable :: items(:)
        integer :: n_files, n_items, selected
        character(len=1) :: key
        logical :: running, hide_dotfiles
        character(len=256) :: repo_name, branch_name, term_program
        integer :: term_height, viewport_offset, visible_items, top_padding
        integer :: prev_selected, prev_viewport
        logical :: needs_full_redraw
        character(len=10) :: mode  ! "normal" or "git" mode
        ! Search state for fuzzy jump
        character(len=32) :: search_buffer
        integer :: search_length
        integer(8) :: last_search_tick, current_tick, clock_rate
        type(tree_node), pointer :: tree_root

        ! Initialize tree pointer
        tree_root => null()

        ! Detect terminal type for padding (fixes WezTerm/Ghostty/iTerm top line cutoff)
        ! Alternate screen buffer needs more padding to prevent top cutoff
        call get_environment_variable("TERM_PROGRAM", term_program)
        if (index(term_program, "iTerm") > 0) then
            top_padding = 4  ! iTerm2 needs 4 lines in alternate screen
        else if (index(term_program, "WezTerm") > 0 .or. index(term_program, "ghostty") > 0) then
            top_padding = 3  ! WezTerm/Ghostty need 3 lines in alternate screen
        else if (index(term_program, "Apple_Terminal") > 0) then
            top_padding = 3  ! Terminal.app needs 3 lines
        else
            top_padding = 2  ! Other terminals need 2 lines
        end if

        ! Get repo and branch info
        call get_repo_info(repo_name, branch_name)

        ! Get terminal height
        call get_terminal_height(term_height)

        ! DEBUG: Show terminal height
        ! print '(A,I0)', 'DEBUG: Terminal height detected: ', term_height

        ! Initialize hide_dotfiles before first use
        hide_dotfiles = .false.

        ! Get files and mark incoming changes
        if (show_all) then
            call get_all_files(files, n_files)
        else
            call get_dirty_files(files, n_files)
        end if
        call mark_incoming_changes(files, n_files)

        if (n_files == 0) then
            print '(A)', 'No files to display'
            return
        end if

        ! Build flat list of items for navigation
        call build_item_list(files, n_files, items, n_items, tree_root, hide_dotfiles)

        ! Calculate visible items accurately
        ! Fixed UI elements that take screen space:
        !   top_padding lines: blank padding for terminal compatibility (2-3 lines)
        !   1 line: repo:branch (e.g., "fuss:trunk")
        !   1 line: blank line after repo
        !   1 line: "." root
        !   Lines X to N-3: tree items (VIEWPORT)
        !   1 line: blank line before help
        !   1 line: help legend (↑=staged ✗=modified ✗=untracked)
        !   1 line: help controls (j/k/↓/↑: navigate | ...)
        ! Total fixed: top_padding + 6 lines
        visible_items = term_height - top_padding - 6
        if (visible_items < 3) visible_items = 3  ! Absolute minimum
        if (visible_items > n_items) visible_items = n_items  ! Don't exceed total items

        ! Initialize selection and viewport at TOP of tree
        selected = 1
        viewport_offset = 1
        running = .true.
        mode = 'normal'  ! Start in normal mode

        ! Initialize search state
        search_buffer = ''
        search_length = 0
        last_search_tick = 0
        call system_clock(count_rate=clock_rate)

        ! Partial redraw optimization: initialize tracking state
        prev_selected = 0  ! Force initial draw
        prev_viewport = 0
        needs_full_redraw = .true.

        ! Enter alternate screen buffer (preserves terminal content)
        call enter_alternate_screen()

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

            ! Conditional redraw for performance optimization
            if (needs_full_redraw .or. viewport_offset /= prev_viewport) then
                ! Full redraw needed: viewport scrolled or forced refresh
                call clear_screen()
                call draw_interactive_tree(tree_root, items, n_items, selected, &
                                           repo_name, branch_name, viewport_offset, visible_items, top_padding, mode)
                needs_full_redraw = .false.
            else if (selected /= prev_selected) then
                ! Only selection changed within same viewport - still need full redraw for now
                ! TODO: Could optimize this with partial line updates in the future
                call clear_screen()
                call draw_interactive_tree(tree_root, items, n_items, selected, &
                                           repo_name, branch_name, viewport_offset, visible_items, top_padding, mode)
            end if

            ! Update tracking state
            prev_selected = selected
            prev_viewport = viewport_offset

            ! Check search timeout (0.5 seconds)
            if (search_length > 0) then
                call system_clock(current_tick)
                ! Check if 0.5 seconds has elapsed (clock_rate/2 ticks)
                if (current_tick - last_search_tick > clock_rate / 2) then
                    search_length = 0
                    search_buffer = ''
                    needs_full_redraw = .true.
                end if
            end if

            ! Always use fast blocking read - timeouts are too slow
            call read_key(key)

            ! DEBUG: Log all control characters to see what we're getting
            if (ichar(key) < 32) then
                open(99, file='/tmp/fuss_debug.log', position='append')
                write(99, '(A,I0)') 'Control char received: ', ichar(key)
                close(99)
            end if

            ! Check for ctrl-c to quit (priority over everything)
            if (key == achar(3)) then
                open(99, file='/tmp/fuss_debug.log', position='append')
                write(99, '(A)') 'CTRL-C detected - quitting!'
                close(99)
                running = .false.
                cycle
            end if

            ! Check for alt-g to toggle git mode
            ! alt-g is encoded as achar(1 + ichar('g') - ichar('a')) = achar(7)
            if (key == achar(7)) then
                ! Toggle between normal and git mode
                if (mode == 'normal') then
                    mode = 'git'
                else
                    mode = 'normal'
                end if
                ! Temporarily restore terminal to flush output properly
                call execute_command_line('stty sane < /dev/tty')
                call clear_screen()
                call draw_interactive_tree(tree_root, items, n_items, selected, &
                                           repo_name, branch_name, viewport_offset, visible_items, top_padding, mode)
                ! Restore cbreak mode
                call enable_raw_mode()
                cycle  ! Skip rest of key handling
            end if

            ! Check for alt-s to show git status (available in both modes)
            ! alt-s is encoded as achar(1 + ichar('s') - ichar('a')) = achar(19)
            if (key == achar(19)) then
                call show_status_view()
                needs_full_redraw = .true.
                cycle
            end if

            ! Check for alt-v to view file (available in both modes)
            ! alt-v is encoded as achar(1 + ichar('v') - ichar('a')) = achar(22)
            if (key == achar(22)) then
                if (items(selected)%is_file) then
                    call view_file(items(selected)%path)
                    needs_full_redraw = .true.
                end if
                cycle
            end if

            ! Handle ESC key - exit git mode or clear search
            if (key == achar(27)) then
                if (mode == 'git') then
                    mode = 'normal'
                    ! Temporarily restore terminal to flush output properly
                    call execute_command_line('stty sane < /dev/tty')
                    call clear_screen()
                    call draw_interactive_tree(tree_root, items, n_items, selected, &
                                               repo_name, branch_name, viewport_offset, visible_items, top_padding, mode)
                    ! Restore cbreak mode
                    call enable_raw_mode()
                    cycle
                else if (search_length > 0) then
                    ! Clear search in normal mode
                    search_length = 0
                    search_buffer = ''
                    needs_full_redraw = .true.
                    cycle
                end if
                ! In normal mode, ESC does nothing for now
                cycle
            end if

            ! Fuzzy search in normal mode - handle any printable character
            ! Exclude A, B, C, D since those are arrow key codes after escape sequence processing
            if (mode == 'normal') then
                if ((key >= 'a' .and. key <= 'z') .or. &
                    ((key >= 'E' .and. key <= 'Z') .or. (key >= '0' .and. key <= '9')) .or. &
                    key == '_' .or. key == '-' .or. key == '.') then

                    ! Check if timeout elapsed since last keypress - if so, start fresh search
                    if (search_length > 0) then
                        call system_clock(current_tick)
                        if (current_tick - last_search_tick > clock_rate / 2) then
                            ! Timeout elapsed (0.5 seconds) - clear buffer and start new search
                            search_length = 0
                            search_buffer = ''
                            ! DEBUG
                            open(99, file='/tmp/fuss_debug.log', position='append')
                            write(99, '(A)') 'TIMEOUT: Starting fresh search (0.5s elapsed)'
                            close(99)
                        end if
                    end if

                    ! Add to search buffer
                    if (search_length < 32) then
                        search_length = search_length + 1
                        search_buffer(search_length:search_length) = key
                        call system_clock(last_search_tick)

                        ! DEBUG
                        open(99, file='/tmp/fuss_debug.log', position='append')
                        write(99, '(A,A,A,I0)') 'Buffer: "', search_buffer(1:search_length), '" -> jumping to match'
                        close(99)

                        call fuzzy_jump_to_match(items, n_items, search_buffer(1:search_length), selected)

                        needs_full_redraw = .true.
                    end if
                    cycle  ! Skip case statement
                else if (key == achar(127) .or. key == achar(8)) then
                    ! Backspace - remove last character
                    if (search_length > 0) then
                        search_length = search_length - 1
                        call system_clock(last_search_tick)
                        if (search_length > 0) then
                            call fuzzy_jump_to_match(items, n_items, search_buffer(1:search_length), selected)
                        end if
                        needs_full_redraw = .true.
                    end if
                    cycle  ! Skip case statement
                end if
            end if

            ! Handle input
            select case (key)
            case ('j', 'B')  ! j or down arrow - navigate to next sibling (skip nested items)
                ! Clear search buffer on navigation
                if (search_length > 0) then
                    search_length = 0
                    search_buffer = ''
                end if
                call navigate_down(items, n_items, selected)
            case ('k', 'A')  ! k or up arrow - navigate to previous sibling (skip nested items)
                ! Clear search buffer on navigation
                if (search_length > 0) then
                    search_length = 0
                    search_buffer = ''
                end if
                call navigate_up(items, n_items, selected)
            case ('D')  ! Left arrow - navigate to parent directory
                ! Clear search buffer on navigation
                if (search_length > 0) then
                    search_length = 0
                    search_buffer = ''
                end if
                call navigate_left(items, n_items, selected)
            case ('C')  ! Right arrow - enter directory
                ! Clear search buffer on navigation
                if (search_length > 0) then
                    search_length = 0
                    search_buffer = ''
                end if
                call navigate_right(items, n_items, selected, tree_root, hide_dotfiles)
            case (' ')  ! Space bar - toggle expand/collapse
                ! Clear search buffer on navigation
                if (search_length > 0) then
                    search_length = 0
                    search_buffer = ''
                end if
                if (.not. items(selected)%is_file .and. associated(items(selected)%node)) then
                    ! Toggle the expanded state
                    items(selected)%node%is_expanded = .not. items(selected)%node%is_expanded
                    ! Rebuild item list to reflect change
                    call rebuild_item_list_from_tree(tree_root, items, n_items, hide_dotfiles)
                    ! Adjust selection if needed
                    if (selected > n_items .and. n_items > 0) selected = n_items
                    ! Force full redraw after tree structure change
                    needs_full_redraw = .true.
                end if
            ! Git operations - only available in git mode
            case ('a')  ! Stage file or directory (lowercase to avoid conflict with arrow A)
                if (mode == 'git') then
                    ! Check if it's a directory - stage all files in it
                    if (.not. items(selected)%is_file) then
                        call git_stage_directory(items(selected)%path)
                        ! Refresh files after staging directory
                        call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                                hide_dotfiles, selected, running, exit_if_empty=.true., force_refresh=.true.)
                        needs_full_redraw = .true.
                    ! Otherwise it's a file - stage individual file
                    else if (items(selected)%is_file .and. (items(selected)%is_unstaged .or. items(selected)%is_untracked)) then
                        call git_add_file(items(selected)%path)
                        ! Refresh files after git add
                        call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                                hide_dotfiles, selected, running, exit_if_empty=.true., force_refresh=.true.)
                        needs_full_redraw = .true.
                    end if
                end if
            case ('u')  ! Unstage file (lowercase)
                if (mode == 'git' .and. items(selected)%is_file .and. items(selected)%is_staged) then
                    call git_unstage_file(items(selected)%path)
                    ! Refresh files after git unstage
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, force_refresh=.true.)
                    needs_full_redraw = .true.
                end if
            case ('S')  ! Stage all (Shift+S to avoid conflict with up arrow 'A')
                if (mode == 'git') then
                    call git_stage_all()
                    ! Refresh files after staging all
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, exit_if_empty=.true., force_refresh=.true.)
                        needs_full_redraw = .true.
                end if
            case ('U')  ! Unstage all (Shift+U)
                if (mode == 'git') then
                    call git_unstage_all()
                    ! Refresh files after unstaging all
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, force_refresh=.true.)
                        needs_full_redraw = .true.
                end if
            case ('m')  ! Commit (lowercase)
                if (mode == 'git') then
                    call commit_prompt()
                    ! Refresh files after commit
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, force_refresh=.true.)
                        needs_full_redraw = .true.
                end if
            case ('M')  ! Amend last commit (Shift+m)
                if (mode == 'git') then
                    call amend_commit_prompt()
                    ! Refresh files after amend commit
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, force_refresh=.true.)
                        needs_full_redraw = .true.
                end if
            case ('p')  ! Push (lowercase)
                if (mode == 'git') then
                    call push_prompt()
                    ! Refresh files after push
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, force_refresh=.true.)
                        needs_full_redraw = .true.
                end if
            case ('t')  ! Tag (lowercase)
                if (mode == 'git') then
                    call tag_prompt()
                    needs_full_redraw = .true.
                end if
            case ('b')  ! Switch branch
                if (mode == 'git') then
                    call branch_switch_prompt()
                    ! Refresh files after branch switch
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, exit_if_empty=.true., force_refresh=.true.)
                        needs_full_redraw = .true.
                    ! Update branch name display
                    call get_repo_info(repo_name, branch_name)
                end if
            case ('n')  ! Create new branch
                if (mode == 'git') then
                    call branch_create_prompt()
                    ! Refresh files after branch creation
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, exit_if_empty=.true., force_refresh=.true.)
                        needs_full_redraw = .true.
                    ! Update branch name display
                    call get_repo_info(repo_name, branch_name)
                end if
            case ('R')  ! Delete branch (Shift+r, since 'r' is used for delete file)
                if (mode == 'git') then
                    call branch_delete_prompt()
                    needs_full_redraw = .true.
                    ! No need to refresh files or update branch name (stays on current branch)
                end if
            case ('f')  ! Git fetch
                if (mode == 'git') then
                    call git_fetch()
                    ! Refresh files after fetch and include files with incoming changes
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, include_incoming=.true., force_refresh=.true.)
                        needs_full_redraw = .true.
                end if
            case ('d')  ! Git diff with less
                if (mode == 'git' .and. items(selected)%is_file) then
                    call git_diff_file(items(selected)%path, items(selected)%has_incoming)
                    needs_full_redraw = .true.
                end if
            case ('c')  ! View file contents (git mode shortcut; use alt-v in normal mode)
                if (mode == 'git' .and. items(selected)%is_file) then
                    call view_file(items(selected)%path)
                    needs_full_redraw = .true.
                end if
            case ('s')  ! Show git status (git mode shortcut; use alt-s in normal mode)
                if (mode == 'git') then
                    call show_status_view()
                    needs_full_redraw = .true.
                end if
            case ('w')  ! Git blame (who changed this line)
                if (mode == 'git' .and. items(selected)%is_file) then
                    call blame_prompt(items(selected)%path)
                    needs_full_redraw = .true.
                end if
            case ('r')  ! Remove/delete file
                if (mode == 'git' .and. items(selected)%is_file) then
                    call delete_prompt(items(selected)%path, items(selected)%is_untracked)
                    ! Refresh files after delete
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, exit_if_empty=.true., force_refresh=.true.)
                    needs_full_redraw = .true.
                end if
            case ('x', 'X')  ! Discard changes
                if (mode == 'git' .and. items(selected)%is_file .and. (items(selected)%is_staged .or. items(selected)%is_unstaged .or. items(selected)%is_untracked)) then
                    call discard_prompt(items(selected)%path, items(selected)%is_staged, items(selected)%is_untracked)
                    ! Refresh files after discard
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, exit_if_empty=.true., force_refresh=.true.)
                    needs_full_redraw = .true.
                end if
            case ('l')  ! Git pull
                if (mode == 'git') then
                    call git_pull()
                    ! Refresh files after pull (incoming indicators will automatically clear)
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, include_incoming=.true., force_refresh=.true.)
                        needs_full_redraw = .true.
                    ! Note: After successful pull, git diff will show no upstream differences
                    ! so has_incoming will be .false. for all files automatically
                end if
            case ('z')  ! Stash push (save changes)
                if (mode == 'git') then
                    call stash_push_prompt()
                    ! Refresh files after stash
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, exit_if_empty=.true., force_refresh=.true.)
                        needs_full_redraw = .true.
                end if
            case ('Z')  ! Stash pop/apply (restore changes)
                if (mode == 'git') then
                    call stash_pop_apply_prompt()
                    ! Refresh files after stash pop/apply
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, force_refresh=.true.)
                        needs_full_redraw = .true.
                end if
            case ('y')  ! Cherry-pick (yank commit)
                if (mode == 'git') then
                    call cherry_pick_prompt()
                    ! Refresh files after cherry-pick
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, force_refresh=.true.)
                        needs_full_redraw = .true.
                end if
            case ('v')  ! Revert commit
                if (mode == 'git') then
                    call revert_commit_prompt()
                    ! Refresh files after revert
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, force_refresh=.true.)
                        needs_full_redraw = .true.
                end if
            case ('h')  ! Show commit history
                if (mode == 'git') then
                    call history_browser_prompt()
                    needs_full_redraw = .true.
                end if
            case ('L')  ! Show reflog (Shift+l)
                if (mode == 'git') then
                    call reflog_browser_prompt()
                    needs_full_redraw = .true.
                end if
            case ('G')  ! Merge branch (Shift+g)
                if (mode == 'git') then
                    call merge_branch_prompt()
                    ! Refresh files after merge
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, force_refresh=.true.)
                        needs_full_redraw = .true.
                    ! Update branch name display in case we merged
                    call get_repo_info(repo_name, branch_name)
                end if
            case ('O')  ! Reset (Shift+o - "Oh no, undo!")
                if (mode == 'git') then
                    call reset_prompt()
                    ! Refresh files after reset
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, force_refresh=.true.)
                        needs_full_redraw = .true.
                end if
            case ('I')  ! Interactive rebase (Shift+i)
                if (mode == 'git') then
                    call rebase_prompt()
                    ! Refresh files after rebase
                    call refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                            hide_dotfiles, selected, running, force_refresh=.true.)
                        needs_full_redraw = .true.
                end if
            case ('.')  ! Toggle hiding dotfiles and gitignored files
                hide_dotfiles = .not. hide_dotfiles
                ! Rebuild item list with new filter
                call build_item_list(files, n_files, items, n_items, tree_root, hide_dotfiles)
                ! Adjust selection and visible_items for new item count
                if (selected > n_items .and. n_items > 0) selected = n_items
                if (n_items > 0 .and. selected < 1) selected = 1
                ! Force full redraw after filter change
                needs_full_redraw = .true.
                ! Recalculate visible_items in case n_items changed
                visible_items = term_height - top_padding - 6
                if (visible_items < 3) visible_items = 3
                if (visible_items > n_items) visible_items = n_items
            case ('q', 'Q')  ! Exit git mode
                if (mode == 'git') then
                    ! In git mode: q exits to normal mode
                    mode = 'normal'
                    needs_full_redraw = .true.
                end if
                ! Note: In normal mode, 'q' is used for fuzzy search
                ! Use ctrl-c to quit from normal mode
            case default
                ! Unhandled keys - do nothing
                continue
            end select
        end do

        ! Restore terminal to normal state
        call cleanup_terminal()

        ! Free the tree
        if (associated(tree_root)) then
            call free_tree(tree_root)
        end if

        ! Final display (now in normal terminal buffer)
        call clear_screen()
        call build_and_display_tree(show_all)
    end subroutine interactive_mode

    subroutine build_item_list(files, n_files, items, n_items, tree_root, hide_dotfiles)
        type(file_entry), intent(in) :: files(:)
        integer, intent(in) :: n_files
        type(selectable_item), allocatable, intent(out) :: items(:)
        integer, intent(out) :: n_items
        type(tree_node), pointer, intent(inout) :: tree_root
        logical, intent(in) :: hide_dotfiles
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
            ! Skip gitignored files and dotfiles if hide_dotfiles is enabled
            if (hide_dotfiles) then
                ! Check if this is a gitignored file
                if (files(i)%is_gitignored) then
                    cycle  ! Skip this file
                end if
                ! Check if this is a dotfile (path starts with . or contains /.)
                if (index(files(i)%path, '/.') > 0 .or. files(i)%path(1:1) == '.') then
                    cycle  ! Skip this file
                end if
            end if
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
        call collect_items_from_tree(tree_root, '', 0, temp_items, n_items, max_items, hide_dotfiles)

        ! Copy to output
        allocate(items(n_items))
        if (n_items > 0) items(1:n_items) = temp_items(1:n_items)
        deallocate(temp_items)

        ! Don't free tree - it's kept alive for expand/collapse operations
    end subroutine build_item_list

    subroutine rebuild_item_list_from_tree(tree_root, items, n_items, hide_dotfiles)
        type(tree_node), pointer, intent(in) :: tree_root
        type(selectable_item), allocatable, intent(out) :: items(:)
        integer, intent(out) :: n_items
        logical, intent(in) :: hide_dotfiles
        type(selectable_item), allocatable :: temp_items(:)
        integer :: max_items

        ! Collect items from existing tree
        max_items = 1000
        allocate(temp_items(max_items))
        n_items = 0

        ! Traverse tree and collect all items
        call collect_items_from_tree(tree_root, '', 0, temp_items, n_items, max_items, hide_dotfiles)

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

    recursive subroutine collect_items_from_tree(node, parent_path, depth, items, n_items, max_items, hide_dotfiles)
        type(tree_node), pointer, intent(in) :: node
        character(len=*), intent(in) :: parent_path
        integer, intent(in) :: depth
        type(selectable_item), allocatable, intent(inout) :: items(:)
        integer, intent(inout) :: n_items, max_items
        logical, intent(in) :: hide_dotfiles
        type(tree_node), pointer :: child
        character(len=512) :: full_path
        logical :: is_root

        ! Check if this is the root node
        is_root = (len_trim(parent_path) == 0 .and. trim(node%name) == '.')

        ! Build full path
        if (is_root) then
            full_path = ''
        else if (len_trim(parent_path) == 0) then
            full_path = trim(node%name)
        else
            full_path = trim(parent_path) // '/' // trim(node%name)
        end if

        ! Add this item to the list (unless it's root)
        if (.not. is_root) then
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
            items(n_items)%depth = depth
            items(n_items)%node => node
        end if

        ! Recursively process children if this node is expanded
        if (node%is_expanded) then
            child => node%first_child
            do while (associated(child))
                call collect_items_from_tree(child, full_path, depth + 1, items, n_items, max_items, hide_dotfiles)
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

    subroutine navigate_right(items, n_items, selected, tree_root, hide_dotfiles)
        type(selectable_item), allocatable, intent(inout) :: items(:)
        integer, intent(inout) :: n_items
        integer, intent(inout) :: selected
        type(tree_node), pointer, intent(in) :: tree_root
        logical, intent(in) :: hide_dotfiles
        integer :: i, target_depth

        if (n_items == 0) return
        if (items(selected)%is_file) return  ! Can't enter a file

        ! We're on a directory
        if (.not. items(selected)%node%is_expanded) then
            ! Directory is collapsed - expand it
            items(selected)%node%is_expanded = .true.
            ! Rebuild item list
            call rebuild_item_list_from_tree(tree_root, items, n_items, hide_dotfiles)
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

    subroutine navigate_left(items, n_items, selected)
        type(selectable_item), allocatable, intent(inout) :: items(:)
        integer, intent(inout) :: n_items
        integer, intent(inout) :: selected
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

            ! Wait for keypress to continue (flush buffered input first)
            call wait_for_key(key)
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

            ! Wait for keypress to continue (flush buffered input first)
            call wait_for_key(key)
        else
            print '(A)', 'No commit message provided. Amend cancelled.'
            print '(A)', 'Press any key to continue...'
            call wait_for_key(key)
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

        ! Wait for keypress to continue (flush buffered input first)
        call wait_for_key(key)
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

            ! Wait for keypress to continue (flush buffered input first)
            print '(A)', 'Press any key to continue...'
            call wait_for_key(key)
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

    subroutine cherry_pick_prompt()
        logical :: success

        ! Clear screen for cherry-pick
        call clear_screen()

        ! Call git cherry-pick (handles its own prompts and key wait)
        call git_cherry_pick(success)
    end subroutine cherry_pick_prompt

    subroutine revert_commit_prompt()
        logical :: success

        ! Clear screen for revert
        call clear_screen()

        ! Call git revert (handles its own prompts and key wait)
        call git_revert_commit(success)
    end subroutine revert_commit_prompt

    subroutine history_browser_prompt()
        ! Clear screen for history browser
        call clear_screen()

        ! Call git history browser (handles its own terminal setup)
        call git_show_history()
    end subroutine history_browser_prompt

    subroutine reflog_browser_prompt()
        ! Clear screen for reflog browser
        call clear_screen()

        ! Call git reflog browser (handles its own terminal setup)
        call git_show_reflog()
    end subroutine reflog_browser_prompt

    subroutine merge_branch_prompt()
        logical :: success

        ! Clear screen for merge
        call clear_screen()

        ! Call git merge (handles its own prompts and key wait)
        call git_merge_branch(success)
    end subroutine merge_branch_prompt

    subroutine blame_prompt(filepath)
        character(len=*), intent(in) :: filepath

        ! Clear screen for blame view
        call clear_screen()

        ! Call git blame (handles its own display and key wait)
        call git_blame_file(filepath)
    end subroutine blame_prompt

    subroutine reset_prompt()
        logical :: success

        ! Clear screen for reset
        call clear_screen()

        ! Call git reset (handles its own prompts and key wait)
        call git_reset_interactive(success)
    end subroutine reset_prompt

    subroutine rebase_prompt()
        logical :: success

        ! Clear screen for rebase
        call clear_screen()

        ! Call git rebase (handles its own prompts and key wait)
        call git_interactive_rebase(success)
    end subroutine rebase_prompt

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

    subroutine refresh_and_rebuild(show_all, files, n_files, items, n_items, tree_root, &
                                    hide_dotfiles, selected, running, exit_if_empty, include_incoming, force_refresh)
        ! Centralized helper to refresh file list and rebuild tree
        ! Consolidates the pattern repeated 20+ times in the codebase
        ! Now with caching support for performance optimization
        logical, intent(in) :: show_all, hide_dotfiles
        type(file_entry), allocatable, intent(inout) :: files(:)
        integer, intent(inout) :: n_files, n_items, selected
        type(tree_node), pointer, intent(inout) :: tree_root
        type(selectable_item), allocatable, intent(inout) :: items(:)
        logical, intent(inout), optional :: running
        logical, intent(in), optional :: exit_if_empty, include_incoming, force_refresh

        logical :: do_exit_if_empty, do_include_incoming, do_force_refresh

        ! Handle optional parameters
        do_exit_if_empty = .false.
        if (present(exit_if_empty)) do_exit_if_empty = exit_if_empty

        do_include_incoming = .false.
        if (present(include_incoming)) do_include_incoming = include_incoming

        do_force_refresh = .false.
        if (present(force_refresh)) do_force_refresh = force_refresh

        ! Get files based on mode (with caching support)
        if (show_all) then
            call get_all_files(files, n_files, force_refresh=do_force_refresh)
            call mark_incoming_changes(files, n_files)
        else
            call get_dirty_files(files, n_files, force_refresh=do_force_refresh)
            if (do_include_incoming) then
                ! For fetch/pull: also include files with only incoming changes
                call add_incoming_files(files, n_files)
            else
                call mark_incoming_changes(files, n_files)
            end if
        end if

        ! Rebuild tree and flatten to items
        call build_item_list(files, n_files, items, n_items, tree_root, hide_dotfiles)

        ! Adjust selection if needed
        if (selected > n_items .and. n_items > 0) selected = n_items

        ! Exit if no items and exit_if_empty is set
        if (do_exit_if_empty .and. n_items == 0) then
            if (present(running)) running = .false.
        end if
    end subroutine refresh_and_rebuild

    subroutine fuzzy_jump_to_match(items, n_items, pattern, selected)
        ! Jump to BEST matching item using fzf-style scoring
        ! Two-pass approach: basename matches first, then path matches
        ! This ensures "src" matches "src/" directory before "src/file.f90"
        type(selectable_item), intent(in) :: items(:)
        integer, intent(in) :: n_items
        character(len=*), intent(in) :: pattern
        integer, intent(inout) :: selected
        integer :: i, best_idx, best_score, score, current_score

        best_idx = selected  ! Stay at current if no matches
        best_score = 0

        ! Check current item's basename first - if it's a perfect match, stay on it!
        if (associated(items(selected)%node)) then
            current_score = fuzzy_match_score(pattern, items(selected)%node%name)
            if (current_score >= 10000) then  ! Exact match - stay here!
                ! DEBUG
                open(99, file='/tmp/fuss_debug.log', position='append')
                write(99, '(A,I0,A,A,A,I0,A)') '  EXACT MATCH (current): item=', selected, ' path=', &
                                              trim(items(selected)%path), ' score=', current_score, ' (basename)'
                close(99)
                return
            end if
            best_score = current_score
            best_idx = selected
        end if

        ! PASS 1: Search for basename matches (directories, file names)
        do i = 1, n_items
            if (i == selected) cycle  ! Already checked current above

            if (associated(items(i)%node)) then
                score = fuzzy_match_score(pattern, items(i)%node%name)
                if (score > best_score) then
                    best_score = score
                    best_idx = i
                end if
            end if
        end do

        ! If we found a good basename match, use it
        if (best_score >= 5000) then  ! Prefix or exact match
            selected = best_idx
            ! DEBUG
            open(99, file='/tmp/fuss_debug.log', position='append')
            write(99, '(A,I0,A,A,A,I0,A)') '  BASENAME MATCH: item=', best_idx, ' path=', &
                                          trim(items(best_idx)%path), ' score=', best_score, ' (basename)'
            close(99)
            return
        end if

        ! PASS 2: Search full paths if no good basename match
        do i = 1, n_items
            if (i == selected) cycle

            score = fuzzy_match_score(pattern, items(i)%path)
            if (score > best_score) then
                best_score = score
                best_idx = i
            end if
        end do

        ! Jump to best match if any was found
        if (best_score > 0) then
            selected = best_idx
            ! DEBUG
            open(99, file='/tmp/fuss_debug.log', position='append')
            write(99, '(A,I0,A,A,A,I0,A)') '  PATH MATCH: item=', best_idx, ' path=', &
                                          trim(items(best_idx)%path), ' score=', best_score, ' (fullpath)'
            close(99)
        end if
    end subroutine fuzzy_jump_to_match

    function fuzzy_match_score(pattern, text) result(score)
        ! Fuzzy matching with fzf-style scoring
        ! Returns a score (higher is better), 0 means no match
        character(len=*), intent(in) :: pattern, text
        integer :: score
        integer :: pattern_idx, text_idx, match_start, consecutive_bonus
        character(len=256) :: pattern_lower, text_lower
        logical :: is_consecutive

        score = 0

        ! Empty pattern matches everything with score 1
        if (len_trim(pattern) == 0) then
            score = 1
            return
        end if

        ! Convert to lowercase once
        pattern_lower = pattern
        text_lower = text
        call to_lowercase(pattern_lower)
        call to_lowercase(text_lower)

        ! Check for exact match first (highest score)
        if (trim(pattern_lower) == trim(text_lower)) then
            score = 10000
            return
        end if

        ! Check for prefix match (very high score)
        if (len_trim(pattern_lower) <= len_trim(text_lower)) then
            if (text_lower(1:len_trim(pattern_lower)) == trim(pattern_lower)) then
                score = 5000
                return
            end if
        end if

        ! Fuzzy match with scoring
        pattern_idx = 1
        consecutive_bonus = 0
        is_consecutive = .false.
        match_start = -1

        do text_idx = 1, len_trim(text_lower)
            if (pattern_idx > len_trim(pattern_lower)) exit

            if (pattern_lower(pattern_idx:pattern_idx) == text_lower(text_idx:text_idx)) then
                if (match_start == -1) match_start = text_idx

                ! Base score for each matched character
                score = score + 100

                ! Bonus for consecutive characters
                if (is_consecutive) then
                    consecutive_bonus = consecutive_bonus + 1
                    score = score + consecutive_bonus * 50
                else
                    consecutive_bonus = 1
                    is_consecutive = .true.
                end if

                ! Bonus for matching at start of text
                if (text_idx == 1) then
                    score = score + 200
                end if

                ! Bonus for matching after separator (word boundary)
                if (text_idx > 1) then
                    if (text_lower(text_idx-1:text_idx-1) == '/' .or. &
                        text_lower(text_idx-1:text_idx-1) == '_' .or. &
                        text_lower(text_idx-1:text_idx-1) == '-' .or. &
                        text_lower(text_idx-1:text_idx-1) == '.') then
                        score = score + 150
                    end if
                end if

                pattern_idx = pattern_idx + 1
            else
                ! Reset consecutive bonus when characters don't match
                is_consecutive = .false.
                consecutive_bonus = 0
                ! Small penalty for gaps
                if (match_start > 0) then
                    score = score - 1
                end if
            end if
        end do

        ! No match if we didn't find all pattern characters
        if (pattern_idx <= len_trim(pattern_lower)) then
            score = 0
            return
        end if

        ! Bonus for shorter strings (prefer concise matches)
        score = score - len_trim(text_lower)

    end function fuzzy_match_score

    subroutine to_lowercase(str)
        ! Convert string to lowercase in-place
        character(len=*), intent(inout) :: str
        integer :: i
        character(len=1) :: c

        do i = 1, len_trim(str)
            c = str(i:i)
            if (c >= 'A' .and. c <= 'Z') then
                str(i:i) = achar(ichar(c) + 32)
            end if
        end do
    end subroutine to_lowercase

end program fuss
