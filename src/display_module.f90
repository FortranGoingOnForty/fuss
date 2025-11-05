module display_module
    use types_module
    use tree_module
    use git_module
    use terminal_module
    implicit none

contains

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
        root%is_untracked = .false.
        root%has_incoming = .false.
        root%is_expanded = .true.  ! Root is always expanded
        root%first_child => null()
        root%next_sibling => null()

        ! Build tree
        do i = 1, n_files
            call add_to_tree(root, files(i)%path, files(i)%is_staged, files(i)%is_unstaged, files(i)%is_untracked, files(i)%has_incoming, files(i)%is_gitignored)
        end do

        ! Sort tree
        call sort_tree(root)

        ! Print tree
        call print_tree_node(root, '', .true., .true.)

        ! Cleanup
        call free_tree(root)
    end subroutine display_tree

    recursive subroutine print_tree_node(node, prefix, is_last, is_root)
        type(tree_node), pointer, intent(in) :: node
        character(len=*), intent(in) :: prefix
        logical, intent(in) :: is_last, is_root

        character(len=1024) :: line
        character(len=:), allocatable :: new_prefix
        type(tree_node), pointer :: child
        integer :: n_children, i

        ! UTF-8 box-drawing characters
        character(len=*), parameter :: branch_last = '└──'
        character(len=*), parameter :: branch_mid = '├──'
        character(len=*), parameter :: vertical = '│'
        character(len=1), parameter :: ESC = achar(27)

        ! Build colored marks
        character(len=50) :: mark_unstaged
        character(len=50) :: mark_untracked
        character(len=50) :: mark_staged
        character(len=50) :: mark_incoming

        write(mark_unstaged, '(A,A,A,A,A)') ESC, '[31m', ' ✗', ESC, '[0m'
        write(mark_untracked, '(A,A,A,A,A)') ESC, '[90m', ' ✗', ESC, '[0m'
        write(mark_staged, '(A,A,A,A,A)') ESC, '[32m', ' ↑', ESC, '[0m'
        write(mark_incoming, '(A,A,A,A,A)') ESC, '[34m', ' ↓', ESC, '[0m'

        ! Count children first
        n_children = 0
        child => node%first_child
        do while (associated(child))
            n_children = n_children + 1
            child => child%next_sibling
        end do

        ! Don't print root node
        if (.not. is_root) then
            ! Build line
            if (is_last) then
                line = prefix // branch_last // ' '
            else
                line = prefix // branch_mid // ' '
            end if

            ! Add name with grey color if gitignored
            if (node%is_gitignored) then
                line = trim(line) // ESC // '[90m' // trim(node%name) // ESC // '[0m'
            else
                line = trim(line) // trim(node%name)
            end if

            ! Show all applicable indicators
            if (node%is_staged) then
                line = trim(line) // trim(mark_staged)
            end if
            if (node%is_unstaged) then
                line = trim(line) // trim(mark_unstaged)
            end if
            if (node%is_untracked) then
                line = trim(line) // trim(mark_untracked)
            end if
            if (node%has_incoming) then
                line = trim(line) // trim(mark_incoming)
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

            call print_tree_node(child, new_prefix, i == n_children, .false.)
            child => child%next_sibling
        end do
    end subroutine print_tree_node

    subroutine draw_interactive_tree(tree_root, items, n_items, selected, &
                                     repo_name, branch_name, viewport_offset, visible_items, top_padding, mode)
        type(tree_node), pointer, intent(in) :: tree_root
        integer, intent(in) :: n_items, selected
        type(selectable_item), intent(in) :: items(:)
        character(len=*), intent(in) :: repo_name, branch_name, mode
        integer, intent(in) :: viewport_offset, visible_items, top_padding
        integer :: item_idx, viewport_end, i
        character(len=512) :: status_line

        ! Add blank lines at top as padding for terminals that need it (fixes WezTerm/Ghostty)
        do i = 1, top_padding
            print '(A)', ''
        end do

        ! Display repo:branch info at top with mode indicator
        if (len_trim(repo_name) > 0 .and. len_trim(branch_name) > 0) then
            if (mode == 'git') then
                ! Git mode: show in yellow/orange
                write(status_line, '(A,A,A,A,A,A,A,A,A,A,A)') &
                    achar(27) // '[1;36m', trim(repo_name), achar(27) // '[0m', &
                    ':', &
                    achar(27) // '[1;33m', trim(branch_name), achar(27) // '[0m', &
                    ' ', &
                    achar(27) // '[1;33m[ GIT MODE ]', achar(27) // '[0m'
            else
                ! Normal mode
                write(status_line, '(A,A,A,A,A,A,A)') &
                    achar(27) // '[1;36m', trim(repo_name), achar(27) // '[0m', &
                    ':', &
                    achar(27) // '[1;33m', trim(branch_name), achar(27) // '[0m'
            end if
            print '(A)', trim(status_line)
            print '(A)', ''
        end if

        ! Calculate viewport range
        viewport_end = viewport_offset + visible_items - 1
        if (viewport_end > n_items) viewport_end = n_items

        ! Print tree with selection highlighting
        item_idx = 0
        print '(A)', '.'
        call print_interactive_node(tree_root, '', .true., .true., items, selected, &
                                    item_idx, viewport_offset, viewport_end)

        ! Print help (mode-dependent)
        print '(A)', ''
        if (mode == 'git') then
            ! Git mode help - show in yellow tint
            print '(A)', achar(27) // '[33mLegend: ' // achar(27) // '[32m↑' // achar(27) // '[0m=staged ' // &
                         achar(27) // '[31m✗' // achar(27) // '[0m=modified ' // &
                         achar(27) // '[90m✗' // achar(27) // '[0m=untracked ' // &
                         achar(27) // '[34m↓' // achar(27) // '[0m=incoming' // achar(27) // '[0m'
            print '(A)', achar(27) // '[33mKeys: j/k/↑/↓:nav | ←/→:nav tree | space:toggle | .:hide-dots | a:stage | u:unstage | S:stage-all | U:unstage-all | x:discard | z:stash | Z:unstash | b:switch | n:new-br | R:del-br | G:merge | O:reset | I:rebase | f:fetch | d:diff | c:view | w:blame | h:history | L:reflog | y:cherry-pick | v:revert | r:delete | l:pull | m:commit | M:amend | p:push | t:tag | s:status | q:exit-mode | ESC:exit-mode' // achar(27) // '[0m'
        else
            ! Normal mode help
            print '(A)', 'Legend: ' // achar(27) // '[32m↑' // achar(27) // '[0m=staged ' // &
                         achar(27) // '[31m✗' // achar(27) // '[0m=modified ' // &
                         achar(27) // '[90m✗' // achar(27) // '[0m=untracked ' // &
                         achar(27) // '[34m↓' // achar(27) // '[0m=incoming'
            print '(A)', 'Keys: j/k/↑/↓:nav | ←/→:nav tree | space:toggle | .:hide-dots | alt-g:git-mode | q:quit'
        end if

        ! Don't free tree - it's owned by interactive_mode
    end subroutine draw_interactive_tree

    recursive subroutine print_interactive_node(node, prefix, is_last, is_root, items, selected, &
                                                item_idx, viewport_offset, viewport_end)
        type(tree_node), pointer, intent(in) :: node
        character(len=*), intent(in) :: prefix
        logical, intent(in) :: is_last, is_root
        type(selectable_item), intent(in) :: items(:)
        integer, intent(in) :: selected, viewport_offset, viewport_end
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

        ! Build colored marks
        character(len=50) :: mark_unstaged
        character(len=50) :: mark_untracked
        character(len=50) :: mark_staged
        character(len=50) :: mark_incoming

        write(mark_unstaged, '(A,A,A,A,A)') ESC, '[31m', ' ✗', ESC, '[0m'
        write(mark_untracked, '(A,A,A,A,A)') ESC, '[90m', ' ✗', ESC, '[0m'
        write(mark_staged, '(A,A,A,A,A)') ESC, '[32m', ' ↑', ESC, '[0m'
        write(mark_incoming, '(A,A,A,A,A)') ESC, '[34m', ' ↓', ESC, '[0m'

        ! Count children first
        n_children = 0
        child => node%first_child
        do while (associated(child))
            n_children = n_children + 1
            child => child%next_sibling
        end do

        ! Don't print root node
        if (.not. is_root) then
            ! Increment item index for all nodes
            item_idx = item_idx + 1
            is_selected = (item_idx == selected)

            ! Only print if within viewport range
            if (item_idx >= viewport_offset .and. item_idx <= viewport_end) then
                ! Build line with appropriate branch character
                if (is_last) then
                    line = prefix // branch_last // ' '
                else
                    line = prefix // branch_mid // ' '
                end if

                ! Add expand/collapse indicator for directories
                if (.not. node%is_file) then
                    if (node%is_expanded) then
                        line = trim(line) // '▼ '
                    else
                        line = trim(line) // '▶ '
                    end if
                end if

                ! Add name with highlighting if selected
                if (is_selected) then
                    if (node%is_gitignored) then
                        line = trim(line) // highlight_on // ESC // '[90m' // trim(node%name) // ESC // '[0m'
                    else
                        line = trim(line) // highlight_on // trim(node%name)
                    end if
                    if (node%is_staged) then
                        line = trim(line) // trim(mark_staged)
                    end if
                    if (node%is_unstaged) then
                        line = trim(line) // trim(mark_unstaged)
                    end if
                    if (node%is_untracked) then
                        line = trim(line) // trim(mark_untracked)
                    end if
                    if (node%has_incoming) then
                        line = trim(line) // trim(mark_incoming)
                    end if
                    line = trim(line) // highlight_off
                else
                    if (node%is_gitignored) then
                        line = trim(line) // ESC // '[90m' // trim(node%name) // ESC // '[0m'
                    else
                        line = trim(line) // trim(node%name)
                    end if
                    if (node%is_staged) then
                        line = trim(line) // trim(mark_staged)
                    end if
                    if (node%is_unstaged) then
                        line = trim(line) // trim(mark_unstaged)
                    end if
                    if (node%is_untracked) then
                        line = trim(line) // trim(mark_untracked)
                    end if
                    if (node%has_incoming) then
                        line = trim(line) // trim(mark_incoming)
                    end if
                end if

                print '(A)', trim(line)
            end if
        end if

        ! Print children only if expanded (or if this is root)
        if (node%is_expanded) then
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

                call print_interactive_node(child, new_prefix, i == n_children, .false., items, selected, &
                                        item_idx, viewport_offset, viewport_end)
                child => child%next_sibling
            end do
        end if
    end subroutine print_interactive_node

    subroutine draw_status_view(status_lines, n_lines)
        character(len=*), intent(in) :: status_lines(:)
        integer, intent(in) :: n_lines
        integer :: i

        ! Clear screen
        call clear_screen()

        ! Display header
        print '(A)', achar(27) // '[1mGit Status' // achar(27) // '[0m'
        print '(A)', '=========================================='
        print '(A)', ''

        ! Display status output
        do i = 1, n_lines
            print '(A)', trim(status_lines(i))
        end do

        ! Display footer
        print '(A)', ''
        print '(A)', '=========================================='
        print '(A)', 'Press any key to return to tree view...'
    end subroutine draw_status_view

end module display_module
