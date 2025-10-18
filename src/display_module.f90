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
        root%first_child => null()
        root%next_sibling => null()

        ! Build tree
        do i = 1, n_files
            call add_to_tree(root, files(i)%path, files(i)%is_staged, files(i)%is_unstaged, files(i)%is_untracked)
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

        write(mark_unstaged, '(A,A,A,A,A)') ESC, '[31m', ' ✗', ESC, '[0m'
        write(mark_untracked, '(A,A,A,A,A)') ESC, '[90m', ' ✗', ESC, '[0m'
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
            ! Build line
            if (is_last) then
                line = prefix // branch_last // ' ' // trim(node%name)
            else
                line = prefix // branch_mid // ' ' // trim(node%name)
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
        root%is_untracked = .false.
        root%first_child => null()
        root%next_sibling => null()

        do i = 1, n_files
            call add_to_tree(root, files(i)%path, files(i)%is_staged, files(i)%is_unstaged, files(i)%is_untracked)
        end do

        call sort_tree(root)

        ! Print tree with selection highlighting
        item_idx = 0
        print '(A)', '.'
        call print_interactive_node(root, '', .true., .true., items, selected, item_idx)

        ! Print help
        print '(A)', ''
        print '(A)', achar(27) // '[32m↑' // achar(27) // '[0m=staged ' // &
                     achar(27) // '[31m✗' // achar(27) // '[0m=modified ' // &
                     achar(27) // '[90m✗' // achar(27) // '[0m=untracked'
        print '(A)', 'j/↓: down | k/↑: up | Space: stage file | q: quit'

        call free_tree(root)
    end subroutine draw_interactive_tree

    recursive subroutine print_interactive_node(node, prefix, is_last, is_root, items, selected, item_idx)
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

        ! Build colored marks
        character(len=50) :: mark_unstaged
        character(len=50) :: mark_untracked
        character(len=50) :: mark_staged

        write(mark_unstaged, '(A,A,A,A,A)') ESC, '[31m', ' ✗', ESC, '[0m'
        write(mark_untracked, '(A,A,A,A,A)') ESC, '[90m', ' ✗', ESC, '[0m'
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
            ! Increment item index for all nodes
            item_idx = item_idx + 1
            is_selected = (item_idx == selected)

            ! Build line with appropriate branch character
            if (is_last) then
                line = prefix // branch_last // ' '
            else
                line = prefix // branch_mid // ' '
            end if

            ! Add name with highlighting if selected
            if (is_selected) then
                line = trim(line) // highlight_on // trim(node%name)
                if (node%is_staged) then
                    line = trim(line) // trim(mark_staged)
                end if
                if (node%is_unstaged) then
                    line = trim(line) // trim(mark_unstaged)
                end if
                if (node%is_untracked) then
                    line = trim(line) // trim(mark_untracked)
                end if
                line = trim(line) // highlight_off
            else
                line = trim(line) // trim(node%name)
                if (node%is_staged) then
                    line = trim(line) // trim(mark_staged)
                end if
                if (node%is_unstaged) then
                    line = trim(line) // trim(mark_unstaged)
                end if
                if (node%is_untracked) then
                    line = trim(line) // trim(mark_untracked)
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

            call print_interactive_node(child, new_prefix, i == n_children, .false., items, selected, item_idx)
            child => child%next_sibling
        end do
    end subroutine print_interactive_node

end module display_module
