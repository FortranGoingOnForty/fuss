module tree_module
    use types_module
    implicit none

contains

    recursive subroutine add_to_tree(node, path, is_staged, is_unstaged, is_untracked, has_incoming)
        type(tree_node), pointer, intent(in) :: node
        character(len=*), intent(in) :: path
        logical, intent(in) :: is_staged, is_unstaged, is_untracked, has_incoming

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
                    child%is_untracked = child%is_untracked .or. is_untracked
                    child%has_incoming = child%has_incoming .or. has_incoming
                    return
                end if
                if (.not. associated(child%next_sibling)) exit
                child => child%next_sibling
            end do

            ! Check if this is a directory
            inquire(file=trim(path), exist=is_directory, iostat=iostat)
            if (iostat /= 0) is_directory = .false.

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
            new_child%is_untracked = is_untracked
            new_child%has_incoming = has_incoming
            new_child%is_expanded = .true.  ! Directories expanded by default
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
                    call add_to_tree(child, rest, is_staged, is_unstaged, is_untracked, has_incoming)
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
            new_child%is_untracked = .false.
            new_child%has_incoming = .false.
            new_child%is_expanded = .true.  ! Directories expanded by default
            new_child%first_child => null()
            new_child%next_sibling => null()

            if (.not. associated(node%first_child)) then
                node%first_child => new_child
            else
                child%next_sibling => new_child
            end if

            call add_to_tree(new_child, rest, is_staged, is_unstaged, is_untracked, has_incoming)
        end if
    end subroutine add_to_tree

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
                ! Find insertion point
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

end module tree_module
