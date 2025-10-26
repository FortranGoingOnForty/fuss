module types_module
    implicit none

    ! Tree node using linked list structure (first-child, next-sibling)
    type :: tree_node
        character(len=256) :: name
        logical :: is_file
        logical :: is_staged
        logical :: is_unstaged
        logical :: is_untracked
        logical :: has_incoming
        logical :: is_expanded  ! True if directory is expanded, always true for files
        type(tree_node), pointer :: first_child => null()
        type(tree_node), pointer :: next_sibling => null()
    end type tree_node

    type :: file_entry
        character(len=512) :: path
        character(len=2) :: status
        logical :: is_staged
        logical :: is_unstaged
        logical :: is_untracked
        logical :: has_incoming
    end type file_entry

    type :: selectable_item
        character(len=512) :: path
        logical :: is_staged
        logical :: is_unstaged
        logical :: is_untracked
        logical :: has_incoming
        logical :: is_file
        integer :: depth  ! Nesting depth (0 = root level)
        type(tree_node), pointer :: node => null()  ! Pointer to corresponding tree node
    end type selectable_item

end module types_module
