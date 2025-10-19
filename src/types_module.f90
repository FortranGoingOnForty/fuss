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
    end type selectable_item

end module types_module
