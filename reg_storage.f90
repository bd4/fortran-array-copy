module reg_storage_m
  type, public :: reg_storage_t
    integer :: n_elements
    complex, dimension(:), allocatable :: alloc
  contains
    procedure :: malloc
    procedure :: free
    procedure :: get_4D_ptr
  end type reg_storage_t
contains
  subroutine malloc(this, n)
    class(reg_storage_t) :: this
    integer, intent(in) :: n
    
    allocate(this%alloc(n))
  end subroutine malloc

  subroutine free(this)
    class(reg_storage_t) :: this
    deallocate(this%alloc)
  end subroutine free

  function get_4D_ptr(this, l1, u1, l2, u2, l3, u3, l4, u4)
    class(reg_storage_t), target :: this
    integer :: l1, u1, l2, u2, l3, u3, l4, u4
    complex, dimension(:,:,:,:), pointer, contiguous :: get_4D_ptr

    get_4D_ptr(l1:u1, l2:u2, l3:u3, l4:u4) => this%alloc
  end function get_4D_ptr
end module reg_storage_m


