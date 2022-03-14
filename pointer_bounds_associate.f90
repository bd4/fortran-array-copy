!> Reproducer for bug in ifx related to using bounds when associating a pointer
!  to a function result.
module test_m

contains
  subroutine printAddress(s, a)
    character(len=*), intent(in) :: s
    complex, dimension(*), intent(in), target :: a

    write(*,"(A,' & 0x',Z0)") s, loc(a)
  end subroutine printAddress
end module test_m

module reg_storage_m
  type, public :: reg_storage_t
    integer :: n_elements
    complex, dimension(:), allocatable :: alloc
  contains
    procedure :: malloc
    procedure :: free
    procedure :: get_1D_ptr
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

  function get_1D_ptr(this)
    class(reg_storage_t), target :: this
    complex, dimension(:), pointer, contiguous :: get_1D_ptr

    get_1D_ptr => this%alloc
  end function get_1D_ptr

  function get_4D_ptr(this, l1, u1, l2, u2, l3, u3, l4, u4)
    class(reg_storage_t), target :: this
    integer :: l1, u1, l2, u2, l3, u3, l4, u4
    complex, dimension(:,:,:,:), pointer, contiguous :: get_4D_ptr

    get_4D_ptr(l1:u1, l2:u2, l3:u3, l4:u4) => this%alloc
  end function get_4D_ptr
end module reg_storage_m

program test
  use test_m  
  use reg_storage_m
  implicit none

  class(reg_storage_t), allocatable, target :: reg_stor
  complex, dimension(:,:,:,:), pointer, contiguous :: p
  complex, dimension(:), pointer, contiguous :: p_flat
  complex, dimension(:), pointer, contiguous :: p_flat_nobounds

  integer :: lb, ub

  lb = 1
  ub = lb+10240-1

  allocate(reg_stor)
  call reg_stor%malloc(10240)
  p => reg_stor%get_4D_ptr(0, 15, 0, 15, 1, 2, 1, 20)
  p_flat(lb:ub) => reg_stor%get_1D_ptr()
  p_flat_nobounds => reg_stor%get_1D_ptr()

  if (loc(p) /= loc(p_flat)) then
    print*,'ERROR: not same address'
    call exit(1)
  else
    print*,'OK'
    call exit(0)
  end if

  call printAddress("p        ", p)
  call printAddress("p slice 1", p(:,:,:,1))
  call printAddress("p slice 2", p(:,:,:,2))
  call printAddress("p_flat        ", p_flat)
  call printAddress("p_flat slice 1", p_flat(lb:))
  call printAddress("p_flat slice 2", p_flat(lb+10240/20:))
  call printAddress("p_flat nb        ", p_flat_nobounds)
  call printAddress("p_flat nb slice 1", p_flat_nobounds(lb:))
  call printAddress("p_flat nb slice 2", p_flat_nobounds(lb+10240/20:))
end program test
