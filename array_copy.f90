module test_m

contains

    subroutine sliceAndPrintAddress(s, a)
      character(len=*), intent(in) :: s
      complex, dimension(:,:,:,:), intent(in) :: a

      call printAddress(s, a(:,:,:,1))
    end subroutine sliceAndPrintAddress

    subroutine sliceAndPrintAddress2(s, a)
      character(len=*), intent(in) :: s
      complex, dimension(:,:,:,2:), intent(inout) :: a

      do n=2,3
        a(1,1,1,n) = n
        call printAddress(s, a(:,:,:,n))
      end do
    end subroutine sliceAndPrintAddress2

    subroutine sliceAndPrintAddress3(s, a)
      character(len=*), intent(in) :: s
      complex, dimension(:,:,1:,2:), intent(inout) :: a

      do n=2,3
        a(1,1,1,n) = n
        call printAddress(s, a(:,:,:,n))
      end do
    end subroutine sliceAndPrintAddress3

    subroutine printAddress(s, a)
       character(len=*), intent(in) :: s
       complex, dimension(*), intent(in), target :: a

       write(*,"(A,' & 0x',Z0)") s, loc(a)
    end subroutine printAddress
end module test_m

program test
    use test_m  
    implicit none

    complex, dimension(:), allocatable, target :: alloc
    complex, dimension(:,:,:,:), pointer, contiguous :: p

    allocate(alloc(10240))
    p(0:15,0:15,1:2,1:20) => alloc

    call printAddress("p        ", p)
    call printAddress("p slice 1", p(:,:,:,1))
    call printAddress("p slice 2", p(:,:,:,2))

    call sliceAndPrintAddress("sub p slice ", p)

    call sliceAndPrintAddress2("sub p slice2", p)

    call sliceAndPrintAddress3("sub p slice3", p)
end program test
