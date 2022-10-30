! { dg-do compile }
!
! Bug 106731 - ICE on automatic array of derived type with DTIO
! Contributed by Federico Perini, federico.perini@gmail.com
! 
module d 
    implicit none

    type :: t
        contains
        procedure :: write_formatted
        generic :: write(formatted) => write_formatted
    end type t

    contains

    subroutine write_formatted(this, unit, iotype, v_list, iostat, iomsg)
       class(t), intent(in) :: this
       integer, intent(in) :: unit
       character(*), intent(in) :: iotype
       integer, intent(in) :: v_list(:)
       integer, intent(out) :: iostat
       character(*), intent(inout) :: iomsg
       write(unit, '(a)', iostat=iostat, iomsg=iomsg) 'dummy'
    end subroutine write_formatted

end module d 
      
program test
  use d
  implicit none

  call print_auto(3)
  call print_auto(10)

  contains 

    subroutine print_auto(n)
          integer, intent(in) :: n

          integer :: i
          type(t) :: auto(n) ! automatic

          do i=1,n
            print *, auto(i)
          end do
    end subroutine print_auto

end program test  

