! { dg-do run }
!
! Test dtio for static arrays of derived types 
! Introduced after removing TREE_STATIC for derived types with DTIO 
! Contributed by Federico Perini, federico.perini@gmail.com
!
MODULE p
  TYPE :: person
    sequence
    CHARACTER (LEN=20) :: name
    INTEGER(4) :: age
  END TYPE person
  INTERFACE WRITE(UNFORMATTED)
    MODULE PROCEDURE pwuf
  END INTERFACE
  INTERFACE READ(UNFORMATTED)
    MODULE PROCEDURE pruf
  END INTERFACE
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE peq
  END INTERFACE

CONTAINS

  SUBROUTINE pwuf (dtv,unit,iostat,iomsg)
    type(person), INTENT(IN) :: dtv
    INTEGER, INTENT(IN) :: unit
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    WRITE (UNIT=UNIT, IOSTAT=IOSTAT, IOMSG=IOMSG) DTV%name, DTV%age
  END SUBROUTINE pwuf

  SUBROUTINE pruf (dtv,unit,iostat,iomsg)
    type(person), INTENT(INOUT) :: dtv
    INTEGER, INTENT(IN) :: unit
    INTEGER, INTENT(OUT) :: iostat
    CHARACTER (LEN=*), INTENT(INOUT) :: iomsg
    READ (UNIT=UNIT, IOSTAT=IOSTAT, IOMSG=IOMSG) dtv%name, dtv%age
  END SUBROUTINE pruf

  ELEMENTAL LOGICAL FUNCTION peq (dtv,other)
    type(person), intent(in) :: dtv,other
    integer :: i
    peq = .FALSE.
    do i=1,len(dtv%name)
      if (dtv%name(i:i)/=other%name(i:i)) return
    end do
    if (dtv%age/=other%age) return
    peq = .TRUE.
  END FUNCTION peq

END MODULE p

PROGRAM test_dtio_automatic_Array
  USE p
  TYPE (person) :: teamr(10),team(10)
  character(10) :: line
  character(256) :: iomsg
  integer :: iostat

  team(1) = person("Charlie",62)
  team(2) = person("Linda",34)
  team(3) = person("Paul",52)
  team(4) = person("Eddie",73)
  team(5) = person("Steve",43)
  team(6) = person("Julia",24)
  team(7) = person("Rebecca",55)
  team(8) = person("Laura",33)
  team(9) = person("Chris",37)
  team(10) = person("Dave",48)

  OPEN (UNIT=71, status = 'scratch', FORM='UNFORMATTED')
  call write_automatic(10,team,71) 
  rewind (71)

  call read_automatic(10,teamr,71) 
  close (unit = 71)

  if (.not.all(team==teamr)) then 
     STOP -1
  else
     STOP 0
  endif

  contains 

      subroutine write_automatic(n,people,unit)
         integer, intent(in) :: n,unit
         type(person), intent(in) :: people(n)

         ! Check ICE with this automatic array
         type(person) :: team(64)
         integer :: buf

         buf = min(n,size(team))
         team(1:buf) = people(1:buf)

         write(unit) team
      end subroutine write_automatic

      subroutine read_automatic(n,people,unit)
         integer, intent(in) :: n,unit
         type(person), intent(out) :: people(n)

         ! Check ICE with this automatic array
         type(person) :: team(64)
         integer :: buf

         buf = min(n,size(team))

         read(unit) team
         people(1:buf) = team(1:buf)

      end subroutine read_automatic
   
END PROGRAM test_dtio_automatic_array
