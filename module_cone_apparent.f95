module module_cone_apparent

use module_parameters
use module_system
use module_cosmology
use module_user
use module_geometry

type(type_snapshot),allocatable     :: snapshot(:)
integer*8                           :: nmockgalaxies
   
contains

subroutine make_cone_apparent

   implicit none
   character(len=255)      :: filename
   integer*8               :: n,i,m
   integer*4               :: bytespergalaxy
   integer*8               :: bytes
   type(type_galaxy_base)  :: base
   type(type_galaxy_sam)   :: sam   ! intrinsic galaxy properties from SAM
   type(type_galaxy_cone)  :: cone  ! apparent galaxy properties
   
   ! write user info
   call tic
   call out('CONVERT INTRINSIC CONE TO APPARENT CONE')
   
   ! determine number of bytes per galaxy in intrinsic cone
   filename = trim(para%path_output)//'.tmpsizeof'
   open(1,file=trim(filename),action='write',form="unformatted",status='replace',access='stream')
   write(1) base,sam
   close(1)
   inquire(file=trim(filename), size=bytespergalaxy)
   
   ! determine number of galaxies in intrinsic cone
   filename = trim(para%path_output)//'cone_intrinsic.bin'
   inquire(file=filename, size=bytes)
   if (modulo(bytes,bytespergalaxy).ne.0) then
      call out('ERROR: Size of intrinsic cone file inconsistent with type_galaxy_base and/or type_galaxy_sam.')
      stop
   end if
   n = bytes/bytespergalaxy
   
   ! open intrinsic cone
   open(2,file=trim(filename),action='read',form='unformatted',status='old',access='stream')
   
   ! initialize master one
   filename = trim(para%path_output)//'cone_apparent.bin'
   open(1,file=trim(filename),action='write',form="unformatted",status='replace',access='stream')
   
   ! write user info
   call out('Number of galaxies in intrinsic cone:',n)
   
   ! convert galaxy properties and write master cone
   m = 0
   do i = 1,n
      read(2) base,sam
      cone = convert_properties(base,sam)
      if (post_selection(cone)) then
         m = m+1
         call write_galaxy(cone)
      end if
   end do
   
   ! close files
   close(1)
   close(2)
   
   ! write info
   inquire(file=filename, size=bytes)
   filename = trim(para%path_output)//'cone_apparent_info.txt'
   open(1,file=trim(filename),action='write',form="formatted",status='replace')
   write(1,'(A,I10)') 'Number.of.galaxies.in.apparent.cone        ',m
   write(1,'(A,I10)') 'Number.of.bytes.per.galaxy.in.apparent.cone',bytes/m
   close(1)
   
   ! user output
   call out('Number of galaxies in apparent cone:',m)
   call out('Number of bytes per galaxy in apparent cone:',bytes/m)
   call toc
   
end subroutine make_cone_apparent

end module module_cone_apparent