! Claudia:
! + include L in hdf5, currently computed from effective volumes
! + some positions are just above 210 (assume these are groups or trees that have moved out)
! + many positions -1
! + add units in dataset headers
! + track galaxy axes

module module_user

! default modules, do not edit
use module_constants
use module_types
use module_system
use module_parameters
use module_cosmology
use module_conversion

! custom modules
use module_hdf5_utilities

! ==============================================================================================================
! VARIABLE TYPES
! ==============================================================================================================

! specify the galaxy properties output by the semi-analytic model
! these are mostly intrinsic galaxy properties
! they must be stored as fortran binary file exactly in this order

type type_galaxy_sam

   integer*8   :: id_galaxy      ! unique galaxy ID
   integer*8   :: id_halo        ! unique ID of parent halo
   real*4      :: position(3)    ! [Mpc/h] position of galaxy centre in simulation box
   real*4      :: velocity(3)    ! [proper km/s] peculiar velocity
   real*4      :: mstars_disk    ! [Msun] stellar mass disk
   real*4      :: mstars_bulge   ! [Msun] stellar mass bulge
   real*4      :: matom_disk     ! [Msun] atomic gas mass disk
   real*4      :: matom_bulge    ! [Msun] atomic gas mass bulge

end type type_galaxy_sam

! specify the galaxy properties in the mock cone
! these are mostly apparent galaxy properties

type type_galaxy_cone

   integer*8   :: id             ! unique galaxy ID
   integer*4   :: snapshot       ! snapshot
   real*4      :: z              ! apparent redshift
   real*4      :: dc             ! [simulation units = Mpc/h] comoving distance
   real*4      :: ra             ! [rad] right ascension
   real*4      :: decl           ! [rad] declination
   real*4      :: mag            ! apparent magnitude (generic band)
   real*8      :: SHI            ! [W/m^2] integrated HI line flux
   real*4      :: v(3)           ! [proper km/s] peculiar velocity

end type type_galaxy_cone

contains

! mapping from type_galaxy_sam onto three basic properties of type_galaxy_base

function extract_base(sam) result(base)
   
   implicit none
   type(type_galaxy_sam)   :: sam
   type(type_galaxy_base)  :: base
   
   base%id        = sam%id_galaxy
   base%groupid   = sam%id_halo
   base%xbox      = sam%position

end function extract_base


! ==============================================================================================================
! SELECTION OF GALAXIES IN THE MOCK OBSERVING CONE FILE
! ==============================================================================================================

! selection function applied to SAM-properties, *before* producing intrinsic cone

logical function pre_selection(sam)

   implicit none
   type(type_galaxy_sam)   :: sam
   
   pre_selection = (sam%position(1)>=0.0) .and. (sam%mstars_disk>1e9)

end function pre_selection

! selection function applied to SAM-properties, when converting the intrinsic cone into the apparent cone

logical function post_selection(cone)

   implicit none
   type(type_galaxy_cone)  :: cone
   
   post_selection = cone%id>-1

end function post_selection


! ==============================================================================================================
! IO routines
! ==============================================================================================================

! load parameters

subroutine make_parameters(parameter_filename_custom)
   
   implicit none
   
   character(len=255),intent(in) :: parameter_filename_custom
   character(len=255),parameter  :: parameter_filename_default = 'parameters_shark.txt'
   character(len=255)            :: parameter_filename
   character(len=255)            :: filename
   real*4                        :: Veff
   
   ! load parameters from parameter file (normally don't edit this part)
   if (len(trim(parameter_filename_custom))>0) then
      parameter_filename = parameter_filename_custom
   else
      parameter_filename = parameter_filename_default
   end if
   call initialize_default_parameters
   call load_parameters(parameter_filename)
   call check_and_adjust_parameters
   
   ! set parameters automatically (e.g. from information provided with the snapshot files)
   write(filename,'(A,I0,A)') trim(para%path_input),para%snapshot_min,'/0/galaxies.hdf5'
   call hdf5_open(filename)
   call hdf5_read_dataset('/Cosmology/h',para%h)
   call hdf5_read_dataset('/Cosmology/OmegaL',para%OmegaL)
   call hdf5_read_dataset('/Cosmology/OmegaM',para%OmegaM)
   call hdf5_read_dataset('/runInfo/EffectiveVolume',Veff)
   para%L = (Veff*64)**(1.0/3.0)
   call hdf5_close()
   
end subroutine make_parameters

! write mock-cone galaxy into binary file

subroutine write_galaxy(cone)

   ! choose which variables of the structure 'cone' to save
   type(type_galaxy_cone),intent(in) :: cone
   write(1) cone
   
end subroutine write_galaxy

! load redshifts
! this routine must allocate the array snapshot and fill in its real*4-valued property 'redshift'

subroutine load_redshifts(snapshot)

   implicit none
   type(type_snapshot),intent(inout),allocatable   :: snapshot(:)
   character(len=255)                              :: filename
   integer*4                                       :: isnapshot
   real*8                                          :: z

   allocate(snapshot(para%snapshot_min:para%snapshot_max))
   do isnapshot = para%snapshot_min,para%snapshot_max
   
      write(filename,'(A,I0,A)') trim(para%path_input),isnapshot,'/0/galaxies.hdf5'
      call hdf5_open(filename)
      call hdf5_read_dataset('/runInfo/redshift',z)
      snapshot(isnapshot)%redshift = real(z,4)
      call hdf5_close()
      
   end do
   
end subroutine load_redshifts

! load SAM snapshot file

subroutine load_sam_snapshot(index,subindex,sam,snapshotname)

   ! variable declaration
   implicit none
   integer*4,intent(in)                            :: index,subindex
   type(type_galaxy_sam),allocatable,intent(out)   :: sam(:)
   character(len=100),intent(out)                  :: snapshotname
   character(len=255)                              :: filename
   integer*8                                       :: n
   character(*),parameter                          :: g = '/Galaxies/' ! group name
   
   ! open file
   write(filename,'(A,I0,A,I0,A)') trim(para%path_input),index,'/',subindex,'/galaxies.hdf5'
   call hdf5_open(filename)
   
   ! determine number of galaxies in this (sub)snapshot
   n = hdf5_dataset_size(g//'id_galaxy')

   ! allocate array
   if (allocated(sam)) deallocate(sam)
   allocate(sam(n))
   
   ! read file
   call hdf5_read_dataset(g//'id_galaxy',sam%id_galaxy)
   call hdf5_read_dataset(g//'id_halo',sam%id_halo)
   call hdf5_read_dataset(g//'position_x',sam%position(1))
   call hdf5_read_dataset(g//'position_y',sam%position(2))
   call hdf5_read_dataset(g//'position_z',sam%position(3))
   call hdf5_read_dataset(g//'velocity_x',sam%velocity(1))
   call hdf5_read_dataset(g//'velocity_y',sam%velocity(2))
   call hdf5_read_dataset(g//'velocity_z',sam%velocity(3))
   call hdf5_read_dataset(g//'mstars_disk',sam%mstars_disk)
   call hdf5_read_dataset(g//'mstars_bulge',sam%mstars_bulge)
   call hdf5_read_dataset(g//'matom_disk',sam%matom_disk)
   call hdf5_read_dataset(g//'matom_bulge',sam%matom_bulge)
   
   ! close file
   call hdf5_close()
   
   ! return snapshot name for screen output
   write(snapshotname,'(I0,A,I0,A,I0,A)') index,', subindex ',subindex,' (',n,' galaxies)'
   
end subroutine load_sam_snapshot


! ==============================================================================================================
! CONVERSION BETWEEN INTRINSIC AND APPARENT GALAXY PROPERTIES
! ==============================================================================================================

function convert_properties(base,sam) result(cone)

   implicit none
   type(type_galaxy_base),intent(in)      :: base
   type(type_galaxy_sam),intent(in)       :: sam      ! intrinsic galaxy properties from SAM
   type(type_galaxy_cone)                 :: cone     ! apparent galaxy properties
   real*4                                 :: zcos     ! cosmological redshift due to Hubble flow
   real*4                                 :: zpec     ! redshift due to the peculiar motion of object relative to Hubble flow
   real*4                                 :: zobs     ! redshift due to the peculiar motion of the observer relative to the Hubble flow
   real*4                                 :: z        ! total redshift
   real*4                                 :: dc       ! [simulation length units] comoving distance to observer
   real*4                                 :: dl       ! [simulation length units] luminosity distance to observer
   real*4                                 :: da       ! [simulation length units] angular diameter distance to observer
   real*4                                 :: norm     ! [box side-length] comoving distance to observer
   real*4                                 :: elos(3)  ! unit vector pointing from the observer to the object in comoving space
   real*4                                 :: vpec(3)  ! peculiar velocity
   
   ! compute basic redshift and distance
   norm = sqrt(sum(base%xcone**2))
   elos = base%xcone/norm ! unit-vector along the line of slight
   dc = norm*para%L*(para%length_unit/Mpc) ! [Mpc]
   zcos = dc_to_redshift(dc)
   vpec = convert_vector(sam%velocity,base%rotation)
   zpec = min(0.1,max(-0.1,sum(vpec*elos)/c*1e3)) ! limited to 0.1 to avoid relativistic regime, ok for all practical purposes
   zobs = min(0.1,max(-0.1,-sum(para%velocity*base%xcone)/c*1e3)) ! limited to 0.1 to avoid relativistic regime, ok for all practical purposes
   z = (1+zcos)*(1+zpec)*(1+zobs)-1 ! following Davis & Scrimgeour 2014
   dl = dc*(1+z)
   da = dc/(1+z)
   
   ! copy basic variables
   cone%id        = sam%id_galaxy
   cone%snapshot  = base%snapshot
   
   ! make geometric properties
   cone%z      = z
   cone%dc     = norm*para%L ! [simulation units] comoving distance
   cone%ra     = atan2(base%xcone(1),base%xcone(3))
   cone%decl   = asin(base%xcone(2)/norm)
   
   ! convert intrinsic to apparent properties
   cone%mag    = convert_abs2appmag(convert_stellarmass2absmag(sam%mstars_disk+sam%mstars_bulge,1.0),dl)
   cone%v      = vpec
   cone%SHI    = convert_luminosity2flux(real(sam%matom_disk+sam%matom_bulge,8)*real(LMratioHI,8)*Lsun,dl)
   
end function convert_properties


! ==============================================================================================================
! MAKE FAKE DATA
! ==============================================================================================================

subroutine make_fake_data(ngalaxies)

   implicit none
   integer*4,intent(in)    :: ngalaxies
   
   call out('ERROR: No instructions for building fake data have been provided.')
   stop
   
   if (.false.) write(*,*) ngalaxies ! avoids compiler warning if argument unused
   
end subroutine make_fake_data

end module module_user