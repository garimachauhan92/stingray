! **********************************************************************************************************************************
! Shared Fortran module with mathematical functions that do not requires external libraries (such as LAPACK)
! Developed by Danail Obreschkow
! **********************************************************************************************************************************

module shared_module_maths

   private
   
   ! operators
   public   :: operator(.safedivide.)
   
   ! random numbers
   public   :: set_seed
   public   :: get_normal_random_number

   ! geometry
   public   :: sph2car
   public   :: car2sph

   ! linear algebra (without LAPACK)
   public   :: rotation_matrix
   public   :: determinant

   ! spectra analysis
   public   :: spherical_harmonic
   
   interface operator(.safedivide.)
      procedure savedivide_r4_by_r4
      procedure savedivide_r4_by_r8
      procedure savedivide_r4_by_i4
      procedure savedivide_r4_by_i8
      procedure savedivide_r8_by_r4
      procedure savedivide_r8_by_r8
      procedure savedivide_r8_by_i4
      procedure savedivide_r8_by_i8
      procedure savedivide_i4_by_r4
      procedure savedivide_i4_by_r8
      procedure savedivide_i4_by_i4
      procedure savedivide_i4_by_i8
      procedure savedivide_i8_by_r4
      procedure savedivide_i8_by_r8
      procedure savedivide_i8_by_i4
      procedure savedivide_i8_by_i8
   end interface operator(.safedivide.)

contains
   
! operators ************************************************************************************************************************

function savedivide_r4_by_r4(a,b) result(q)
   implicit none
   real*4,intent(in) :: a
   real*4,intent(in) :: b
   real*4 :: q
   if (exponent(a)-exponent(b)>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*sign(1.0,a)*sign(1.0,b)
      end if
   else 
      q = a/b
   endif
end function savedivide_r4_by_r4

function savedivide_r4_by_r8(a,b) result(q)
   implicit none
   real*4,intent(in) :: a
   real*8,intent(in) :: b
   real*8 :: q
   if (exponent(a)-exponent(b)>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*sign(1.0,a)*sign(1.0_8,b)
      end if
   else 
      q = a/b
   endif
end function savedivide_r4_by_r8

function savedivide_r4_by_i4(a,b) result(q)
   implicit none
   real*4,intent(in) :: a
   integer*4,intent(in) :: b
   real*4 :: q
   if (exponent(a)-exponent(real(b,4))>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*sign(1.0,a)*sign(1,b)
      end if
   else 
      q = a/b
   endif
end function savedivide_r4_by_i4

function savedivide_r4_by_i8(a,b) result(q)
   implicit none
   real*4,intent(in) :: a
   integer*8,intent(in) :: b
   real*4 :: q
   if (exponent(a)-exponent(real(b,4))>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*sign(1.0,a)*real(sign(1_8,b),4)
      end if
   else 
      q = a/real(b,4)
   endif
end function savedivide_r4_by_i8

function savedivide_r8_by_r4(a,b) result(q)
   implicit none
   real*8,intent(in) :: a
   real*4,intent(in) :: b
   real*8 :: q
   if (exponent(a)-exponent(b)>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*sign(1.0_8,a)*sign(1.0,b)
      end if
   else 
      q = a/b
   endif
end function savedivide_r8_by_r4

function savedivide_r8_by_r8(a,b) result(q)
   implicit none
   real*8,intent(in) :: a
   real*8,intent(in) :: b
   real*8 :: q
   if (exponent(a)-exponent(b)>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*sign(1.0_8,a)*sign(1.0_8,b)
      end if
   else 
      q = a/b
   endif
end function savedivide_r8_by_r8

function savedivide_r8_by_i4(a,b) result(q)
   implicit none
   real*8,intent(in) :: a
   integer*4,intent(in) :: b
   real*8 :: q
   if (exponent(a)-exponent(real(b,4))>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*sign(1.0_8,a)*sign(1,b)
      end if
   else 
      q = a/b
   endif
end function savedivide_r8_by_i4

function savedivide_r8_by_i8(a,b) result(q)
   implicit none
   real*8,intent(in) :: a
   integer*8,intent(in) :: b
   real*8 :: q
   if (exponent(a)-exponent(real(b,4))>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*sign(1.0_8,a)*sign(1_8,b)
      end if
   else 
      q = a/b
   endif
end function savedivide_r8_by_i8

function savedivide_i4_by_r4(a,b) result(q)
   implicit none
   integer*4,intent(in) :: a
   real*4,intent(in) :: b
   real*4 :: q
   if (exponent(real(a,4))-exponent(b)>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*sign(1,a)*sign(1.0,b)
      end if
   else 
      q = a/b
   endif
end function savedivide_i4_by_r4

function savedivide_i4_by_r8(a,b) result(q)
   implicit none
   integer*4,intent(in) :: a
   real*8,intent(in) :: b
   real*8 :: q
   if (exponent(real(a,4))-exponent(b)>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*sign(1,a)*sign(1.0_8,b)
      end if
   else 
      q = a/b
   endif
end function savedivide_i4_by_r8

function savedivide_i4_by_i4(a,b) result(q)
   implicit none
   integer*4,intent(in) :: a
   integer*4,intent(in) :: b
   real*4 :: q
   if (exponent(real(a,4))-exponent(real(b,4))>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*sign(1,a)*sign(1,b)
      end if
   else 
      q = real(a,4)/b
   endif
end function savedivide_i4_by_i4

function savedivide_i4_by_i8(a,b) result(q)
   implicit none
   integer*4,intent(in) :: a
   integer*8,intent(in) :: b
   real*4 :: q
   if (exponent(real(a,4))-exponent(real(b,4))>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*real(sign(1,a),4)*real(sign(1_8,b),4)
      end if
   else 
      q = real(a,4)/real(b,4)
   endif
end function savedivide_i4_by_i8

function savedivide_i8_by_r4(a,b) result(q)
   implicit none
   integer*8,intent(in) :: a
   real*4,intent(in) :: b
   real*4 :: q
   if (exponent(real(a,4))-exponent(b)>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*real(sign(1_8,a),4)*sign(1.0,b)
      end if
   else 
      q = real(a,4)/b
   endif
end function savedivide_i8_by_r4

function savedivide_i8_by_r8(a,b) result(q)
   implicit none
   integer*8,intent(in) :: a
   real*8,intent(in) :: b
   real*8 :: q
   if (exponent(real(a,4))-exponent(b)>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*sign(1_8,a)*sign(1.0_8,b)
      end if
   else 
      q = a/b
   endif
end function savedivide_i8_by_r8

function savedivide_i8_by_i4(a,b) result(q)
   implicit none
   integer*8,intent(in) :: a
   integer*4,intent(in) :: b
   real*4 :: q
   if (exponent(real(a,4))-exponent(real(b,4))>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*real(sign(1_8,a),4)*real(sign(1,b),4)
      end if
   else 
      q = real(a,4)/b
   endif
end function savedivide_i8_by_i4

function savedivide_i8_by_i8(a,b) result(q)
   implicit none
   integer*8,intent(in) :: a
   integer*8,intent(in) :: b
   real*4 :: q
   if (exponent(real(a,4))-exponent(real(b,4))>=maxexponent(q) .or. b==0) then
      if (a==0) then
         q = 0
      else
         q = huge(q)*real(sign(1_8,a),4)*real(sign(1_8,b),4)
      end if
   else 
      q = real(a,4)/real(b,4)
   endif
end function savedivide_i8_by_i8

   
! random numbers *******************************************************************************************************************

subroutine set_seed(seed)

   implicit none
   integer*4,intent(in)    :: seed
   integer*4               :: rnd_size
   integer*4,allocatable   :: seed_array(:)
   
   ! set seed for PRNG of random_number(), Fortran 90 standard
   call random_seed(size=rnd_size)
   allocate(seed_array(rnd_size))
   seed_array = seed
   call random_seed(put=seed_array)
   
   ! set sed for PRNG of rand(), GNU Fortran 77 standard
   call srand(seed)
   
end subroutine set_seed

real*4 function get_normal_random_number(mu,sigma)

   implicit none
   real*4,intent(in) :: mu,sigma
   real*4,parameter  :: pi = 3.14159265359
   real*4            :: z,u1,u2
   
   call random_number(u1)
   call random_number(u2)
   z = sqrt(-2.0*log(1.0-u1))*cos(2*pi*u2)
   get_normal_random_number = mu+z*sigma
   
end function get_normal_random_number


! geometry *************************************************************************************************************************

subroutine sph2car(radius,longitude,lattitude,x,astro)

   ! convert spherical coordinates to cartesian coordinates
   ! by default the origin on the sphere (longitude=lattitude=0) is identified with the x-axis, and the poles lie on the z-axis
   ! if astro=.true., the origin on the sphere is identified with the z-axis, and the poles lie on the y-axis
   
   real*4,intent(in)             :: radius
   real*4,intent(in)             :: longitude ! [rad]
   real*4,intent(in)             :: lattitude ! [rad]
   real*4,intent(out)            :: x(3) ! coordinates x,y,z
   logical*4,intent(in),optional :: astro
   
   x = (/cos(longitude)*cos(lattitude), sin(longitude)*cos(lattitude), sin(lattitude)/)*radius
   
   if (present(astro)) then
      if (astro) x = (/x(2),x(3),x(1)/) ! from standard convention to astro convention
   end if
   
end subroutine sph2car

subroutine car2sph(x,radius,longitude,lattitude,astro)

   ! convert cartesian coordinates to spherical coordinates
   ! by default the origin on the sphere (longitude=lattitude=0) is identified with the x-axis, and the poles lie on the z-axis
   ! if astro=.true., the origin on the sphere is identified with the z-axis, and the poles lie on the y-axis

   real*4,intent(in)             :: x(3)  ! coordinates x,y,z
   real*4,intent(out)            :: radius
   real*4,intent(out)            :: longitude ! [rad]
   real*4,intent(out)            :: lattitude ! [rad]
   real*4                        :: x_(3)
   logical*4,intent(in),optional :: astro
   
   radius = sqrt(sum(x**2))
   if (radius<epsilon(radius)) then
      longitude = 0.0
      lattitude = 0.0
   else
      x_ = x
      if (present(astro)) then
         if (astro) x_ = (/x_(3),x_(1),x_(2)/) ! from astro convention to standard convention
      end if
      longitude = modulo(atan2(x_(2),x_(1)),6.2831853071795862319959)
      lattitude = asin(min(1.0,x_(3)/radius)) ! "min" to avoid numerical issues if radius very close to x(2)
   end if
   
end subroutine car2sph


! linear algebra (without LAPACK) **************************************************************************************************

recursive function determinant(mat,n) result(accum)
    integer :: n
    real    :: mat(n, n)
    real    :: submat(n-1, n-1), accum
    integer :: i, sgn
    if ( n == 1 ) then
        accum = mat(1,1)
    else
        accum = 0.0
        sgn = 1
        do i = 1, n
            submat( 1:n-1, 1:i-1 ) = mat( 2:n, 1:i-1 )
            submat( 1:n-1, i:n-1 ) = mat( 2:n, i+1:n )
            accum = accum + sgn * mat(1, i) * determinant( submat, n-1 )
            sgn = - sgn
        enddo
    endif
end function determinant

function rotation_matrix(axis,angle) result(R)

   implicit none
   real*4,intent(in)          :: axis(3)
   real*4,intent(in),optional :: angle    ! [rad]
   real*4                     :: R(3,3)
   real*4                     :: c,s,d,e(3),axis_norm,theta
   
   ! make rotation axis
   axis_norm = sqrt(sum(axis**2))
   e = axis/axis_norm
   
   ! make rotation angle
   if (present(angle)) then
      theta = angle
   else
      theta = axis_norm
   end if
   c = cos(theta)
   s = sin(theta)
   d = 1-c

   ! make rotation matrix
   R(1,1) = c+e(1)**2*d
   R(1,2) = e(1)*e(2)*d-e(3)*s
   R(1,3) = e(1)*e(3)*d+e(2)*s
   R(2,1) = e(2)*e(1)*d+e(3)*s
   R(2,2) = c+e(2)**2*d
   R(2,3) = e(2)*e(3)*d-e(1)*s
   R(3,1) = e(3)*e(1)*d-e(2)*s
   R(3,2) = e(3)*e(2)*d+e(1)*s
   R(3,3) = c+e(3)**2*d
   
end function rotation_matrix


! spectral analysis ****************************************************************************************************************

function spherical_harmonic(l,m,x) result(Y)

   ! returns the complex-valued spherical harmonics up to order l=4 as a function of the position on the sphere, specified by a 3D vector
    
   implicit none
   integer*4,intent(in) :: l,m
   real*4,intent(in)    :: x(3)
   real*4               :: radius
   complex*8            :: Y
   complex*8,parameter  :: i = (0.0,1.0)
   real*4               :: phi,theta
   real*4,parameter     :: pi = 3.14159265359
   
   Y = (0.0,0.0) ! default value
   
   radius = sqrt(sum(x**2))
   
   if (radius<epsilon(radius)) then
   
      return
      
   else
   
      phi = atan2(x(2),x(1))
      theta = acos(x(3)/radius)

      if (l==0) then ! monopole

         Y = 0.5/sqrt(pi)
      
      else if (l==1) then ! dipole
   
         select case(m)
            case(-1)
               Y = 0.5*sqrt(1.5/pi)*sin(theta)*exp(-1*i*phi)
            case(0)
               Y = 0.5*sqrt(3.0/pi)*cos(theta)
            case(1)
               Y = -0.5*sqrt(1.5/pi)*sin(theta)*exp(1*i*phi)
         end select
   
      else if (l==2) then ! quadrupole

         select case(m)
            case(-2)
               Y = 0.25*sqrt(7.5/pi)*sin(theta)**2*exp(-2*i*phi)
            case(-1)
               Y = 0.5*sqrt(7.5/pi)*sin(theta)*cos(theta)*exp(-1*i*phi)
            case(0)
               Y = 0.25*sqrt(5/pi)*(3*cos(theta)**2-1)
            case(1)
               Y = -0.5*sqrt(7.5/pi)*sin(theta)*cos(theta)*exp(1*i*phi)
            case(2)
               Y = 0.25*sqrt(7.5/pi)*sin(theta)**2*exp(2*i*phi)
         end select

      else if (l==3) then ! octupole

         select case(m)
          case(-3)
            Y = 0.125*sqrt(35.0/pi)*exp(-3*i*phi)*sin(theta)**3
          case(-2)
            Y = 0.25*sqrt(52.5/pi)*exp(-2*i*phi)*sin(theta)**2*cos(theta)
          case(-1)
            Y = 0.125*sqrt(21.0/pi)*exp(-1*i*phi)*sin(theta)*(5*cos(theta)**2-1)
          case(0)
            Y = 0.25*sqrt(7.0/pi)*(5*cos(theta)**3-3*cos(theta))
          case(1)
            Y = -0.125*sqrt(21.0/pi)*exp(1*i*phi)*sin(theta)*(5*cos(theta)**2-1)
          case(2)
            Y = 0.25*sqrt(52.5/pi)*exp(2*i*phi)*sin(theta)**2*cos(theta)
          case(3)
            Y = -0.125*sqrt(35.0/pi)*exp(3*i*phi)*sin(theta)**3
         end select

      else if (l==4) then ! hexadecapole

         select case(m)
          case(-4)
            Y = 0.1875*sqrt(17.5/pi)*exp(-4*i*phi)*sin(theta)**4
          case(-3)
            Y = 0.375*sqrt(35.0/pi)*exp(-3*i*phi)*sin(theta)**3*cos(theta)
          case(-2)
            Y = 0.375*sqrt(2.5/pi)*exp(-2*i*phi)*sin(theta)**2*(7*cos(theta)**2-1)
          case(-1)
            Y = 0.375*sqrt(5.0/pi)*exp(-1*i*phi)*sin(theta)*(7*cos(theta)**3-3*cos(theta))
          case(0)
            Y = 0.1875*sqrt(1.0/pi)*(35*cos(theta)**4-30*cos(theta)**2+3)
          case(1)
            Y = -0.375*sqrt(5.0/pi)*exp(1*i*phi)*sin(theta)*(7*cos(theta)**3-3*cos(theta))
          case(2)
            Y = 0.375*sqrt(2.5/pi)*exp(2*i*phi)*sin(theta)**2*(7*cos(theta)**2-1)
          case(3)
            Y = -0.375*sqrt(35.0/pi)*exp(3*i*phi)*sin(theta)**3*cos(theta)
          case(4)
            Y = 0.1875*sqrt(17.5/pi)*exp(4*i*phi)*sin(theta)**4
         end select
       
      end if
      
   end if
 
 end function spherical_harmonic

end module shared_module_maths