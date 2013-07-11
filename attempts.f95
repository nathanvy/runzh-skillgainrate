!basically this program will attempt to calculate the
! number of attempts required to grandmaster a given skill
! and compare it to the default runuo method which should
! be around 2700 attempts according to a random forum post

! I wrote this in about 45 minutes, expect it to be messy and ugly

program gainer
  implicit none

  integer :: attempts = 0
  
  !yes this ought to be a bool
  !but i'm using a real because i'm Country like that
  real :: success

  !fuck integers
  real, parameter :: totalcap = 700.0
  real, parameter :: skillcap = 120.0
  real, parameter :: failfactor = 0.2
  real, parameter :: rgainfactor = 1.0
  real, parameter :: sgainfactor = 0.09
  real, parameter :: a1 = 0.3
  real, parameter :: a2 = 0.2
  real, parameter :: a3 = 0.1
  real, parameter :: afactor = 70.0

  real :: total = 0.0
  real :: chance = 0.8
  real :: skill = 0.0

  !gain chances, rgc = runuo, sgc = cosine, pgc = polynomial
  real :: rgc
  real :: sgc
  real :: pgc

  write (*,*) 'RunZH gain rate attempt calculator v 0.3'
  write (*,*) 'Calculating runuo defaults assuming 80% chance of success...'

  do
     if ( chance >= rand(0) ) then
        success = 1.0
     else
        success = 0.0
     end if

     !uhh is this right?
     rgc = (totalcap - total) / totalcap / 4 
     rgc = rgc + (skillcap - skill) / skillcap / 4
     rgc = rgc + ((1.0 - chance) * (success + (1-success) * failfactor))
     rgc = rgc / 2
     rgc = rgc * rgainfactor

     if ( rgc >= rand(0) ) then
        skill = skill + 0.1
        total = total + 0.1
     end if
     attempts = attempts + 1

     if ( skill > 130.0 ) then
        write (*,*) 'GM reached...'
        exit
     end if
     
     if ( attempts > 1000000 ) then
        write (*,*) 'hit 1 mil attempts, wtf'
        exit
     end if
  end do

  write (*,*) 'Attempts: ', attempts
! end runuo

  write (*,*) 'Calculating cosine-experimentals assuming 80% chance of success...'

  skill = 0.0
  total = 0.0
  attempts = 0

  do
     if ( chance >= rand(0) ) then
        success = 1.0
     else
        success = 0.0
     end if

     sgc = (1-chance)*(0.5*cos(skill / 45.0)) + 0.5

     if ( sgc >= rand(0) ) then
        skill = skill + 0.1
     end if
     attempts = attempts + 1

     if ( skill > 130.0 ) then
        write (*,*) 'GM reached...'
        exit
     end if
     
     if ( attempts > 1000000 ) then
        write (*,*) 'hit 1 mil attempts, wtf'
        exit
     end if
  end do
  write (*,*) 'Attempts: ', attempts

! end cosine
  write (*,*) 'Calculating polynomial approximations assuming 80% chance of success...'
  skill = 0.0
  total = 0.0
  attempts = 0

  do
     if ( chance >= rand(0) ) then
        success = 1.0
     else
        success = 0.0
     end if

     pgc = ( 1 / (1 + (a1*(skill/afactor)) + (a2*(skill/afactor)**2) + (a3*(skill/afactor)**3))**4 )*(1-chance)

     if ( pgc >= rand(0) ) then
        skill = skill + 0.1
     end if
     attempts = attempts + 1

     if ( skill > 130.0 ) then
        write (*,*) 'GM reached...'
        exit
     end if
     
     if ( attempts > 1000000 ) then
        write (*,*) 'hit 1 mil attempts, wtf'
        exit
     end if
  end do
  write (*,*) 'Attempts: ', attempts
end program gainer
