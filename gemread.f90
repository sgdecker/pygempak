module gemread
  implicit none

contains

  subroutine ggi(gemFilename, mxgrd, n, nx, ny, proj, ang, lllat, lllon, urlat, urlon,&
       gdattm, level, ivcord, vcord, parm)
    character(*),  intent(in)  :: gemFilename
    integer,       intent(out) :: mxgrd, n, nx, ny
    character(20), intent(out) :: proj
    real,          intent(out) :: ang(3)
    real,          intent(out) :: lllat, lllon, urlat, urlon
    integer(1), intent(inout) :: gdattm(:,:,:)  ! First dim 20 chars
    integer,       intent(inout) :: level(:,:)
    integer,       intent(inout) :: ivcord(:)
    integer(1),  intent(inout) :: vcord(:,:)  ! First dim 4 chars
    integer(1), intent(inout) :: parm(:,:)  ! First dim 12 chars

    integer, parameter :: LLNNAV = 256, LLNANL = 128

    real :: bknav(LLNNAV), bkanl(LLNANL)
    integer :: status, iacss, navsz, ianlsz, ihdrsz, projtype, i
    character(20) :: firstm, lasttm

    character(20), allocatable :: gdattmloc(:,:)
    character(4), allocatable :: vcordloc(:)
    character(12), allocatable :: parmloc(:)

    integer :: c1, c2, j, k
    c1 = size(gdattm,2)
    c2 = size(gdattm,3)

    allocate(gdattmloc(c1,c2), vcordloc(c1), parmloc(c1))

    ! Initialize to bad values in case of error
    mxgrd = -1
    n = -1
    nx = -1
    ny = -1
    proj = "ERROR"
    ang = -9999.
    lllat = -9999.
    lllon = -9999.
    urlat = -9999.
    urlon = -9999.
    
    call in_bdta(status)
    if (status == 0) then
       call gd_init(status)
    else
       print *, 'Inexplicable error with in_bdta!'
       return
    end if

    if (status /= 0) then
       print *, 'Error initializing GEMPAK interface!'
       return
    end if

    call gd_ofil(gemFilename, .false., .false., iacss, navsz, bknav, ianlsz,  &
         bkanl, ihdrsz, mxgrd, status)
    if (status /= 0) then
       print *, 'Error opening GEMPAK file!'
       return
    end if

    call gd_ngrd(iacss, n, firstm, lasttm, status)
    if (status /= 0) then
       print *, 'Error getting number of grids!'
       return
    end if

    do i = 1, n
       call gd_gidn(iacss, i, gdattmloc(i,:), level(i,:), ivcord(i), parmloc(i), &
            status)
       if (status /= 0) then
          print *, 'Error reading grid info!'
          exit
       end if
       call lv_ccrd(ivcord(i), vcordloc(i), status)
       if (status /= 0) print *, 'Oopsie!'
    end do
    
    call gd_clos(iacss, status)
    if (status /= 0) print *, 'Error closing GEMPAK file!'

    call st_itoc(bknav(2), 1, proj, status)
    if (status /= 0) print *, 'Could not determine projection!'

    ! Grab the data we need
    nx = bknav(5)
    ny = bknav(6)
    projtype = bknav(1)
    select case (projtype)
    case (1)
       print *, 'Case 1'
       ang = 0.
    case (2)
       ang = [bknav(11), bknav(12), bknav(13)]
    case (3)
       print *, 'Case 3'
       ang = 0.
    case default
       print *, 'Unimplemented projection'
       return
    end select
    
    lllat = bknav(7)
    lllon = bknav(8)
    urlat = bknav(9)
    urlon = bknav(10)

    !print *, gdattmloc(:100,:), vcordloc(:100), parmloc(:100)
    do j = 1, c2
       do i = 1, c1
          do k = 1, 20
             gdattm(k,i,j) = iachar(gdattmloc(i,j)(k:k))
          end do
       end do
    end do
    do i = 1, c1
       do j = 1, 4
          vcord(j,i) = iachar(vcordloc(i)(j:j))
       end do
       do j = 1, 12
          parm(j,i) = iachar(parmloc(i)(j:j))
       end do
    end do
  end subroutine ggi

  subroutine get_grid_info(gemFilename, mxgrd, n, nx, ny, proj, ang, lllat,  &
       lllon, urlat, urlon, gdattm, level, ivcord, vcord, parm)
    character(*),  intent(in)    :: gemFilename
    integer,       intent(out)   :: mxgrd, n, nx, ny
    character(20), intent(out)   :: proj
    real,          intent(out)   :: ang(3)
    real,          intent(out)   :: lllat, lllon, urlat, urlon
    character(*), intent(inout) :: gdattm(:,:)
    integer,       intent(inout) :: level(:,:)
    integer,       intent(inout) :: ivcord(:)
    character(*),  intent(inout) :: vcord(:)
    character(*), intent(inout) :: parm(:)

    integer, parameter :: LLNNAV = 256, LLNANL = 128

    real :: bknav(LLNNAV), bkanl(LLNANL)
    integer :: status, iacss, navsz, ianlsz, ihdrsz, projtype, i
    character(20) :: firstm, lasttm

    ! Initialize to bad values in case of error
    mxgrd = -1
    n = -1
    nx = -1
    ny = -1
    proj = "ERROR"
    ang = -9999.
    lllat = -9999.
    lllon = -9999.
    urlat = -9999.
    urlon = -9999.
    
    call in_bdta(status)
    if (status == 0) then
       call gd_init(status)
    else
       print *, 'Inexplicable error with in_bdta!'
       return
    end if

    if (status /= 0) then
       print *, 'Error initializing GEMPAK interface!'
       return
    end if

    call gd_ofil(gemFilename, .false., .false., iacss, navsz, bknav, ianlsz,  &
         bkanl, ihdrsz, mxgrd, status)
    if (status /= 0) then
       print *, 'Error opening GEMPAK file!'
       return
    end if

    call gd_ngrd(iacss, n, firstm, lasttm, status)
    if (status /= 0) then
       print *, 'Error getting number of grids!'
       return
    end if

    do i = 1, n
       call gd_gidn(iacss, i, gdattm(i,:), level(i,:), ivcord(i), parm(i), &
            status)
       if (status /= 0) then
          print *, 'Error reading grid info!'
          exit
       end if
       call lv_ccrd(ivcord(i), vcord(i), status)
       if (status /= 0) print *, 'Oopsie!'
    end do
    
    call gd_clos(iacss, status)
    if (status /= 0) print *, 'Error closing GEMPAK file!'

    call st_itoc(bknav(2), 1, proj, status)
    if (status /= 0) print *, 'Could not determine projection!'

    ! Grab the data we need
    nx = bknav(5)
    ny = bknav(6)
    projtype = bknav(1)
    select case (projtype)
    case (1)
       print *, 'Case 1'
       ang = 0.
    case (2)
       ang = [bknav(11), bknav(12), bknav(13)]
    case (3)
       print *, 'Case 3'
       ang = 0.
    case default
       print *, 'Unimplemented projection'
       return
    end select
    
    lllat = bknav(7)
    lllon = bknav(8)
    urlat = bknav(9)
    urlon = bknav(10)
  end subroutine get_grid_info

  subroutine get_max_grids(gemFilename, mxgrd)
    character(*), intent(in) :: gemFilename
    integer, intent(out) :: mxgrd

    integer, parameter :: LLNNAV = 256, LLNANL = 128

    real :: bknav(LLNNAV), bkanl(LLNANL)
    integer :: status, iacss, navsz, ianlsz, ihdrsz

    call in_bdta(status)
    if (status == 0) call gd_init(status)
    if (status /= 0) then
       print *, 'Error initializing GEMPAK interface!'
       mxgrd = -1
       return
    end if

    call gd_ofil(gemFilename, .false., .false., iacss, navsz, bknav, ianlsz,  &
         bkanl, ihdrsz, mxgrd, status)
    if (status /= 0) then
       print *, 'Error opening GEMPAK file!'
       mxgrd = -1
       return
    end if

    call gd_clos(iacss, status)
    if (status /=0) print *, 'Error closing GEMPAK file!'
  end subroutine get_max_grids

  subroutine get_num_grids(gemFilename, n)
    character(*), intent(in) :: gemFilename
    integer, intent(out) :: n

    integer, parameter :: LLNNAV = 256, LLNANL = 128

    real :: bknav(LLNNAV), bkanl(LLNANL)
    integer :: status, iacss, navsz, ianlsz, ihdrsz, mxgrd
    character(20) :: firstm, lasttm

    call in_bdta(status)
    if (status == 0) call gd_init(status)
    if (status /= 0) then
       print *, 'Error initializing GEMPAK interface!'
       n = -1
       return
    end if

    call gd_ofil(gemFilename, .false., .false., iacss, navsz, bknav, ianlsz,  &
         bkanl, ihdrsz, mxgrd, status)
    if (status /= 0) then
       print *, 'Error opening GEMPAK file!'
       n = -1
       return
    end if

    call gd_ngrd(iacss, n, firstm, lasttm, status)
    if (status /= 0) then
       print *, 'Error getting number of grids!'
       n = -1
       return
    end if

    call gd_clos(iacss, status)
    if (status /=0) print *, 'Error closing GEMPAK file!'
  end subroutine get_num_grids

  subroutine get_grid_dims(gemFilename, kx, ky)
    character(*), intent(in) :: gemFilename
    integer, intent(out) :: kx, ky

    integer, parameter :: LLNNAV = 256, LLNANL = 128

    real :: bknav(LLNNAV), bkanl(LLNANL)
    integer :: status, iacss, navsz, ianlsz, ihdrsz, mxgrd
    character(4) :: proj

    kx = -1 ; ky = -1

    call in_bdta(status)
    if (status == 0) call gd_init(status)
    if (status /= 0) then
       print *, 'Error initializing GEMPAK interface!'
       return
    end if

    call gd_ofil(gemFilename, .false., .false., iacss, navsz, bknav, ianlsz,  &
         bkanl, ihdrsz, mxgrd, status)
    if (status /= 0) then
       print *, 'Error opening GEMPAK file!'
       return
    end if

    call gr_rnav(bknav, proj, kx, ky, status)
    if (status /= 0) print *, 'Error reading GEMPAK navigation block!'

    call gd_clos(iacss, status)
    if (status /=0) print *, 'Error closing GEMPAK file!'
  end subroutine get_grid_dims

  subroutine read_grid(gemFilename, gdattm1, gdattm2, level1, level2, vcord, &
       parmin, grid)
    character(*), intent(in) :: gemFilename, gdattm1, gdattm2, vcord, parmin
    integer, intent(in) :: level1, level2
    real, dimension(:,:), intent(inout) :: grid

    integer, parameter :: LLNNAV = 256, LLNANL = 128

    integer, dimension(:), allocatable :: ighdr

    real :: bknav(LLNNAV), bkanl(LLNANL)
    integer, dimension(2) :: level
    integer :: ivcord, status, iacss, navsz, ianlsz, ihdrsz, mxgrd, igx, igy
    character(20) :: gdattm(2)
    character(12) :: parm
    character(4) :: vcoord, junk

    grid = -999.

    gdattm = (/ gdattm1, gdattm2 /)
    
    level = (/ level1, level2 /)
    
    call st_lcuc(vcord, vcoord, status)
    call lv_cord(vcoord, junk, ivcord, status) 
    if (status /= 0) then
       print *, 'Error with vertical coordinate!'
       return
    end if

    parm = parmin

    call in_bdta(status)
    if (status == 0) call gd_init(status)
    if (status /= 0) then
       print *, 'Error initializing GEMPAK interface!'
       return
    end if

    call gd_ofil(gemFilename, .false., .false., iacss, navsz, bknav, ianlsz,  &
         bkanl, ihdrsz, mxgrd, status)
    if (status /= 0) then
       print *, 'Error opening GEMPAK file!'
       return
    end if

    allocate(ighdr(ihdrsz))

    call gd_rdat(iacss, gdattm, level, ivcord, parm, grid, igx, igy, ighdr,  &
         status)
    if (status /=0) print *, 'Error reading GEMPAK file!'

    call gd_clos(iacss, status)
    if (status /=0) print *, 'Error closing GEMPAK file!'
  end subroutine read_grid

  subroutine create_gemfile(newfname, copyfname, mxgrd, status)
    character(*), intent(in) :: newfname, copyfname
    integer, intent(in) :: mxgrd
    integer, intent(out) :: status

    integer, parameter :: LLNNAV = 256, LLNANL = 128

    real :: bknav(LLNNAV), bkanl(LLNANL)
    integer :: iacss, navsz, ianlsz, ihdrsz, origmxgrd
    logical :: exists

    status = -1

    inquire(file=newfname, exist=exists)
    if (exists) then
       print *, 'File to create already exists!'
       return
    end if
    inquire(file=copyfname, exist=exists)
    if (.not. exists) then
       print *, 'File to copy from does not exist!'
       return
    end if

    call in_bdta(status)
    if (status == 0) call gd_init(status)
    if (status /= 0) then
       print *, 'Error initializing GEMPAK interface!'
       return
    end if
    
    call gd_ofil(copyfname, .false., .false., iacss, navsz, bknav, ianlsz,  &
         bkanl, ihdrsz, origmxgrd, status)
    if (status /= 0) then
       print *, 'Error opening GEMPAK file!'
       return
    end if

    call gd_clos(iacss, status)
    if (status /= 0) print *, 'Error closing GEMPAK file!'

    call gd_cref(newfname, LLNNAV, bknav, LLNANL, bkanl, 2, mxgrd, iacss,  &
         status)
    if (status /= 0) then
       print *, 'Error creating GEMPAK file!'
       return
    end if

    call gd_clos(iacss, status)
    if (status /= 0) print *, 'Error closing GEMPAK file!'
  end subroutine create_gemfile

  subroutine write_grid(gemFilename, gdattm1, gdattm2, level1, level2, vcord, &
       parmin, grid, status)
    character(*), intent(in) :: gemFilename, gdattm1, gdattm2, vcord, parmin
    integer, intent(in) :: level1, level2
    real, dimension(:,:), intent(in) :: grid
    integer, intent(out) :: status

    integer, parameter :: LLNNAV = 256, LLNANL = 128

    integer, dimension(2) :: ighdr = 0

    real :: bknav(LLNNAV), bkanl(LLNANL)
    integer, dimension(2) :: level
    integer :: ivcord, iacss, navsz, ianlsz, ihdrsz, mxgrd, igx, igy
    character(20) :: gdattm(2)
    character(12) :: parm
    character(4) :: vcoord, junk

    igx = size(grid, 1)
    igy = size(grid, 2)

    gdattm = (/ gdattm1, gdattm2 /)
    
    level = (/ level1, level2 /)
    
    call st_lcuc(vcord, vcoord, status)
    call lv_cord(vcoord, junk, ivcord, status) 
    if (status /= 0) then
       print *, 'Error with vertical coordinate!'
       return
    end if

    parm = parmin

    call in_bdta(status)
    if (status == 0) call gd_init(status)
    if (status /= 0) then
       print *, 'Error initializing GEMPAK interface!'
       return
    end if

    call gd_ofil(gemFilename, .true., .false., iacss, navsz, bknav, ianlsz,  &
         bkanl, ihdrsz, mxgrd, status)
    if (status /= 0) then
       print *, 'Error opening GEMPAK file!'
       return
    end if

    call gd_wdat(iacss, grid, igx, igy, ighdr, gdattm, level, ivcord, parm,  &
         .false., status)
    if (status /= 0) then
       print *, 'Error writing GEMPAK file!'
       print *, status
    end if

    call gd_clos(iacss, status)
    if (status /= 0) print *, 'Error closing GEMPAK file!'
  end subroutine write_grid
end module gemread

program test
  use gemread
  implicit none

  character(*), parameter ::  &
       fname = '/ldmdata/gempak/model/nam/14032812_nam212.gem'

  real, dimension(:,:), allocatable :: grid
  integer, dimension(:,:), allocatable :: level
  character(20), dimension(:,:), allocatable :: gdattm
  integer, dimension(:), allocatable :: ivcord
  character(4), dimension(:), allocatable :: vcord
  character(12), dimension(:), allocatable :: parm
  real, dimension(3) :: ang
  real :: lllat, lllon, urlat, urlon
  integer :: i, n, nx, ny
  character(20) :: proj

  call get_num_grids(fname, n)
  allocate(gdattm(n,2), level(n,2), ivcord(n), vcord(n), parm(n))
  call get_grid_info(fname, i, n, nx, ny, proj, ang, lllat, lllon, urlat, &
       urlon, gdattm, level, ivcord, vcord, parm)
!  call get_max_grids(fname, i)
  print *, i
!  call get_num_grids(fname, n)
  print *, n
!  call get_grid_dims(fname, nx, ny)
  print *, nx, ny
  print *, proj, ang, lllat, lllon, urlat, urlon
  print *, gdattm(:100,:)
  print *, level(:100,:)
  print *, ivcord(:100)
  print *, vcord(:100)
  print *, parm(:100)
  allocate(grid(nx,ny))
  call read_grid(fname, '140328/1200F000', '', 850, -1, 'PRES', 'HGHT', grid)
  print *, grid(10,20)
  call create_gemfile('newfile.gem', fname, 1000, i)
  print *, i
  call write_grid('newfile.gem', '121025/0000F000', '', 850, -1, 'PRES', &
       'HGHT', grid, i)
  print *, i
end program test
