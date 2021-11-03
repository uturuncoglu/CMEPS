module shr_flux_mod

  use shr_kind_mod    ! shared kinds
  use shr_const_mod   ! shared constants
  use shr_sys_mod     ! shared system routines
  use shr_log_mod, only: s_logunit => shr_log_Unit
#ifdef UFS_AOFLUX
  use machine,  only: kp => kind_phys
#endif

  implicit none

  private ! default private

  ! !PUBLIC MEMBER FUNCTIONS:

  public :: shr_flux_atmOcn           ! computes atm/ocn fluxes
#ifdef UFS_AOFLUX
  public :: shr_flux_atmOcn_ufs       ! computes atm/ocn fluxes consistent with UFS
#endif
  public :: shr_flux_adjust_constants ! adjust constant values used in flux calculations.

  !--- rename kinds for local readability only ---
  integer,parameter :: R8 = SHR_KIND_R8  ! 8 byte real
  integer,parameter :: IN = SHR_KIND_IN  ! native/default integer

  ! The follow variables are not declared as parameters so that they can be
  ! adjusted to support aquaplanet and potentially other simple model modes.
  ! The shr_flux_adjust_constants subroutine is called to set the desired
  ! values.  The default values are from shr_const_mod.  Currently they are
  ! only used by the shr_flux_atmocn and shr_flux_atmice routines.
  real(R8) :: loc_zvir   = shr_const_zvir
  real(R8) :: loc_cpdair = shr_const_cpdair
  real(R8) :: loc_cpvir  = shr_const_cpvir
  real(R8) :: loc_karman = shr_const_karman
  real(R8) :: loc_g      = shr_const_g
  real(R8) :: loc_latvap = shr_const_latvap
  real(R8) :: loc_latice = shr_const_latice
  real(R8) :: loc_stebol = shr_const_stebol
  real(R8) :: loc_tkfrz  = shr_const_tkfrz

  ! These control convergence of the iterative flux calculation
  ! (For Large and Pond scheme only; not UA or COARE).
  real(r8)    :: flux_con_tol = 0.0_R8
  integer(IN) :: flux_con_max_iter = 2

  character(len=*), parameter :: sourcefile = &
       __FILE__

  !--- cold air outbreak parameters  (Mahrt & Sun 1995,MWR) -------------
  logical :: use_coldair_outbreak_mod = .false.
  real(R8),parameter    :: alpha = 1.4_R8
  real(R8),parameter    :: maxscl =2._R8  ! maximum wind scaling for flux
  real(R8),parameter    :: td0 = -10._R8   ! start t-ts for scaling

#ifdef UFS_AOFLUX
  real(kp), allocatable, dimension(:) :: z0rl        , z0rl_wav  ,            &
                                         z0rl_wat    , z0rl_lnd  , z0rl_ice  !, &
  !                                       tskin       ,                         &
  !                                       tskin_wat   , tskin_lnd , tskin_ice , &
  !                                       fm_wat      , fm_lnd    , fm_ice    , &
  !                                       fh_wat      , fh_lnd    , fh_ice    , &
  !                                       fm10_wat    , fm10_lnd  , fm10_ice  !, &
  !                                       ustar       , ustar_wat , ustar_lnd , &
  !                                       ustar_ice
#endif

!===============================================================================
contains
!===============================================================================

  subroutine shr_flux_adjust_constants( &
       zvir, cpair, cpvir, karman, gravit, &
       latvap, latice, stebol, flux_convergence_tolerance, &
       flux_convergence_max_iteration, &
       coldair_outbreak_mod)

    ! Adjust local constants.  Used to support simple models.

    real(R8), optional, intent(in) :: zvir
    real(R8), optional, intent(in) :: cpair
    real(R8), optional, intent(in) :: cpvir
    real(R8), optional, intent(in) :: karman
    real(R8), optional, intent(in) :: gravit
    real(R8), optional, intent(in) :: latvap
    real(R8), optional, intent(in) :: latice
    real(R8), optional, intent(in) :: stebol
    real(r8), optional, intent(in)  :: flux_convergence_tolerance
    integer(in), optional, intent(in) :: flux_convergence_max_iteration
    logical, optional, intent(in) :: coldair_outbreak_mod
    !----------------------------------------------------------------------------

    if (present(zvir))   loc_zvir   = zvir
    if (present(cpair))  loc_cpdair = cpair
    if (present(cpvir))  loc_cpvir  = cpvir
    if (present(karman)) loc_karman = karman
    if (present(gravit)) loc_g      = gravit
    if (present(latvap)) loc_latvap = latvap
    if (present(latice)) loc_latice = latice
    if (present(stebol)) loc_stebol = stebol
    if (present(flux_convergence_tolerance)) flux_con_tol = flux_convergence_tolerance
    if (present(flux_convergence_max_iteration)) flux_con_max_iter = flux_convergence_max_iteration
    if(present(coldair_outbreak_mod)) use_coldair_outbreak_mod = coldair_outbreak_mod
  end subroutine shr_flux_adjust_constants

  !===============================================================================
  subroutine shr_flux_atmOcn(nMax  ,zbot  ,ubot  ,vbot  ,thbot ,   &
       &               qbot  ,s16O  ,sHDO  ,s18O  ,rbot  ,   &
       &               tbot  ,us    ,vs    ,   &
       &               ts    ,mask  ,seq_flux_atmocn_minwind, &
       &               sen   ,lat   ,lwup  ,   &
       &               r16O, rhdo, r18O, &
       &               evap  ,evap_16O, evap_HDO, evap_18O, &
       &               taux  ,tauy  ,tref  ,qref  ,   &
       &               ocn_surface_flux_scheme, &
       &               duu10n,  ustar_sv   ,re_sv ,ssq_sv,   &
       &               missval    )

    implicit none

    !--- input arguments --------------------------------
    integer(IN),intent(in) ::       nMax  ! data vector length
    integer(IN),intent(in) :: mask (nMax) ! ocn domain mask       0 <=> out of domain
    real(R8)   ,intent(in) :: zbot (nMax) ! atm level height                     (m)
    real(R8)   ,intent(in) :: ubot (nMax) ! atm u wind (bottom or 10m)           (m/s)
    real(R8)   ,intent(in) :: vbot (nMax) ! atm v wind (bottom or 10m)           (m/s)
    real(R8)   ,intent(in) :: thbot(nMax) ! atm potential T                      (K)
    real(R8)   ,intent(in) :: qbot (nMax) ! atm specific humidity (bottom or 2m) (kg/kg)
    real(R8)   ,intent(in) :: s16O (nMax) ! atm H216O tracer conc.               (kg/kg)
    real(R8)   ,intent(in) :: sHDO (nMax) ! atm HDO tracer conc.                 (kg/kg)
    real(R8)   ,intent(in) :: s18O (nMax) ! atm H218O tracer conc.               (kg/kg)
    real(R8)   ,intent(in) :: r16O (nMax) ! ocn H216O tracer ratio/Rstd
    real(R8)   ,intent(in) :: rHDO (nMax) ! ocn HDO tracer ratio/Rstd
    real(R8)   ,intent(in) :: r18O (nMax) ! ocn H218O tracer ratio/Rstd
    real(R8)   ,intent(in) :: rbot (nMax) ! atm air density                      (kg/m^3)
    real(R8)   ,intent(in) :: tbot (nMax) ! atm T (bottom or 2m)                 (K)
    real(R8)   ,intent(in) :: us   (nMax) ! ocn u-velocity                       (m/s)
    real(R8)   ,intent(in) :: vs   (nMax) ! ocn v-velocity                       (m/s)
    real(R8)   ,intent(in) :: ts   (nMax) ! ocn temperature                      (K)
    integer(IN),intent(in), optional :: ocn_surface_flux_scheme
    real(R8)   ,intent(in), optional :: seq_flux_atmocn_minwind ! minimum wind speed for atmocn (m/s)

    !--- output arguments -------------------------------
    real(R8),intent(out)  ::  sen  (nMax) ! heat flux: sensible    (W/m^2)
    real(R8),intent(out)  ::  lat  (nMax) ! heat flux: latent      (W/m^2)
    real(R8),intent(out)  ::  lwup (nMax) ! heat flux: lw upward   (W/m^2)
    real(R8),intent(out)  ::  evap (nMax) ! water flux: evap  ((kg/s)/m^2)
    real(R8),intent(out)  ::  evap_16O (nMax) ! water flux: evap ((kg/s/m^2)
    real(R8),intent(out)  ::  evap_HDO (nMax) ! water flux: evap ((kg/s)/m^2)
    real(R8),intent(out)  ::  evap_18O (nMax) ! water flux: evap ((kg/s/m^2)
    real(R8),intent(out)  ::  taux (nMax) ! surface stress, zonal      (N)
    real(R8),intent(out)  ::  tauy (nMax) ! surface stress, maridional (N)
    real(R8),intent(out)  ::  tref (nMax) ! diag:  2m ref height T     (K)
    real(R8),intent(out)  ::  qref (nMax) ! diag:  2m ref humidity (kg/kg)
    real(R8),intent(out)  :: duu10n(nMax) ! diag: 10m wind speed squared (m/s)^2

    real(R8),intent(out),optional :: ustar_sv(nMax) ! diag: ustar
    real(R8),intent(out),optional :: re_sv   (nMax) ! diag: sqrt of exchange coefficient (water)
    real(R8),intent(out),optional :: ssq_sv  (nMax) ! diag: sea surface humidity  (kg/kg)

    real(R8),intent(in) ,optional :: missval        ! masked value

    ! !EOP

    !--- local constants --------------------------------
    real(R8),parameter :: umin  =  0.5_R8 ! minimum wind speed       (m/s)
    real(R8),parameter :: zref  = 10.0_R8 ! reference height           (m)
    real(R8),parameter :: ztref =  2.0_R8 ! reference height for air T (m)
    !!++ Large only
    !real(R8),parameter :: cexcd  = 0.0346_R8 ! ratio Ch(water)/CD
    !real(R8),parameter :: chxcds = 0.018_R8  ! ratio Ch(heat)/CD for stable case
    !real(R8),parameter :: chxcdu = 0.0327_R8 ! ratio Ch(heat)/CD for unstable case
    !!++ COARE only
    real(R8),parameter :: zpbl =700.0_R8 ! PBL depth [m] for gustiness parametriz.

    !--- local variables --------------------------------
    integer(IN) :: n      ! vector loop index
    integer(IN) :: iter
    real(R8)    :: vmag   ! surface wind magnitude   (m/s)
    real(R8)    :: ssq    ! sea surface humidity     (kg/kg)
    real(R8)    :: delt   ! potential T difference   (K)
    real(R8)    :: delq   ! humidity difference      (kg/kg)
    real(R8)    :: stable ! stability factor
    real(R8)    :: rdn    ! sqrt of neutral exchange coeff (momentum)
    real(R8)    :: rhn    ! sqrt of neutral exchange coeff (heat)
    real(R8)    :: ren    ! sqrt of neutral exchange coeff (water)
    real(R8)    :: rd     ! sqrt of exchange coefficient (momentum)
    real(R8)    :: rh     ! sqrt of exchange coefficient (heat)
    real(R8)    :: re     ! sqrt of exchange coefficient (water)
    real(R8)    :: ustar  ! ustar
    real(r8)     :: ustar_prev
    real(R8)    :: qstar  ! qstar
    real(R8)    :: tstar  ! tstar
    real(R8)    :: hol    ! H (at zbot) over L
    real(R8)    :: xsq    ! ?
    real(R8)    :: xqq    ! ?
    !!++ Large only
    real(R8)    :: psimh  ! stability function at zbot (momentum)
    real(R8)    :: psixh  ! stability function at zbot (heat and water)
    real(R8)    :: psix2  ! stability function at ztref reference height
    real(R8)    :: alz    ! ln(zbot/zref)
    real(R8)    :: al2    ! ln(zref/ztref)
    real(R8)    :: u10n   ! 10m neutral wind
    real(R8)    :: tau    ! stress at zbot
    real(R8)    :: cp     ! specific heat of moist air
    real(R8)    :: fac    ! vertical interpolation factor
    real(R8)    :: spval  ! local missing value
    !!++ COARE only
    real(R8)    :: zo,zot,zoq      ! roughness lengths
    real(R8)    :: hsb,hlb         ! sens & lat heat flxs at zbot
    real(R8) :: trf,qrf,urf,vrf ! reference-height quantities

    !--- local functions --------------------------------
    real(R8)    :: qsat   ! function: the saturation humididty of air (kg/m^3)
    !!++ Large only (formula v*=[c4/U10+c5+c6*U10]*U10 in Large et al. 1994)
    real(R8)    :: cdn    ! function: neutral drag coeff at 10m
    !!++ Large only (stability functions)
    real(R8)    :: psimhu ! function: unstable part of psimh
    real(R8)    :: psixhu ! function: unstable part of psimx
    real(R8)    :: Umps   ! dummy arg ~ wind velocity (m/s)
    real(R8)    :: Tk     ! dummy arg ~ temperature (K)
    real(R8)    :: xd     ! dummy arg ~ ?
    !--- for cold air outbreak calc --------------------------------
    real(R8)    :: tdiff(nMax)               ! tbot - ts
    real(R8)    :: vscl

    qsat(Tk)   = 640380.0_R8 / exp(5107.4_R8/Tk)
    cdn(Umps)  =   0.0027_R8 / Umps + 0.000142_R8 + 0.0000764_R8 * Umps
    psimhu(xd) = log((1.0_R8+xd*(2.0_R8+xd))*(1.0_R8+xd*xd)/8.0_R8) - 2.0_R8*atan(xd) + 1.571_R8
    psixhu(xd) = 2.0_R8 * log((1.0_R8 + xd*xd)/2.0_R8)

    !--- formats ----------------------------------------
    character(*),parameter :: subName = '(shr_flux_atmOcn) '
    character(*),parameter ::   F00 = "('(shr_flux_atmOcn) ',4a)"

    !-------------------------------------------------------------------------------
    ! PURPOSE:
    !   computes atm/ocn surface fluxes
    !
    ! NOTES:
    !   o all fluxes are positive downward
    !   o net heat flux = net sw + lw up + lw down + sen + lat
    !   o here, tstar = <WT>/U*, and qstar = <WQ>/U*.
    !   o wind speeds should all be above a minimum speed (eg. 1.0 m/s)
    !
    ! ASSUMPTIONS:
    !  Large:
    !   o Neutral 10m drag coeff: cdn = .0027/U10 + .000142 + .0000764 U10
    !   o Neutral 10m stanton number: ctn = .0327 sqrt(cdn), unstable
    !                                 ctn = .0180 sqrt(cdn), stable
    !   o Neutral 10m dalton number:  cen = .0346 sqrt(cdn)
    !   o The saturation humidity of air at T(K): qsat(T)  (kg/m^3)
    !  COARE:
    !   o use COAREv3.0 function (tht 22/11/2013)
    !-------------------------------------------------------------------------------

    if (present(missval)) then
       spval = missval
    else
       spval = shr_const_spval
    endif
    u10n = spval
    rh = spval
    psixh = spval
    hol=spval

    !--- for cold air outbreak calc --------------------------------
    tdiff= tbot - ts

    al2 = log(zref/ztref)
    DO n=1,nMax
       if (mask(n) /= 0) then

          !--- compute some needed quantities ---
          vmag   = max(umin, sqrt( (ubot(n)-us(n))**2 + (vbot(n)-vs(n))**2) )
          if (use_coldair_outbreak_mod) then
             ! Cold Air Outbreak Modification:
             ! Increase windspeed for negative tbot-ts
             ! based on Mahrt & Sun 1995,MWR

             if (tdiff(n).lt.td0) then
                vscl=min((1._R8+alpha*(abs(tdiff(n)-td0)**0.5_R8/abs(vmag))),maxscl)
                vmag=vmag*vscl
             endif
          endif
          ssq    = 0.98_R8 * qsat(ts(n)) / rbot(n)   ! sea surf hum (kg/kg)
          delt   = thbot(n) - ts(n)                  ! pot temp diff (K)
          delq   = qbot(n) - ssq                     ! spec hum dif (kg/kg)
          alz    = log(zbot(n)/zref)
          cp     = loc_cpdair*(1.0_R8 + loc_cpvir*ssq)

          !------------------------------------------------------------
          ! first estimate of Z/L and ustar, tstar and qstar
          !------------------------------------------------------------
          !--- neutral coefficients, z/L = 0.0 ---
          stable = 0.5_R8 + sign(0.5_R8 , delt)
          rdn    = sqrt(cdn(vmag))
          rhn    = (1.0_R8-stable) * 0.0327_R8 + stable * 0.018_R8
          !(1.0_R8-stable) * chxcdu + stable * chxcds
          ren    = 0.0346_R8 !cexcd

          !--- ustar, tstar, qstar ---
          ustar = rdn * vmag
          tstar = rhn * delt
          qstar = ren * delq
          ustar_prev = ustar*2.0_R8
          iter = 0
          do while( abs((ustar - ustar_prev)/ustar) > flux_con_tol .and. iter < flux_con_max_iter)
             iter = iter + 1
             ustar_prev = ustar
             !--- compute stability & evaluate all stability functions ---
             hol  = loc_karman*loc_g*zbot(n)*  &
                  (tstar/thbot(n)+qstar/(1.0_R8/loc_zvir+qbot(n)))/ustar**2
             hol  = sign( min(abs(hol),10.0_R8), hol )
             stable = 0.5_R8 + sign(0.5_R8 , hol)
             xsq    = max(sqrt(abs(1.0_R8 - 16.0_R8*hol)) , 1.0_R8)
             xqq    = sqrt(xsq)
             psimh  = -5.0_R8*hol*stable + (1.0_R8-stable)*psimhu(xqq)
             psixh  = -5.0_R8*hol*stable + (1.0_R8-stable)*psixhu(xqq)

             !--- shift wind speed using old coefficient ---
             rd   = rdn / (1.0_R8 + rdn/loc_karman*(alz-psimh))
             if (ocn_surface_flux_scheme == -1)then
                u10n = vmag
             else
                u10n = vmag * rd / rdn
             end if

             !--- update transfer coeffs at 10m and neutral stability ---
             rdn = sqrt(cdn(u10n))
             ren = 0.0346_R8 !cexcd
             rhn = (1.0_R8-stable)*0.0327_R8 + stable * 0.018_R8
             !(1.0_R8-stable) * chxcdu + stable * chxcds

             !--- shift all coeffs to measurement height and stability ---
             if (ocn_surface_flux_scheme == -1)then
               rd = rdn
             else
               rd = rdn / (1.0_R8 + rdn/loc_karman*(alz-psimh))
             end if
             rh = rhn / (1.0_R8 + rhn/loc_karman*(alz-psixh))
             re = ren / (1.0_R8 + ren/loc_karman*(alz-psixh))

             !--- update ustar, tstar, qstar using updated, shifted coeffs --
             ustar = rd * vmag
             tstar = rh * delt
             qstar = re * delq
          enddo
          if (iter < 1) then
             write(s_logunit,*) ustar,ustar_prev,flux_con_tol,flux_con_max_iter
             call shr_sys_abort('shr_flux_mod: No iterations performed ')
          end if
          !------------------------------------------------------------
          ! compute the fluxes
          !------------------------------------------------------------

          tau = rbot(n) * ustar * ustar

          !--- momentum flux ---
          taux(n) = tau * (ubot(n)-us(n)) / vmag
          tauy(n) = tau * (vbot(n)-vs(n)) / vmag

          !--- heat flux ---
          sen (n) =          cp * tau * tstar / ustar
          lat (n) =  loc_latvap * tau * qstar / ustar
          lwup(n) = -loc_stebol * ts(n)**4

          !--- water flux ---
          evap(n) = lat(n)/loc_latvap

          !------------------------------------------------------------
          ! compute diagnositcs: 2m ref T & Q, 10m wind speed squared
          !------------------------------------------------------------
          hol = hol*ztref/zbot(n)
          xsq = max( 1.0_R8, sqrt(abs(1.0_R8-16.0_R8*hol)) )
          xqq = sqrt(xsq)
          psix2   = -5.0_R8*hol*stable + (1.0_R8-stable)*psixhu(xqq)
          fac     = (rh/loc_karman) * (alz + al2 - psixh + psix2 )
          tref(n) = thbot(n) - delt*fac
          tref(n) = tref(n) - 0.01_R8*ztref   ! pot temp to temp correction
          fac     = (re/loc_karman) * (alz + al2 - psixh + psix2 )
          qref(n) =  qbot(n) - delq*fac

          duu10n(n) = u10n*u10n ! 10m wind speed squared

          !------------------------------------------------------------
          ! optional diagnostics, needed for water tracer fluxes (dcn)
          !------------------------------------------------------------
          if (present(ustar_sv)) ustar_sv(n) = ustar
          if (present(re_sv   )) re_sv(n)    = re
          if (present(ssq_sv  )) ssq_sv(n)   = ssq

       else
          !------------------------------------------------------------
          ! no valid data here -- out of domain
          !------------------------------------------------------------
          sen   (n) = spval  ! sensible         heat flux  (W/m^2)
          lat   (n) = spval  ! latent           heat flux  (W/m^2)
          lwup  (n) = spval  ! long-wave upward heat flux  (W/m^2)
          evap  (n) = spval  ! evaporative water flux ((kg/s)/m^2)
          evap_16O (n) = spval !water tracer flux (kg/s)/m^2)
          evap_HDO (n) = spval !HDO tracer flux  (kg/s)/m^2)
          evap_18O (n) = spval !H218O tracer flux (kg/s)/m^2)
          taux  (n) = spval  ! x surface stress (N)
          tauy  (n) = spval  ! y surface stress (N)
          tref  (n) = spval  !  2m reference height temperature (K)
          qref  (n) = spval  !  2m reference height humidity (kg/kg)
          duu10n(n) = spval  ! 10m wind speed squared (m/s)^2

          if (present(ustar_sv)) ustar_sv(n) = spval
          if (present(re_sv   )) re_sv   (n) = spval
          if (present(ssq_sv  )) ssq_sv  (n) = spval
       endif
    end DO

  end subroutine shr_flux_atmOcn

#ifdef UFS_AOFLUX
  !===============================================================================
  subroutine shr_flux_atmOcn_ufs(nMax, mask, psfc, pbot, tbot, qbot, zbot, &
             garea, ubot, usfc, vbot, vsfc, rbot, ts, sen, lat, taux, tauy, missval)

    !-----------------------------------------------------------------------
    ! ???
    !-----------------------------------------------------------------------

    use funcphys, only: gpvs, fpvs, fpvsx
    use physcons, only: eps => con_eps
    use physcons, only: epsm1 => con_epsm1
    use physcons, only: grav => con_g
    use physcons, only: rvrdm1 => con_fvirt
    use physcons, only: cappa => con_rocp
    use physcons, only: hvap => con_hvap
    use physcons, only: cp => con_cp
    use physcons, only: rd => con_rd
    use physcons, only: rv => con_rv
    use physcons, only: hfus => con_hfus
    use physcons, only: p0 => con_p0
    use physcons, only: tice => con_tice
    use sfc_diff, only: sfc_diff_run
    use sfc_ocean, only: sfc_ocean_run
    use GFS_surface_composites_pre, only: GFS_surface_composites_pre_run
    use GFS_surface_composites_post, only: GFS_surface_composites_post_run
    use GFS_surface_loop_control_part1, only: GFS_surface_loop_control_part1_run
    use GFS_surface_loop_control_part2, only: GFS_surface_loop_control_part2_run

    implicit none

    !--- input arguments --------------------------------
    integer(IN), intent(in)  :: nMax        ! data vector length
    integer(IN), intent(in)  :: mask (nMax) ! ocn domain mask
    real(R8)   , intent(in)  :: psfc(nMax)  ! atm P (surface)                (Pa)
    real(R8)   , intent(in)  :: pbot(nMax)  ! atm P (bottom)                 (Pa)
    real(R8)   , intent(in)  :: tbot(nMax)  ! atm T (bottom)                 (K)
    real(R8)   , intent(in)  :: qbot(nMax)  ! atm specific humidity (bottom) (kg/kg)
    real(R8)   , intent(in)  :: zbot(nMax)  ! atm level height               (m)
    real(R8)   , intent(in)  :: garea(nMax) ! grid area                      (m^2)
    real(R8)   , intent(in)  :: ubot(nMax)  ! atm u wind (bottom)            (m/s)
    real(R8)   , intent(in)  :: usfc(nMax)  ! atm u wind (surface)           (m/s)
    real(R8)   , intent(in)  :: vbot(nMax)  ! atm v wind (bottom)            (m/s)    
    real(R8)   , intent(in)  :: vsfc(nMax)  ! atm v wind (surface)           (m/s)    
    real(R8)   , intent(in)  :: rbot(nMax)  ! atm density                    (kg/m^3)    
    real(R8)   , intent(in)  :: ts(nMax)    ! ocn surface temperature        (K)
    real(R8)   , intent(in), optional :: missval ! masked value

    !--- output arguments -------------------------------
    real(R8)   , intent(out) :: sen(nMax)   ! heat flux: sensible            (W/m^2)
    real(R8)   , intent(out) :: lat(nMax)   ! heat flux: latent              (W/m^2)
    real(R8)   , intent(out) :: taux(nMax)  ! surface stress, zonal          (N)
    real(R8)   , intent(out) :: tauy(nMax)  ! surface stress, maridional     (N)

    !--- local variables --------------------------------
    integer                   :: n           , iter      , ivegsrc   , &
                                 sfc_z0_type , errflg    , nstf_name1, &
                                 lkm         , nthreads  , levs      , &
                                 isot        , kice      , km 
    real(kp)                  :: spval       , cpinv     , hvapi     , &
                                 elocp       , rch       , tem       , &
                                 min_lakeice , min_seaice, tgice     , &
                                 h0facu      , h0facs
    logical                   :: redrag      , thsfc_loc , lseaspray , &
                                 flag_restart, frac_grid , cplflx    , &
                                 cplice      , cplwav2atm, lheatstrg
    character(len=1024)       :: errmsg
    integer, dimension(nMax)  :: vegtype     , islmsk    , stype     , &
                                 islmsk_cice , vtype     , slope
    real(kp), dimension(nMax) :: prsl1       , prslki    , prsik1    , &
                                 prslk1      , wind      , sigmaf    , &
                                 shdmax      , z0pert    , ztpert    , &
                                 tsurf_wat   , tsurf_lnd , tsurf_ice , &
                                 zvfun       , cm        , cm_wat    , &
                                 cm_lnd      , cm_ice    , ch        , &
                                 ch_wat      , ch_lnd    , ch_ice    , &
                                 rb          , rb_wat    , rb_lnd    , &
                                 rb_ice      , stress    ,             &
                                 stress_wat  , stress_lnd, stress_ice, &
                                 ztmax_wat   , ztmax_lnd , ztmax_ice , &
                                 landfrac    , lakefrac  , lakedepth , &
                                 oceanfrac   , frland    , hice      , &
                                 cice        , snowd     , snowd_lnd , &
                                 snowd_ice   , tprcp     , tprcp_wat , &
                                 tprcp_lnd   , tprcp_ice , weasd     , &
                                 weasd_lnd   , weasd_ice , hflxq     , &
                                 tsfco       , tsfcl     , tisfc     , &
                                 slmsk       , hffac     , &
                                 qss         , qss_wat   , qss_lnd   , &
                                 qss_ice     , vfrac     ,             &
                                 tskin       ,                         &
                                 tskin_wat   , tskin_lnd , tskin_ice , &
                                 ustar       ,                         &
                                 ustar_wat   , ustar_lnd , ustar_ice , &
                                 fm          ,                         &
                                 fm_wat      , fm_lnd    , fm_ice    , &
                                 fh          ,                         &
                                 fh_wat      , fh_lnd    , fh_ice    , &
                                 fm10        ,                         &
                                 fm10_wat    , fm10_lnd  , fm10_ice  , &
                                 fh2         ,                         & 
                                 fh2_wat     , fh2_lnd   , fh2_ice   , &
                                 cmm         ,                         &
                                 cmm_wat     , cmm_lnd   , cmm_ice   , &
                                 chh         ,                         &
                                 chh_wat     , chh_lnd   , chh_ice   , &
                                 gflx        ,                         &
                                 gflx_wat    , gflx_lnd  , gflx_ice  , &
                                 ep1d        ,                         &
                                 ep1d_wat    , ep1d_lnd  , ep1d_ice  , &
                                 evap        ,                         &
                                 evap_wat    , evap_lnd  , evap_ice  , &
                                 hflx        ,                         &
                                 hflx_wat    , hflx_lnd  , hflx_ice  , &
                                 tsfc        ,                         &
                                 tsfc_wat    , tsfc_lnd  , tsfc_ice
    real(kp), dimension(nMax,1) :: tiice
    real(kp), dimension(nMax,1) :: stc
    logical, dimension(nMax)  :: flag_iter   , flag_guess, use_flake , &
                                 wet         , dry       , icy       , &
                                 flag_cice   , lake

    !--- local variables that are carried out -----------
    logical, save             :: flag_init = .true.
    integer, save             :: kdt = 0

    !--- parameters -------------------------------------
    real(kp), parameter :: huge = 9.9692099683868690E36
    real(kp), parameter :: zero = 0.0_kp
    real(kp), parameter :: clear_val = zero

    !--- missing value --- 
    if (present(missval)) then
       spval = missval
    else
       spval = shr_const_spval
    endif

    !--- addtional constants ---
    cpinv = 1.0_kp/cp
    hvapi = 1.0_kp/hvap
    elocp = hvap/cp
 
    !--- compute some needed quantities ---
    wind(:) = sqrt(ubot(:)**2+vbot(:)**2)

    !--- compute dimensionless exner function ---
    prslk1(:) = (pbot(:)/p0)**cappa ! dimensionless_exner_function_at_surface_adjacent_layer
    prsik1(:) = (psfc(:)/p0)**cappa ! surface_dimensionless_exner_function
    prslki(:) = prsik1(:)/prslk1(:) ! ratio_of_exner_function_between_midlayer_and_interface_at_lowest_model_layer

    !--- initialization of variables ---
    !levs         = 127            ! vertical_layer_dimension, npz in input.nml
    !isot         = 1              ! control_for_soil_type_dataset, isot in input.nml
    !stype(:)     = 0              ! soil_type_classification, no land
    !vtype(:)     = 0              ! vegetation_type_classification, no land
    !slope(:)     = 0              ! surface_slope_classification
    !vfrac(:)     = 0.0_kp         ! vegetation_area_fraction, no land set it to zero

    kice         = 1              ! vertical_dimension_of_sea_ice
    km           = 1              ! vertical_dimension_of_soil
    tiice(:,:)   = 0.0_kp         ! temperature_in_ice_layer
    lheatstrg    = .true.         ! flag_for_canopy_heat_storage_in_land_surface_scheme
    h0facu       = 0.25_kp        ! multiplicative_tuning_parameter_for_reduced_surface_heat_fluxes_due_to_canopy_heat_storage
    h0facs       = 1.0            ! multiplicative_tuning_parameter_for_reduced_latent_heat_flux_due_to_canopy_heat_storage
    hflxq(:)     = 0.0_kp         ! kinematic_surface_upward_sensible_heat_flux_reduced_by_surface_roughness_and_vegetation
    hffac(:)     = 0.0_kp         ! surface_upward_sensible_heat_flux_reduction_factor
    stc(:,:)     = 0.0_kp         ! soil_temperature

    flag_restart = .false.        ! flag_for_restart, restart run
    lkm          = 0              ! control_for_lake_surface_scheme
    frac_grid    = .true.         ! flag_for_fractional_landmask
    flag_cice(:) = .true.         ! flag_for_cice
    cplflx       = .true.         ! flag_for_surface_flux_coupling
    cplice       = .true.         ! flag_for_sea_ice_coupling
    cplwav2atm   = .false.        ! flag_for_one_way_ocean_wave_coupling_to_atmosphere
    where (mask(:) /= 0)
    landfrac(:)  = 0.0_kp         ! land_area_fraction
    elsewhere
    landfrac(:)  = 1.0_kp         ! land_area_fraction
    end where 
    lakefrac(:)  = 0.0_kp         ! lake_area_fraction
    lakedepth(:) = 0.0_kp         ! lake_depth
    where (mask(:) /= 0)
    oceanfrac(:) = 1.0_kp         ! sea_area_fraction
    elsewhere
    oceanfrac(:) = 0.0_kp         ! sea_area_fraction
    end where 
    frland(:)    = 0.0_kp         ! land_area_fraction_for_microphysics
    dry(:)       = .false.        ! flag_nonzero_land_surface_fraction, no land
    icy(:)       = .false.        ! flag_nonzero_sea_ice_surface_fraction, no sea-ice
    lake(:)      = .false.        ! flag_nonzero_lake_surface_fraction
    use_flake(:) = .false.        ! flag_for_using_flake
    wet(:)       = .false.        ! (mask(:) /= 0) ! flag_nonzero_wet_surface_fraction
    hice(:)      = 0.0_kp         ! sea_ice_thickness
    cice(:)      = 0.0_kp         ! sea_ice_area_fraction_of_sea_area_fraction

    if (flag_init) then
       allocate(z0rl_wat(nMax))
       z0rl_wat(:) = 0.0_kp       ! surface_roughness_length_over_water
       allocate(z0rl_lnd(nMax))
       z0rl_lnd(:) = 0.0_kp       ! surface_roughness_length_over_land
       allocate(z0rl_ice(nMax))
       z0rl_ice(:) = 0.0_kp       ! surface_roughness_length_over_ice
       allocate(z0rl_wav(nMax))
       z0rl_wav(:) = 0.0_kp       ! surface_roughness_length_from_wave_model
       allocate(z0rl(nMax))
       z0rl(:)     = 0.0_kp       ! surface_roughness_length
    end if

    snowd(:)     = 0.0_kp         ! lwe_surface_snow
    snowd_lnd(:) = 0.0_kp         ! surface_snow_thickness_water_equivalent_over_land
    snowd_ice(:) = 0.0_kp         ! surface_snow_thickness_water_equivalent_over_ice
    tprcp(:)     = 0.0_kp         ! nonnegative_lwe_thickness_of_precipitation_amount_on_dynamics_timestep
    tprcp_wat(:) = 0.0_kp         ! nonnegative_lwe_thickness_of_precipitation_amount_on_dynamics_timestep_over_water
    tprcp_lnd(:) = 0.0_kp         ! nonnegative_lwe_thickness_of_precipitation_amount_on_dynamics_timestep_over_land
    tprcp_ice(:) = 0.0_kp         ! nonnegative_lwe_thickness_of_precipitation_amount_on_dynamics_timestep_over_ice
    
    !if (flag_init) then
    !   allocate(ustar(nMax))
       ustar(:)     = 0.0_kp      ! surface_friction_velocity
    !   allocate(ustar_wat(nMax))
       ustar_wat(:) = 0.0_kp      ! surface_friction_velocity_over_water
    !   allocate(ustar_lnd(nMax))
       ustar_lnd(:) = 0.0_kp      ! surface_friction_velocity_over_land
    !   allocate(ustar_ice(nMax))
       ustar_ice(:) = 0.0_kp      ! surface_friction_velocity_over_ice
    !end if
    weasd(:)     = 0.0_kp         ! lwe_thickness_of_surface_snow_amount
    weasd_lnd(:) = 0.0_kp         ! water_equivalent_accumulated_snow_depth_over_land
    weasd_ice(:) = 0.0_kp         ! water_equivalent_accumulated_snow_depth_over_ice

    !if (flag_init) then
    !   allocate(tskin(nMax))
       tskin(:)     = 0.0_kp      ! surface_skin_temperature
   !    allocate(tskin_wat(nMax))
       tskin_wat(:) = 0.0_kp      ! surface_skin_temperature_over_water 
   !    allocate(tskin_lnd(nMax))
       tskin_lnd(:) = 0.0_kp      ! surface_skin_temperature_over_land
   !    allocate(tskin_ice(nMax))
       tskin_ice(:) = 0.0_kp      ! surface_skin_temperature_over_ice
    !end if

    tsfc(:)      = 0.0_kp         ! surface_skin_temperature
    tsfc_wat(:)  = 0.0_kp         ! surface_skin_temperature_over_water_interstitial
    tsfc_lnd(:)  = 0.0_kp         ! surface_skin_temperature_over_land_interstitial
    tsfc_ice(:)  = 0.0_kp         ! surface_skin_temperature_over_ice_interstitial
    tsfco(:)     = ts(:)          ! sea_surface_temperature
    tsurf_wat(:) = 0.0_kp         ! surface_skin_temperature_after_iteration_over_water
    tsurf_lnd(:) = 0.0_kp         ! surface_skin_temperature_after_iteration_over_land
    tsurf_ice(:) = 0.0_kp         ! surface_skin_temperature_after_iteration_over_ice
    tisfc(:)     = 0.0_kp         ! sea_ice_temperature
    tgice        = tice           ! freezing_point_temperature_of_seawater
    islmsk(:)    = 0              ! sea_land_ice_mask, all sea 
    islmsk_cice(:) = 0            ! sea_land_ice_mask_cice, all sea
    slmsk(:)     = 0              ! area_type, all sea
    qss(:)       = qbot(:)        ! surface_specific_humidity ? not the lowest level
    qss_wat(:)   = qss(:)         ! surface_specific_humidity_over_water
    qss_lnd(:)   = 0.0_kp         ! surface_specific_humidity_over_land
    qss_ice(:)   = 0.0_kp         ! surface_specific_humidity_over_ice
    min_lakeice  = 0.15_kp        ! min_lake_ice_area_fraction
    min_seaice   = 1.0e-11_kp     ! min_sea_ice_area_fraction
    kdt          = kdt+1          ! index_of_timestep

    sigmaf(:)     = 0.0_kp         ! bounded_vegetation_area_fraction, no veg
    vegtype(:)    = 0              ! vegetation_type_classification
    shdmax(:)     = 0.0_kp         ! max_vegetation_area_fraction
    ivegsrc       = 1              ! control_for_vegetation_dataset, IGBP
    z0pert(:)     = 0.0_kp         ! perturbation_of_momentum_roughness_length
    ztpert(:)     = 0.0_kp         ! perturbation_of_heat_to_momentum_roughness_length_ratio
    flag_iter(:)  = .true.         ! flag_for_iteration
    redrag        = .true.         ! flag_for_limited_surface_roughness_length_over_ocean, redrag in input.nml
    sfc_z0_type   = 0              ! flag_for_surface_roughness_option_over_water, no change
    thsfc_loc     = .true.         ! flag_for_reference_pressure_theta
    cm(:)         = 0.0_kp         ! surface_drag_coefficient_for_momentum
    cm_wat(:)     = 0.0_kp         ! surface_drag_coefficient_for_momentum_in_air_over_water
    cm_lnd(:)     = 0.0_kp         ! surface_drag_coefficient_for_momentum_in_air_over_land
    cm_ice(:)     = 0.0_kp         ! surface_drag_coefficient_for_momentum_in_air_over_ice
    ch(:)         = 0.0_kp         ! surface_drag_coefficient_for_heat_and_moisture
    ch_wat(:)     = 0.0_kp         ! surface_drag_coefficient_for_heat_and_moisture_in_air_over_water
    ch_lnd(:)     = 0.0_kp         ! surface_drag_coefficient_for_heat_and_moisture_in_air_over_land
    ch_ice(:)     = 0.0_kp         ! surface_drag_coefficient_for_heat_and_moisture_in_air_over_ice
    rb(:)         = 0.0_kp         ! bulk_richardson_number_at_lowest_model_level
    rb_wat(:)     = 0.0_kp         ! bulk_richardson_number_at_lowest_model_level_over_water
    rb_lnd(:)     = 0.0_kp         ! bulk_richardson_number_at_lowest_model_level_over_land
    rb_ice(:)     = 0.0_kp         ! bulk_richardson_number_at_lowest_model_level_over_ice
    stress(:)     = 0.0_kp         ! surface_wind_stress
    stress_wat(:) = 0.0_kp         ! surface_wind_stress_over_water
    stress_lnd(:) = 0.0_kp         ! surface_wind_stress_over_land
    stress_ice(:) = 0.0_kp         ! surface_wind_stress_over_ice

    !if (flag_init) then
       !allocate(fm_wat(nMax))
       fm(:)        = 0.0_kp      ! Monin_Obukhov_similarity_function_for_momentum
       fm_wat(:)    = 0.0_kp      ! Monin_Obukhov_similarity_function_for_momentum_over_water
       !allocate(fm_lnd(nMax))
       fm_lnd(:)    = 0.0_kp      ! Monin_Obukhov_similarity_function_for_momentum_over_land
       !allocate(fm_ice(nMax))
       fm_ice(:)    = 0.0_kp      ! Monin_Obukhov_similarity_function_for_momentum_over_ice
       !allocate(fh_wat(nMax))
       fh(:)        = 0.0_kp      ! Monin_Obukhov_similarity_function_for_heat
       fh_wat(:)    = 0.0_kp      ! Monin_Obukhov_similarity_function_for_heat_over_water
       !allocate(fh_lnd(nMax))
       fh_lnd(:)    = 0.0_kp      ! Monin_Obukhov_similarity_function_for_heat_over_land
       !allocate(fh_ice(nMax))
       fh_ice(:)    = 0.0_kp      ! Monin_Obukhov_similarity_function_for_heat_over_ice
       !allocate(fm10_wat(nMax))
       fm10(:)      = 0.0_kp      ! Monin_Obukhov_similarity_function_for_momentum
       fm10_wat(:)  = 0.0_kp      ! Monin_Obukhov_similarity_function_for_momentum_at_10m_over_water
       !allocate(fm10_lnd(nMax))
       fm10_lnd(:)  = 0.0_kp      ! Monin_Obukhov_similarity_function_for_momentum_at_10m_over_land
       !allocate(fm10_ice(nMax))
       fm10_ice(:)  = 0.0_kp      ! Monin_Obukhov_similarity_function_for_momentum_at_10m_over_ice
    !end if
    fh2(:)       = 0.0_kp         ! Monin_Obukhov_similarity_function_for_heat
    fh2_wat(:)   = 0.0_kp         ! Monin_Obukhov_similarity_function_for_heat_at_2m_over_water
    fh2_lnd(:)   = 0.0_kp         ! Monin_Obukhov_similarity_function_for_heat_at_2m_over_land
    fh2_ice(:)   = 0.0_kp         ! Monin_Obukhov_similarity_function_for_heat_at_2m_over_ice
    ztmax_wat(:) = 0.0_kp         ! bounded_surface_roughness_length_for_heat_over_water
    ztmax_lnd(:) = 0.0_kp         ! bounded_surface_roughness_length_for_heat_over_land
    ztmax_ice(:) = 0.0_kp         ! bounded_surface_roughness_length_for_heat_over_ice
    zvfun(:)     = 0.0_kp         ! function_of_surface_roughness_length_and_green_vegetation_fraction

    lseaspray    = .true.         ! flag_for_sea_spray
    cmm(:)       = 0.0_kp         ! surface_drag_wind_speed_for_momentum         
    cmm_wat(:)   = 0.0_kp         ! surface_drag_wind_speed_for_momentum_in_air_over_water
    cmm_lnd(:)   = 0.0_kp         ! surface_drag_wind_speed_for_momentum_in_air_over_land
    cmm_ice(:)   = 0.0_kp         ! surface_drag_wind_speed_for_momentum_in_air_over_ice
    chh(:)       = 0.0_kp         ! surface_drag_mass_flux_for_heat_and_moisture 
    chh_wat(:)   = 0.0_kp         ! surface_drag_mass_flux_for_heat_and_moisture_in_air_over_water
    chh_lnd(:)   = 0.0_kp         ! surface_drag_mass_flux_for_heat_and_moisture_in_air_over_land
    chh_ice(:)   = 0.0_kp         ! surface_drag_mass_flux_for_heat_and_moisture_in_air_over_ice
    gflx(:)      = 0.0_kp         ! upward_heat_flux_in_soil
    gflx_wat(:)  = 0.0_kp         ! upward_heat_flux_in_soil_over_water
    gflx_lnd(:)  = 0.0_kp         ! upward_heat_flux_in_soil_over_lnd
    gflx_ice(:)  = 0.0_kp         ! upward_heat_flux_in_soil_over_ice
    evap(:)      = 0.0_kp         ! kinematic_surface_upward_latent_heat_flux
    evap_wat(:)  = 0.0_kp         ! kinematic_surface_upward_latent_heat_flux_over_water
    evap_lnd(:)  = 0.0_kp         ! kinematic_surface_upward_latent_heat_flux_over_land
    evap_ice(:)  = 0.0_kp         ! kinematic_surface_upward_latent_heat_flux_over_ice
    hflx(:)      = 0.0_kp         ! kinematic_surface_upward_sensible_heat_flux
    hflx_wat(:)  = 0.0_kp         ! kinematic_surface_upward_sensible_heat_flux_over_water
    hflx_lnd(:)  = 0.0_kp         ! kinematic_surface_upward_sensible_heat_flux_over_land
    hflx_ice(:)  = 0.0_kp         ! kinematic_surface_upward_sensible_heat_flux_over_ice
    ep1d(:)      = 0.0_kp         ! surface_upward_potential_latent_heat_flux
    ep1d_wat(:)  = 0.0_kp         ! surface_upward_potential_latent_heat_flux_over_water
    ep1d_lnd(:)  = 0.0_kp         ! surface_upward_potential_latent_heat_flux_over_land
    ep1d_ice(:)  = 0.0_kp         ! surface_upward_potential_latent_heat_flux_over_ice

    !--- generic surface call ---
    !call GFS_surface_generic_pre_run( &
    !     nthreads  , nMax       , levs        , &
    !     vfrac     , islmsk     , isot        , &
    !     ivegsrc   , stype      , vtype       , &
    !     slope     , prsik1     , prslk1      , &
    !     tsfc      , phil       , grav        , & ! ? phil
    !     sigmaf    , prslki     , z1          , &
    !                      drain_cpl, dsnow_cpl, rain_cpl, snow_cpl, lndp_type, n_var_lndp, sfc_wts,        &
    !                      lndp_var_list, lndp_prt_list,                                                    &
    !                      z01d, zt1d, bexp1d, xlai1d, vegf1d, lndp_vgf,                                    &
    !                      cplflx, flag_cice, islmsk_cice, slimskin_cpl,                                    &
    !                      wind, u1, v1, cnvwind, smcwlt2, smcref2, vtype_save, stype_save, slope_save,     &
    !                      errmsg, errflg)

    !--- GFS surface scheme pre ---
    call GFS_surface_composites_pre_run( &
         nMax      , flag_init  , flag_restart, &
         lkm       , frac_grid  , flag_cice   , &
         cplflx    , cplice     , cplwav2atm  , &
         landfrac  , lakefrac   , lakedepth   , &
         oceanfrac , frland     , dry         , &
         icy       , lake       , use_flake   , &
         wet       , hice       , cice        , &
         z0rl_wat  , z0rl_lnd   , z0rl_ice    , &
         snowd     , snowd_lnd  , snowd_ice   , &
         tprcp     ,                            &
         tprcp_wat , tprcp_lnd  , tprcp_ice   , &
         ustar     , ustar_wat  , ustar_lnd   , &
         ustar_ice ,                            &
         weasd     , weasd_lnd  , weasd_ice   , &
         ep1d_ice  , tskin      , tsfco       , &
         tskin_lnd , tskin_wat  , tskin_ice   , &
         tsurf_wat , tsurf_lnd  , tsurf_ice   , &
         gflx_ice  , tgice      ,               &
         islmsk    , islmsk_cice, slmsk       , &
         qss       , qss_wat    , qss_lnd     , &
         qss_ice   , min_lakeice, min_seaice  , &
         kdt       , huge       , errmsg      , &
         errflg)

    !--- surface iteration loop ---
    do iter = 1, 2
       !--- calculate stability parameters ---
       call sfc_diff_run( &
            nMax      , rvrdm1     , eps         , &
            epsm1     , grav       , psfc        , &
            tbot      , qbot       , zbot        , &
            garea     , wind       , pbot        , &
            prslki    , prsik1     , prslk1      , &
            sigmaf    , vegtype    , shdmax      , &
            ivegsrc   , z0pert     , ztpert      , &
            flag_iter , redrag     , usfc        , &
            vsfc      , sfc_z0_type, wet         , &
            dry       , icy        , thsfc_loc   , &
            tskin_wat , tskin_lnd  , tskin_ice   , &
            tsurf_wat , tsurf_lnd  , tsurf_ice   , &
            z0rl_wat  , z0rl_lnd   , z0rl_ice    , &
            z0rl_wav  ,                            &
            ustar_wat , ustar_lnd  , ustar_ice   , &
            cm_wat    , cm_lnd     , cm_ice      , &
            ch_wat    , ch_lnd     , ch_ice      , &
            rb_wat    , rb_lnd     , rb_ice      , &
            stress_wat, stress_lnd , stress_ice  , &
            fm_wat    , fm_lnd     , fm_ice      , &
            fh_wat    , fh_lnd     , fh_ice      , &
            fm10_wat  , fm10_lnd   , fm10_ice    , &
            fh2_wat   , fh2_lnd    , fh2_ice     , &
            ztmax_wat , ztmax_lnd  , ztmax_ice   , &
            zvfun     , errmsg     , errflg)

       !print*, "cm_wat    = ", iter, minval(cm_wat, mask=(mask(:) /= 0)), maxval(cm_wat, mask=(mask(:) /= 0))
       !print*, "ch_wat    = ", iter, minval(ch_wat, mask=(mask(:) /= 0)), maxval(ch_wat, mask=(mask(:) /= 0))
       !print*, "fm_wat    = ", iter, minval(fm_wat, mask=(mask(:) /= 0)), maxval(fm_wat, mask=(mask(:) /= 0))
       !print*, "fm10_wat  = ", iter, minval(fm10_wat, mask=(mask(:) /= 0)), maxval(fm10_wat, mask=(mask(:) /= 0))
       !print*, "z0rl_wat  = ", iter, minval(z0rl_wat, mask=(mask(:) /= 0)), maxval(z0rl_wat, mask=(mask(:) /= 0))
       !print*, "tskin_wat = ", iter, minval(tskin_wat, mask=(mask(:) /= 0)), maxval(tskin_wat, mask=(mask(:) /= 0))

       !--- update flag_guess ---
       call GFS_surface_loop_control_part1_run( &
            nMax       , iter      , wind        , &
            flag_guess , errmsg    , errflg)

       !--- calculate heat fluxes ---
       call sfc_ocean_run( &
            nMax       , hvap      , cp          , &
            rd         , eps       , epsm1       , &
            rvrdm1     , psfc      , ubot        , &
            vbot       , tbot      , qbot        , &
            tskin_wat  , cm_wat    , ch_wat      , &
            lseaspray  , fm_wat    , fm10_wat    , &
            pbot       , prslki    , wet         , &
            use_flake  , wind      , flag_iter   , &
            qss_wat    , cmm_wat   , chh_wat     , &
            gflx_wat   , evap_wat  , hflx_wat    , &
            ep1d_wat   , errmsg    , errflg, 'a')

       !print*, "lat = ", iter, minval(evap_wat, mask=(mask(:) /= 0)), maxval(evap_wat, mask=(mask(:) /= 0))
       !print*, "sen = ", iter, minval(hflx_wat, mask=(mask(:) /= 0)), maxval(hflx_wat, mask=(mask(:) /= 0))
    end do

    !--- GFS surface scheme post ---
    call GFS_surface_composites_post_run( &
         nMax      , kice       , km          , &
         rd        , rvrdm1     , cplflx      , &
         cplwav2atm, frac_grid  , flag_cice   , &
         thsfc_loc , islmsk     , dry         , &
         wet       , icy        , wind        , &
         tbot      , qbot       , pbot        , &
         landfrac  , lakefrac   , oceanfrac   , &
         z0rl      , z0rl_wat   , z0rl_lnd    , &
         z0rl_ice  , garea      , cm          , &
         cm_wat    , cm_lnd     , cm_ice      , &
         ch        , ch_wat     , ch_lnd      , &
         ch_ice    , rb         , rb_wat      , &
         rb_lnd    , rb_ice     , stress      , &
         stress_wat, stress_lnd , stress_ice  , &
         fm        , fm_wat     , fm_lnd      , &
         fm_ice    , fh         , fh_wat      , &
         fh_lnd    , fh_ice     , ustar       , &
         ustar_wat , ustar_lnd  , ustar_ice   , &
         fm10      , fm10_wat   , fm10_lnd    , &
         fm10_ice  , fh2        , fh2_wat     , &
         fh2_lnd   , fh2_ice    , tsurf_wat   , &
         tsurf_lnd , tsurf_ice  , cmm         , &
         cmm_wat   , cmm_lnd    , cmm_ice     , &
         chh       , chh_wat    , chh_lnd     , &
         chh_ice   , gflx       , gflx_wat    , &
         gflx_lnd  , gflx_ice   , ep1d        , &
         ep1d_wat  , ep1d_lnd   , ep1d_ice    , &
         weasd     , weasd_lnd  , weasd_ice   , &
         snowd     , snowd_lnd  , snowd_ice   , &
         tprcp     , tprcp_wat  , tprcp_lnd   , &
         tprcp_ice , evap       , evap_wat    , &
         evap_lnd  , evap_ice   , hflx        , &
         hflx_wat  , hflx_lnd   , hflx_ice    , &
         qss       , qss_wat    , qss_lnd     , &
         qss_ice   , tsfc       , tsfco       , &
         tsfcl     , tsfc_wat   ,               &
         tisfc     ,                            &
         hice      , cice       ,               & 
         tiice     , sigmaf     , zvfun       , &
         lheatstrg , h0facu     , h0facs      , &
         hflxq     , hffac      , stc         , &
         grav      , prsik1     , prslk1      , &
         prslki    , zbot       , ztmax_wat   , &
         ztmax_lnd , ztmax_ice  , huge        , &
         errmsg    , errflg)

    !--- unit conversion ---
    do n = 1, nMax
       if (mask(n) /= 0) then
          sen(n) = hflx_wat(n)*rbot(n)*cp
          lat(n) = evap_wat(n)*rbot(n)*hvap
          taux(n) = qbot(n)
          tauy(n) = rbot(n)
       else
          sen(n) = spval
          lat(n) = spval
          taux(n) = spval
          tauy(n) = spval
       end if
    end do

    flag_init = .false.

    !print*, "lat = ", minval(lat, mask=(mask(:) /= 0)), maxval(lat, mask=(mask(:) /= 0))
    !print*, "sen = ", minval(sen, mask=(mask(:) /= 0)), maxval(sen, mask=(mask(:) /= 0))

  end subroutine shr_flux_atmOcn_ufs
#endif

end module shr_flux_mod
