module esmFldsExchange_mod

  use ESMF
  use NUOPC
  use med_utils_mod, only : chkerr => med_utils_chkerr
  use med_kind_mod,  only : CX=>SHR_KIND_CX
  use med_kind_mod,  only : CS=>SHR_KIND_CS
  use med_kind_mod,  only : CL=>SHR_KIND_CL
  use esmflds,       only : compmed
  use esmflds,       only : compatm
  use esmflds,       only : compocn
  use esmflds,       only : ncomps
  use esmflds,       only : fldListTo
  use esmflds,       only : fldListFr

  !---------------------------------------------------------------------
  ! This is a mediator specific routine that determines ALL possible
  ! fields exchanged between components and their associated routing,
  ! mapping and merging
  !---------------------------------------------------------------------

  implicit none
  private

  public :: esmFldsExchange

  character(*), parameter :: u_FILE_u = __FILE__

  type gcomp_attr
    character(len=CX) :: atm2ocn_fmap='unset'
    character(len=CX) :: atm2ocn_smap='unset'
    character(len=CX) :: atm2ocn_vmap='unset'
    character(len=CX) :: ocn2atm_fmap='unset'
    character(len=CX) :: ocn2atm_smap='unset'
    character(len=CX) :: ocn2atm_vmap='unset'
  end type

!===============================================================================
contains
!===============================================================================

  subroutine esmFldsExchange(gcomp, phase, rc)

    ! input/output parameters:
    type(ESMF_GridComp)             :: gcomp
    character(len=*), intent(in)    :: phase
    integer         , intent(inout) :: rc

    ! local variables:
    character(len=*) , parameter    :: subname='(esmFldsExchange_mod)'
    !--------------------------------------

    call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO)
    rc = ESMF_SUCCESS

    if (phase == 'advertise') then
      call esmFldsExchange_advt(gcomp, phase, rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
    elseif (phase == 'fieldcheck') then
      call esmFldsExchange_fchk(gcomp, phase, rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
    elseif (phase == 'initialize') then
      call esmFldsExchange_init(gcomp, phase, rc)
      if (chkerr(rc,__LINE__,u_FILE_u)) return
    else
      call ESMF_LogSetError(ESMF_FAILURE, &
         msg=trim(subname)//": Phase is set to "//trim(phase), &
         line=__LINE__, file=__FILE__, rcToReturn=rc)
      return  ! bail out
    endif

    call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO)

  end subroutine esmFldsExchange

  !-----------------------------------------------------------------------------

  subroutine esmFldsExchange_advt(gcomp, phase, rc)

    ! input/output parameters:
    type(ESMF_GridComp)             :: gcomp
    character(len=*), intent(in)    :: phase
    integer         , intent(inout) :: rc

    ! local variables:
    integer                         :: i, n
    logical                         :: isPresent
    character(len=CL)               :: cvalue
    character(len=CS)               :: fldname
    type(gcomp_attr)                :: attr
    character(len=CS), allocatable  :: flds(:)
    character(len=*), parameter     :: subname='(esmFldsExchange_advt)'
    !--------------------------------------

    call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO)
    rc = ESMF_SUCCESS

    !=====================================================================
    ! scalar information
    !=====================================================================

    call NUOPC_CompAttributeGet(gcomp, name='ScalarFieldName', isPresent=isPresent, rc=rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    if (isPresent) then
       call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldName", value=cvalue, rc=rc)
       if (ChkErr(rc,__LINE__,u_FILE_u)) return
       do n = 1, ncomps
          call addfld(fldListFr(n)%flds, trim(cvalue))
          call addfld(fldListTo(n)%flds, trim(cvalue))
       end do
    end if

    !=====================================================================
    ! FIELDS TO OCN
    !=====================================================================

    allocate(flds(25))
    flds = (/ 'Faxa_swnet', &
              'Faxa_lwnet', &
              'Faxa_rainc', &
              'Faxa_rainl', &
              'Faxa_snowc', &
              'Faxa_snowl', &
              'Faxa_swdn ', &
              'Faxa_lwdn ', &
              'Faxa_rain ', &
              'Faxa_taux ', &
              'Faxa_tauy ', &
              'Faxa_sen  ', &
              'Faxa_lat  ', &
              'Sa_wspd   ', &
              'Sa_tbot   ', &
              'Sa_tskn   ', &
              'Sa_pbot   ', &
              'Sa_shum   ', &
              'Sa_ptem   ', &
              'Sa_pslv   ', &
              'Sa_dens   ', &
              'Sa_topo   ', &
              'Sa_u      ', &
              'Sa_v      ', &
              'Sa_z      ' /)
    do n = 1,size(flds)
       fldname = trim(flds(n))
       call addfld(fldListFr(compatm)%flds, trim(fldname))
       call addfld(fldListTo(compocn)%flds, trim(fldname))
    end do
    deallocate(flds)

    !=====================================================================
    ! FIELDS TO ATM
    !=====================================================================

    allocate(flds(1))
    flds = (/ 'So_t' /)
    do n = 1,size(flds)
       fldname = trim(flds(n))
       call addfld(fldListFr(compocn)%flds, trim(fldname))
       call addfld(fldListTo(compatm)%flds, trim(fldname))
    end do
    deallocate(flds)

    call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO)

  end subroutine esmFldsExchange_advt

  !-----------------------------------------------------------------------------

  subroutine esmFldsExchange_fchk(gcomp, phase, rc)

    use med_methods_mod, only : fldchk => med_methods_FB_FldChk
    use med_internalstate_mod, only : InternalState

    ! input/output parameters:
    type(ESMF_GridComp)             :: gcomp
    character(len=*), intent(in)    :: phase
    integer         , intent(inout) :: rc

    ! local variables:
    type(InternalState)             :: is_local
    character(len=*) , parameter    :: subname='(esmFldsExchange_fchk)'
    !--------------------------------------

    call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO)
    rc = ESMF_SUCCESS

    !=====================================================================
    ! Get the internal state
    !=====================================================================

    nullify(is_local%wrap)
    call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
    if (chkerr(rc,__LINE__,u_FILE_u)) return

    call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO)

  end subroutine esmFldsExchange_fchk

  !-----------------------------------------------------------------------------

  subroutine esmFldsExchange_init(gcomp, phase, rc)

  use med_methods_mod, only : fldchk => med_methods_FB_FldChk
  use med_internalstate_mod, only : InternalState

  ! input/output parameters:
  type(ESMF_GridComp)             :: gcomp
  character(len=*), intent(in)    :: phase
  integer         , intent(inout) :: rc

  ! local variables:
  type(InternalState)             :: is_local
  integer                         :: n
  character(len=CS)               :: fldname
  character(len=CS), allocatable  :: flds(:)
  character(len=*) , parameter    :: subname='(esmFldsExchange_init)'
  !--------------------------------------

  call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO)
  rc = ESMF_SUCCESS

  !=====================================================================
  ! Get the internal state
  !=====================================================================

  nullify(is_local%wrap)
  call ESMF_GridCompGetInternalState(gcomp, is_local, rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return

  !=====================================================================
  ! FIELDS TO OCN
  !=====================================================================

  allocate(flds(11))
  flds = (/ 'Sa_u', &
            'Sa_v', &
            'Sa_wspd', &
            'Sa_z', &
            'Sa_tskn', &
            'Sa_pbot', &
            'Sa_shum', &
            'Sa_ptem', &
            'Sa_pslv', &
            'Sa_dens', &
            'Sa_topo' /)
  do n = 1,size(flds)
     fldname = trim(flds(n))
     call addmap(fldListFr(compatm)%flds, trim(fldname), compocn, &
          mapbilnr, 'one', attr%atm2ocn_smap)
     call addmrg(fldListTo(compocn)%flds, trim(fldname), &
          mrg_from=compatm, mrg_fld=trim(fldname), mrg_type='copy')
  end do
  deallocate(flds)

  allocate(flds(1))
  flds = (/ 'Sa_tbot' /)
  do n = 1,size(flds)
     fldname = trim(flds(n))
     call addmap(fldListFr(compatm)%flds, trim(fldname), compocn, &
          mapbilnr, 'one', attr%atm2ocn_smap)
  end do
  deallocate(flds)

  allocate(flds(8))
  flds = (/ 'Faxa_swnet', &
            'Faxa_lwnet', &
            'Faxa_swdn ', &
            'Faxa_lwdn ', &
            'Faxa_sen  ', &
            'Faxa_lat  ', &
            'Faxa_taux ', &
            'Faxa_tauy ' /)
  do n = 1,size(flds)
     fldname = trim(flds(n))
     call addmap(fldListFr(compatm)%flds, trim(fldname), compocn, &
          mapconsf, 'one', attr%atm2ocn_fmap)
  end do
  deallocate(flds)

  allocate(flds(5))
  flds = (/ 'Faxa_rainc', &
            'Faxa_rainl', &
            'Faxa_rain ', &
            'Faxa_snowc', &
            'Faxa_snowl' /)
  do n = 1,size(flds)
     fldname = trim(flds(n))
     call addmap(fldListFr(compatm)%flds, trim(fldname), compocn, &
          mapconsf, 'one', attr%atm2ocn_fmap)
     call addmrg(fldListTo(compocn)%flds, trim(fldname), &
          mrg_from=compatm, mrg_fld=trim(fldname), mrg_type='copy')
  end do
  deallocate(flds)

  call addmrg(fldListTo(compocn)%flds, Foxx_swnet, &
     mrg_from=compatm, mrg_fld='Faxa_swnet', mrg_type='copy')
  call addmrg(fldListTo(compocn)%flds, Foxx_lwnet, &
     mrg_from=compatm, mrg_fld='Faxa_lwnet', mrg_type='copy')
  call addmrg(fldListTo(compocn)%flds, Foxx_sen, &
     mrg_from=compatm, mrg_fld='Faxa_sen', mrg_type='copy')
  call addmrg(fldListTo(compocn)%flds, Foxx_lat, &
     mrg_from=compatm, mrg_fld='Faxa_lat', mrg_type='copy')
  call addmrg(fldListTo(compocn)%flds, Foxx_taux, &
     mrg_from=compatm, mrg_fld='Faxa_taux', mrg_type='copy')
  call addmrg(fldListTo(compocn)%flds, Foxx_taux, &
     mrg_from=compatm, mrg_fld='Faxa_tauy', mrg_type='copy')

  !=====================================================================
  ! FIELDS TO ATM
  !=====================================================================

  allocate(flds(1))
  flds = (/ 'So_t' /)
  do n = 1,size(flds)
     fldname = trim(flds(n))
     call addmap(fldListFr(compocn)%flds, trim(fldname), compatm, &
          mapfillv_bilnr, 'norm', attr%ocn2atm_smap)
     call addmrg(fldListTo(compatm)%flds, trim(fldname), &
          mrg_from=compocn, mrg_fld=trim(fldname), mrg_type='copy')
  end do
  deallocate(flds)


  call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO)

  end subroutine esmFldsExchange_init

  !-----------------------------------------------------------------------------

  subroutine esmFldsExchange_attr(gcomp, attr, rc)

  ! input/output parameters:
  type(ESMF_GridComp)              :: gcomp
  type(gcomp_attr), intent(inout)  :: attr
  integer         , intent(inout)  :: rc

  ! local variables:
  character(32)                    :: cname
  integer                          :: verbosity, diagnostic
  character(len=CL)                :: cvalue
  logical                          :: isPresent
  character(len=*), parameter      :: subname='(esmFldsExchange_attr)'

  !--------------------------------------

  call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO)
  rc = ESMF_SUCCESS

  !----------------------------------------------------------
  ! Query component for name, verbosity, and diagnostic values
  !----------------------------------------------------------

#if ESMF_VERSION_MAJOR >= 8
  call NUOPC_CompGet(gcomp, name=cname, verbosity=verbosity, &
    diagnostic=diagnostic, rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return
#else
  call ESMF_GridCompGet(gcomp, name=cname, rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return

  call ESMF_AttributeGet(gcomp, name="Verbosity", value=cvalue, &
    defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return

  verbosity = ESMF_UtilString2Int(cvalue, &
    specialStringList=(/"off ","low ","high","max "/), &
    specialValueList=(/0,9985,32513,131071/), rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return

  call ESMF_AttributeGet(gcomp, name="Diagnostic", value=cvalue, &
    defaultValue="0", convention="NUOPC", purpose="Instance", rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return

  diagnostic = ESMF_UtilString2Int(cvalue, &
    specialStringList=(/"off ","max "/), &
    specialValueList=(/0,131071/), rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return
#endif

  !----------------------------------------------------------
  ! Initialize mapping file names
  !----------------------------------------------------------

  ! mapping to ocn
  call NUOPC_CompAttributeGet(gcomp, name='atm2ocn_fmapname', &
    value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return
  if (isPresent .and. isSet) then
    attr%atm2ocn_fmap = trim(cvalue)
    if (mastertask) write(logunit, '(a)') trim(subname)// &
      'atm2ocn_fmapname = '//trim(attr%atm2ocn_fmap)
  end if

  call NUOPC_CompAttributeGet(gcomp, name='atm2ocn_smapname', &
    value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return
  if (isPresent .and. isSet) then
    attr%atm2ocn_smap = trim(cvalue)
    if (mastertask) write(logunit, '(a)') trim(subname)// &
      'atm2ocn_smapname = '//trim(attr%atm2ocn_smap)
  end if

  call NUOPC_CompAttributeGet(gcomp, name='atm2ocn_fmapname', &
    value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return
  if (isPresent .and. isSet) then
    attr%atm2ocn_vmap = trim(cvalue)
    if (mastertask) write(logunit, '(a)') trim(subname)// &
      'atm2ocn_vmapname = '//trim(attr%atm2ocn_vmap)
  end if

  ! mapping to atm
  call NUOPC_CompAttributeGet(gcomp, name='ocn2atm_fmapname', &
    value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return
  if (isPresent .and. isSet) then
    attr%ocn2atm_fmap = trim(cvalue)
    if (mastertask) write(logunit, '(a)') trim(subname)// &
      'ocn2atm_fmapname = '//trim(attr%ocn2atm_fmap)
  end if

  call NUOPC_CompAttributeGet(gcomp, name='ocn2atm_smapname', &
    value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return
  if (isPresent .and. isSet) then
    attr%ocn2atm_smap = trim(cvalue)
    if (mastertask) write(logunit, '(a)') trim(subname)// &
      'ocn2atm_smapname = '//trim(attr%ocn2atm_smap)
  end if

  call NUOPC_CompAttributeGet(gcomp, name='ocn2atm_fmapname', &
    value=cvalue, isPresent=isPresent, isSet=isSet, rc=rc)
  if (chkerr(rc,__LINE__,u_FILE_u)) return
  if (isPresent .and. isSet) then
    attr%ocn2atm_vmap = trim(cvalue)
    if (mastertask) write(logunit, '(a)') trim(subname)// &
      'ocn2atm_vmapname = '//trim(attr%ocn2atm_vmap)
  end if

  call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO)

  end subroutine esmFldsExchange_attr

end module esmFldsExchange_mod