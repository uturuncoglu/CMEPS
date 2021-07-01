module esmFldsExchange_nems_mod
  !---------------------------------------------------------------------
  ! ATTENTION:
  !
  ! This file is auto-generated and should not be manually edited
  ! Please use application specific YAML file and parser in the gen/
  ! directory to update content of the file.
  !---------------------------------------------------------------------

  !---------------------------------------------------------------------
  ! This is a mediator specific routine that determines ALL possible
  ! fields exchanged between components and their associated routing,
  ! mapping and merging
  !---------------------------------------------------------------------

  implicit none
  public

  public :: esmFldsExchange_nems

  character(*), parameter :: u_FILE_u = &
       __FILE__

!===============================================================================
contains
!===============================================================================

  subroutine esmFldsExchange_nems(gcomp, phase, rc)

    use ESMF
    use NUOPC
    use med_kind_mod          , only : CX=>SHR_KIND_CX, CS=>SHR_KIND_CS
    use med_kind_mod          , only : CL=>SHR_KIND_CL, R8=>SHR_KIND_R8
    use med_utils_mod         , only : chkerr => med_utils_chkerr
    use esmFlds               , only : med_fldList_type
    use esmFlds               , only : addfld => med_fldList_AddFld
    use esmFlds               , only : addmap => med_fldList_AddMap
    use esmFlds               , only : addmrg => med_fldList_AddMrg
    use esmflds               , only : compatm
    use esmflds               , only : compice
    use esmflds               , only : compmed
    use esmflds               , only : compocn
    use esmflds               , only : mapconsf
    use esmflds               , only : mapconsf_aofrac
    use esmflds               , only : mapfcopy
    use esmflds               , only : ncomps, coupling_mode, mapnames
    use esmflds               , only : fldListTo, fldListFr
    use esmflds               , only : fldListMed_aoflux, fldListMed_ocnalb
    use med_internalstate_mod , only : mastertask, logunit

    ! input/output parameters:
    type(ESMF_GridComp)              :: gcomp
    character(len=*) , intent(in)    :: phase
    integer          , intent(inout) :: rc

    ! local variables:
    integer             :: i, n, maptype
    character(len=CX)   :: msgString
    character(len=CL)   :: cvalue
    character(len=CS), allocatable :: flds1(:)
    character(len=CS), allocatable :: flds2(:,:)
    character(len=*) , parameter   :: subname='(esmFldsExchange_nems)'
    !--------------------------------------

    call ESMF_LogWrite(trim(subname)//": called", ESMF_LOGMSG_INFO)
    rc = ESMF_SUCCESS

    !=====================================================================
    ! Scalar information
    !=====================================================================

    call NUOPC_CompAttributeGet(gcomp, name="ScalarFieldName", value=cvalue, rc=rc)
    if (ChkErr(rc,__LINE__,u_FILE_u)) return
    do n = 1,ncomps
       call addfld(fldListFr(n)%flds, trim(cvalue))
       call addfld(fldListTo(n)%flds, trim(cvalue))
    end do

    !=====================================================================
    ! FIELDS TO ATM
    !=====================================================================
    allocate(flds1(9))
    flds1 = (/ 'Faii_taux ', &
               'Faii_tauy ', &
               'Faii_lat  ', &
               'Faii_sen  ', &
               'Faii_lwup ', &
               'Faii_evap ', &
               'Si_vice   ', &
               'Si_vsno   ', &
               'Si_t      ' /)
    do n = 1, size(flds1)
       call addfld(fldListFr(compice)%flds, trim(flds1(n)))
       call addfld(fldListTo(compatm)%flds, trim(flds1(n)))
       call addmap(fldListFr(compice)%flds, trim(flds1(n)), compatm, mapconsf, 'ifrac', 'unset')
       call addmrg(fldListTo(compatm)%flds, trim(flds1(n)), mrg_from=compice, mrg_fld=trim(flds1(n)), mrg_type='copy')
    end do
    deallocate(flds1)

    allocate(flds1(1))
    flds1 = (/ 'Si_ifrac  ' /)
    do n = 1, size(flds1)
       call addfld(fldListFr(compice)%flds, trim(flds1(n)))
       call addfld(fldListTo(compatm)%flds, trim(flds1(n)))
    end do
    deallocate(flds1)

    allocate(flds1(1))
    flds1 = (/ 'So_t      ' /)
    do n = 1, size(flds1)
       call addfld(fldListFr(compocn)%flds, trim(flds1(n)))
       call addfld(fldListTo(compatm)%flds, trim(flds1(n)))
       call addmap(fldListFr(compocn)%flds, trim(flds1(n)), compatm, mapconsf, 'ofrac', 'unset')
       call addmrg(fldListTo(compatm)%flds, trim(flds1(n)), mrg_from=compocn, mrg_fld=trim(flds1(n)), mrg_type='copy')
    end do
    deallocate(flds1)

    !=====================================================================
    ! FIELDS TO ICE
    !=====================================================================
    allocate(flds1(13))
    flds1 = (/ 'Faxa_lwdn ', &
               'Faxa_swndr', &
               'Faxa_swvdr', &
               'Faxa_swndf', &
               'Faxa_swvdf', &
               'Faxa_rain ', &
               'Faxa_snow ', &
               'Sa_z      ', &
               'Sa_pbot   ', &
               'Sa_tbot   ', &
               'Sa_u      ', &
               'Sa_v      ', &
               'Sa_shum   ' /)
    do n = 1, size(flds1)
       call addfld(fldListFr(compatm)%flds, trim(flds1(n)))
       call addfld(fldListTo(compice)%flds, trim(flds1(n)))
       call addmap(fldListFr(compatm)%flds, trim(flds1(n)), compice, mapconsf, 'one', 'unset')
       call addmrg(fldListTo(compice)%flds, trim(flds1(n)), mrg_from=compatm, mrg_fld=trim(flds1(n)), mrg_type='copy')
    end do
    deallocate(flds1)

    allocate(flds1(7))
    flds1 = (/ 'So_t      ', &
               'So_s      ', &
               'So_u      ', &
               'So_v      ', &
               'So_dhdx   ', &
               'So_dhdy   ', &
               'Fioo_q    ' /)
    do n = 1, size(flds1)
       call addfld(fldListFr(compocn)%flds, trim(flds1(n)))
       call addfld(fldListTo(compice)%flds, trim(flds1(n)))
       call addmap(fldListFr(compocn)%flds, trim(flds1(n)), compice, mapfcopy, 'unset', 'unset')
       call addmrg(fldListTo(compice)%flds, trim(flds1(n)), mrg_from=compocn, mrg_fld=trim(flds1(n)), mrg_type='copy')
    end do
    deallocate(flds1)

    allocate(flds1(1))
    flds1 = (/ 'So_omask  ' /)
    do n = 1, size(flds1)
       call addfld(fldListFr(compocn)%flds, trim(flds1(n)))
       call addfld(fldListTo(compice)%flds, trim(flds1(n)))
       call addmap(fldListFr(compocn)%flds, trim(flds1(n)), compice, mapfcopy, 'unset', 'unset')
    end do
    deallocate(flds1)

    !=====================================================================
    ! FIELDS TO MED
    !=====================================================================
    allocate(flds1(1))
    flds1 = (/ 'Sa_ofrac  ' /)
    do n = 1, size(flds1)
       call addfld(fldListFr(compatm)%flds, trim(flds1(n)))
    end do
    deallocate(flds1)

    allocate(flds1(7))
    flds1 = (/ 'Si_avsdf  ', &
               'Si_avsdr  ', &
               'Si_anidf  ', &
               'Si_anidr  ', &
               'Faii_evap ', &
               'mean_sw_pen_to_ocn', &
               'Si_imask  ' /)
    do n = 1, size(flds1)
       call addfld(fldListFr(compice)%flds, trim(flds1(n)))
    end do
    deallocate(flds1)

    !=====================================================================
    ! FIELDS TO OCN
    !=====================================================================
    allocate(flds1(1))
    flds1 = (/ 'Sa_pslv   ' /)
    do n = 1, size(flds1)
       call addfld(fldListFr(compatm)%flds, trim(flds1(n)))
       call addfld(fldListTo(compocn)%flds, trim(flds1(n)))
       call addmap(fldListFr(compatm)%flds, trim(flds1(n)), compocn, mapconsf, 'one', 'unset')
       call addmrg(fldListTo(compocn)%flds, trim(flds1(n)), mrg_from=compatm, mrg_fld=trim(flds1(n)), mrg_type='copy')
    end do
    deallocate(flds1)

    allocate(flds1(2))
    flds1 = (/ 'Faxa_rain ', &
               'Faxa_snow ' /)
    do n = 1, size(flds1)
       call addfld(fldListFr(compatm)%flds, trim(flds1(n)))
       call addfld(fldListTo(compocn)%flds, trim(flds1(n)))
       call addmap(fldListFr(compatm)%flds, trim(flds1(n)), compocn, mapconsf, 'one', 'unset')
       call addmrg(fldListTo(compocn)%flds, trim(flds1(n)), mrg_from=compatm, mrg_fld=trim(flds1(n)), mrg_type='copy_with_weights', mrg_fracname='ofrac')
    end do
    deallocate(flds1)

    allocate(flds1(4))
    flds1 = (/ 'Faxa_swndr', &
               'Faxa_swndf', &
               'Faxa_swvdr', &
               'Faxa_swvdf' /)
    do n = 1, size(flds1)
       call addfld(fldListFr(compatm)%flds, trim(flds1(n)))
       call addfld(fldListTo(compocn)%flds, trim(flds1(n)))
       call addmap(fldListFr(compatm)%flds, trim(flds1(n)), compocn, mapconsf, 'one', 'unset')
    end do
    deallocate(flds1)

    allocate(flds1(1))
    flds1 = (/ 'Faxa_lwnet' /)
    do n = 1, size(flds1)
       call addfld(fldListFr(compatm)%flds, trim(flds1(n)))
       call addfld(fldListTo(compocn)%flds, trim(flds1(n)))
       call addmap(fldListFr(compatm)%flds, trim(flds1(n)), compocn, mapconsf_aofrac, 'aofrac', 'unset')
       call addmrg(fldListTo(compocn)%flds, trim(flds1(n)), mrg_from=compatm, mrg_fld=trim(flds1(n)), mrg_type='copy_with_weights', mrg_fracname='ofrac')
    end do
    deallocate(flds1)

    allocate(flds2(2,2))
    flds2(1,:) = (/ 'Faxa_taux ', 'Foxx_taux ' /)
    flds2(2,:) = (/ 'Faxa_tauy ', 'Foxx_tauy ' /)
    do n = 1, size(flds2(:,1))
       call addfld(fldListFr(compatm)%flds, trim(flds2(n,1)))
       call addfld(fldListTo(compocn)%flds, trim(flds2(n,2)))
       call addmap(fldListFr(compatm)%flds, trim(flds2(n,1)), compocn, mapconsf_aofrac, 'aofrac', 'unset')
    end do
    deallocate(flds2)

    allocate(flds1(2))
    flds1 = (/ 'Faxa_sen  ', &
               'Faxa_lat  ' /)
    do n = 1, size(flds1)
       call addfld(fldListFr(compatm)%flds, trim(flds1(n)))
       call addfld(fldListTo(compocn)%flds, trim(flds1(n)))
       call addmap(fldListFr(compatm)%flds, trim(flds1(n)), compocn, mapconsf_aofrac, 'aofrac', 'unset')
    end do
    deallocate(flds1)

    allocate(flds1(3))
    flds1 = (/ 'Fioi_meltw', &
               'Fioi_melth', &
               'Fioi_salt ' /)
    do n = 1, size(flds1)
       call addfld(fldListFr(compice)%flds, trim(flds1(n)))
       call addfld(fldListTo(compocn)%flds, trim(flds1(n)))
       call addmap(fldListFr(compice)%flds, trim(flds1(n)), compocn, mapfcopy, 'unset', 'unset')
       call addmrg(fldListTo(compocn)%flds, trim(flds1(n)), mrg_from=compice, mrg_fld=trim(flds1(n)), mrg_type='copy_with_weights', mrg_fracname='ifrac')
    end do
    deallocate(flds1)

    allocate(flds2(6,2))
    flds2(1,:) = (/ 'Fioi_swpen_vdr', 'Foxx_swnet_vdr' /)
    flds2(2,:) = (/ 'Fioi_swpen_vdf', 'Foxx_swnet_vdf' /)
    flds2(3,:) = (/ 'Fioi_swpen_idr', 'Foxx_swnet_idr' /)
    flds2(4,:) = (/ 'Fioi_swpen_idf', 'Foxx_swnet_idf' /)
    flds2(5,:) = (/ 'Fioi_taux ', 'Foxx_taux ' /)
    flds2(6,:) = (/ 'Fioi_tauy ', 'Foxx_tauy ' /)
    do n = 1, size(flds2(:,1))
       call addfld(fldListFr(compice)%flds, trim(flds2(n,1)))
       call addfld(fldListTo(compocn)%flds, trim(flds2(n,2)))
       call addmap(fldListFr(compice)%flds, trim(flds2(n,1)), compocn, mapfcopy, 'unset', 'unset')
    end do
    deallocate(flds2)

    allocate(flds1(1))
    flds1 = (/ 'Faxa_evap ' /)
    do n = 1, size(flds1)
       call addfld(fldListTo(compocn)%flds, trim(flds1(n)))
    end do
    deallocate(flds1)

    call ESMF_LogWrite(trim(subname)//": done", ESMF_LOGMSG_INFO)

  end subroutine esmFldsExchange_nems

end module esmFldsExchange_nems_mod
