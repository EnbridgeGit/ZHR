FUNCTION ZHR_GET_MANAGER.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IMP_USERNAME) TYPE  USERNAME OPTIONAL
*"     VALUE(IMP_PERNR) TYPE  PERNR_D OPTIONAL
*"  EXPORTING
*"     VALUE(EXP_MANAGER_USER) TYPE  USERNAME
*"     VALUE(EXP_MANAGER_NUM) TYPE  PERNR_D
*"  EXCEPTIONS
*"      NOBODY_FOUND
*"      NO_MANAGER_FOUND
*"      NO_DATA_SUPPLIED
*"----------------------------------------------------------------------

  "  This function will call out to Global HR to get the manager from the
  "  org chart.

  DATA: lv_muname  TYPE username,
        lv_mpernr  TYPE p_pernr,
        lv_rfc_dest TYPE tb_rfcdest.

  IF imp_username IS INITIAL AND imp_pernr IS INITIAL.
    RAISE no_data_supplied.
  ENDIF.

  "Get RFC Destination Details
  CALL FUNCTION 'ZFI_GET_RFC_DEST'
    EXPORTING
      imp_paramtype = 'HR'
    IMPORTING
      exp_rfcdest   = lv_rfc_dest.

  CALL FUNCTION 'ZFI_GET_MANAGER_DETAILS' DESTINATION lv_rfc_dest
    EXPORTING
      imp_username     = imp_username
      imp_pernr        = imp_pernr
    IMPORTING
      exp_nwid         = lv_muname
      exp_mgr_no       = lv_mpernr
    EXCEPTIONS
      nobody_found     = 1
      no_manager_found = 2.

  IF sy-subrc = 1.
    RAISE nobody_found.
  ELSEIF sy-subrc = 2.
    RAISE no_manager_found.
  ELSEIF sy-subrc = 0.
    exp_manager_user = lv_muname.
    exp_manager_num  = lv_mpernr.
  ENDIF.
ENDFUNCTION.
