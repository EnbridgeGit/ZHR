class ZCL_BADI_CATS_APPROVAL definition
  public
  final
  create public .

public section.
*"* public components of class ZCL_BADI_CATS_APPROVAL
*"* do not include other source files here!!!

  interfaces IF_BADI_CATS_APPROVAL .
  interfaces IF_BADI_INTERFACE .
protected section.
*"* protected components of class ZCL_BADI_CATS_APPROVAL
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_BADI_CATS_APPROVAL
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_BADI_CATS_APPROVAL IMPLEMENTATION.


method IF_BADI_CATS_APPROVAL~CHECK_APPROVAL_BY_EXCEPTION.
endmethod.


METHOD if_badi_cats_approval~determine_approver.

  DATA: lv_manager TYPE username.

  CALL FUNCTION 'ZHR_GET_MANAGER'
    EXPORTING
      imp_pernr        = is_time_record-pernr
    IMPORTING
      exp_manager_user = lv_manager
    EXCEPTIONS
      nobody_found     = 1
      no_manager_found = 2
      no_data_supplied = 3
      OTHERS           = 4.

  IF sy-subrc <> 0.
    MESSAGE e000(zhr) WITH is_time_record-pernr.
  ELSE.
    rv_approver-otype = 'US'.
    rv_approver-objid = lv_manager.
  ENDIF.
ENDMETHOD.
ENDCLASS.
