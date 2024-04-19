*&---------------------------------------------------------------------*
*&  Include           ZHTVE001_MAINT_IT0105_CC_F01
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZHTVE001_MAINT_IT0105_CC                          *
*  Include:          ZHTVE001_MAINT_IT0105_CC_F01                      *
*  Author:           John Hartung                                      *
*  Date:             January 11, 2016                                  *
*  Application Area: HR TV (Travel & Expense)                          *
*                                                                      *
*  Description:      Travel & Expense InfoType 0105 SubType 0011       *
*                    Credit Card Maintenance                           *
*                                                                      *
*                    TOP Include - Subroutines                         *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 01/11/16 JRHARTUNG D30K926519 - Tkt ACR-120 Initial program          *
*----------------------------------------------------------------------*
************************************************************************

*eject
*&---------------------------------------------------------------------*
*&      Form  f_initialization
*&---------------------------------------------------------------------*
*       Initialization
*----------------------------------------------------------------------*
FORM f_initialization.

  DATA:    ls_subty                    LIKE LINE OF s_subty,
           ls_flddlm                   TYPE ty_wa_vrm_value.

* Perform authority check
  AUTHORITY-CHECK     OBJECT 'P_ORGIN'
                          ID 'AUTHC'  FIELD 'E'
                          ID 'INFTY'  FIELD gc_infty_cc
                          ID 'PERSA'  FIELD '*'
                          ID 'PERSG'  FIELD '*'
                          ID 'PERSK'  FIELD '*'
                          ID 'SUBTY'  FIELD gc_subty_cc
                          ID 'VDSK1'  FIELD '*'.
  IF     ( sy-subrc NE 0 ).
    MESSAGE  text-101 TYPE 'E'.
    LEAVE    PROGRAM.
  ENDIF.

* Set the infotype subtype
  CLEAR                                     ls_subty.
  MOVE     'I'                           TO ls_subty-sign.
  MOVE     'EQ'                          TO ls_subty-option.
  MOVE     gc_subty_cc                   TO ls_subty-low.
  APPEND                                    ls_subty
                                         TO s_subty.

* Set the delimiter table
  CLEAR                                     gt_flddlm[].
  CLEAR                                     ls_flddlm.
  MOVE     gc_fdl_comma                  TO ls_flddlm-key.
  APPEND                                    ls_flddlm
                                         TO gt_flddlm.
  CLEAR                                     ls_flddlm.
  MOVE     gc_fdl_excel                  TO ls_flddlm-key.
  APPEND                                    ls_flddlm
                                         TO gt_flddlm.
  CLEAR                                     ls_flddlm.
  MOVE     gc_fdl_tab                    TO ls_flddlm-key.
  APPEND                                    ls_flddlm
                                         TO gt_flddlm.
  CLEAR                                     ls_flddlm.
  MOVE     gc_fdl_vbar                   TO ls_flddlm-key.
  APPEND                                    ls_flddlm
                                         TO gt_flddlm.

ENDFORM.                    " f_initialization
*eject
*&---------------------------------------------------------------------*
*&      Form  f_alter_sel_screen
*&---------------------------------------------------------------------*
*       Alter the selection screen
*----------------------------------------------------------------------*
FORM f_alter_sel_screen.

  LOOP AT SCREEN.

    IF     ( screen-group1 EQ gc_modif_id_dsp ).
      screen-input = 0.
      MODIFY   SCREEN.
    ENDIF.

  ENDLOOP.

ENDFORM.                    " f_alter_sel_screen
*eject
*&---------------------------------------------------------------------*
*&      Form  f_set_field_delimiter
*&---------------------------------------------------------------------*
*       Set the selection screen field delimiter dropdown list
*----------------------------------------------------------------------*
FORM f_set_field_delimiter.

* Set the selection screen field delimiter dropdown list

  CALL FUNCTION 'VRM_SET_VALUES'
    EXPORTING
      id              = gc_flddlm
      values          = gt_flddlm
    EXCEPTIONS
      id_illegal_name = 1
      OTHERS          = 2.

  IF     ( sy-subrc NE 0 ).
    MESSAGE  e000(zfi01) WITH text-103.
  ENDIF.

ENDFORM.                    " f_set_field_delimiter
*eject
*&---------------------------------------------------------------------*
*&      Form  f_validate_selection_screen
*&---------------------------------------------------------------------*
*       Check the selection-screen
*----------------------------------------------------------------------*
FORM f_validate_selection_screen.

  DATA:    ls_flddlm                   TYPE ty_wa_vrm_value.

* Validate the effective as of date
  IF     ( p_efcda   IS INITIAL ).
    MESSAGE  s000(zfi01) WITH text-102 DISPLAY LIKE 'E'.
    LEAVE    LIST-PROCESSING.
  ENDIF.

* Validate the new end date to be assigned
  IF     ( p_endd_p  IS INITIAL ).
    MESSAGE  s000(zfi01) WITH text-103 DISPLAY LIKE 'E'.
    LEAVE    LIST-PROCESSING.
  ENDIF.

* Validate the filename
  IF     ( p_fname   IS INITIAL ).
    MESSAGE  s000(zfi01) WITH text-104 DISPLAY LIKE 'E'.
    LEAVE    LIST-PROCESSING.
  ENDIF.

* Validate the file delimiter
  CLEAR                                     ls_flddlm.
  READ     TABLE gt_flddlm             INTO ls_flddlm
                                   WITH KEY key = p_flddlm.
  IF     ( sy-subrc NE 0 ).
    MESSAGE  s000(zfi01) WITH text-105 DISPLAY LIKE 'E'.
    LEAVE    LIST-PROCESSING.
  ENDIF.

  IF   ( ( p_flddlm                      EQ gc_fdl_excel ) AND
         ( rb_appl                       EQ abap_true    )     ).
    MESSAGE  s000(zfi01) WITH text-106 DISPLAY LIKE 'E'.
    LEAVE    LIST-PROCESSING.
  ENDIF.

ENDFORM.                    " f_validate_selection_screen
*eject
*&---------------------------------------------------------------------*
*&      Form  f_initial_data_elements
*&---------------------------------------------------------------------*
*       Initial the data elements
*----------------------------------------------------------------------*
FORM f_initial_data_elements.

* Initial global internal tables
  CLEAR    gt_errs_data[].
  CLEAR    gt_errs_proc[].

* Initial global variables
  CLEAR    gv_count_err_data.
  CLEAR    gv_count_err_proc.
  CLEAR    gv_flag_err_data.
  CLEAR    gv_flag_err_proc.
  CLEAR    gv_flag_err_mstr.

* Set the filename
  CLEAR                                     gv_filename.
  MOVE     p_fname                       TO gv_filename.

* Set the file delimiter
  CLEAR    gv_fd_lit.
  CLEAR    gv_fd_val.

  CASE     p_flddlm.
    WHEN     gc_fdl_comma.
      MOVE     gc_fdl_comma              TO gv_fd_lit.
      MOVE     gc_fd_comma               TO gv_fd_val.
    WHEN     gc_fdl_excel.
      MOVE     gc_fdl_excel              TO gv_fd_lit.
      MOVE     gc_fd_excel               TO gv_fd_val.
    WHEN     gc_fdl_tab.
      MOVE     gc_fdl_tab                TO gv_fd_lit.
      MOVE     gc_fd_tab                 TO gv_fd_val.
    WHEN     gc_fdl_vbar.
      MOVE     gc_fdl_vbar               TO gv_fd_lit.
      MOVE     gc_fd_vbar                TO gv_fd_val.
    WHEN     OTHERS.
  ENDCASE.

ENDFORM.                    " f_initial_data_elements
*eject
*&---------------------------------------------------------------------*
*&      Form  f_process_main
*&---------------------------------------------------------------------*
*       Main process
*----------------------------------------------------------------------*
FORM f_process_main.

  DATA:    lt_data_file                TYPE ty_it_data_file,
           lt_data_mstr                TYPE ty_it_pa0105_ext.

  IF     ( gv_flag_err_proc IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lt_data_file[].
  CLEAR    lt_data_mstr[].

* Get the input data
  IF     ( rb_pres                       IS NOT INITIAL ).

* Upload the data from a file on the presentation server
    PERFORM  f_upload_data         TABLES   lt_data_file.

  ELSEIF ( rb_appl                       IS NOT INITIAL ).

* Read the data from a file on the application server
    PERFORM  f_read_data           TABLES   lt_data_file.

  ENDIF.

* Validate the data
  PERFORM  f_validate_data         TABLES   lt_data_file
                                            lt_data_mstr.

* Filter the data using the selection criteria
  PERFORM  f_filter_data           TABLES   lt_data_mstr.

* Post the data
  PERFORM  f_post_data             TABLES   lt_data_mstr.

* Report the results
  PERFORM  f_report_results        TABLES   lt_data_file
                                            lt_data_mstr.

ENDFORM.                    " f_process_main
*eject
*&---------------------------------------------------------------------*
*&      Form  f_upload_data
*&---------------------------------------------------------------------*
*       Upload the data from a file on the presentation server
*----------------------------------------------------------------------*
FORM f_upload_data
  TABLES   ct_data_file                TYPE ty_it_data_file.

  TYPES:   ty_wa_intern                TYPE kcde_intern_struc,
           ty_it_intern                TYPE STANDARD TABLE
                                         OF ty_wa_intern.

  DATA:    ls_data_file                TYPE ty_wa_data_file,
           lt_data_file                TYPE ty_it_data_file,
           ls_data_file_p              TYPE ty_wa_data_file,
           lt_data_file_p              TYPE ty_it_data_file,
           ls_intern                   TYPE ty_wa_intern,
           lt_intern                   TYPE ty_it_intern,
           lt_itab                     TYPE truxs_t_text_data.

  DATA:    lv_rc                       TYPE numc5,
           lv_stripped_name            TYPE STRING,
           lv_file_path                TYPE STRING,
           lv_result                   TYPE abap_bool.

  CLEAR    ct_data_file[].

  IF     ( gv_flag_err_proc IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lt_data_file[].
  CLEAR    lt_data_file_p[].
  CLEAR    lt_intern[].
  CLEAR    lt_itab[].

  CLEAR    lv_stripped_name.
  CLEAR    lv_file_path.
  CLEAR    lv_result.

* Check the filepath\name
  CLEAR    lv_rc.

  IF     ( gv_filename                   IS INITIAL ).

    PERFORM  f_error_in_process    USING    0         lv_rc
                                            'ZFI01'   'E'       131
                                            text-131  SPACE
                                            SPACE     SPACE.

    RETURN.
  ENDIF.

*eject

  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      FULL_NAME     = gv_filename
    IMPORTING
      STRIPPED_NAME = lv_stripped_name
      FILE_PATH     = lv_file_path
    EXCEPTIONS
      X_ERROR       = 1
      OTHERS        = 2.

  lv_rc = sy-subrc.

  IF   ( ( lv_rc                         NE 0       ) OR
         ( lv_stripped_name              IS INITIAL ) OR
         ( lv_file_path                  IS INITIAL )    ).

    PERFORM  f_error_in_process    USING    0         lv_rc
                                            'ZFI01'   'E'       131
                                            text-131  SPACE
                                            SPACE     SPACE.

    RETURN.
  ENDIF.

  IF   ( ( lv_file_path CS 'C:' )      OR ( lv_file_path CS 'c:' ) OR
         ( lv_file_path CS 'C$' )      OR ( lv_file_path CS 'c$' ) ).

    PERFORM  f_error_in_process    USING    0         lv_rc
                                            'ZFI01'   'E'       132
                                            text-132  SPACE
                                            SPACE     SPACE.

    RETURN.
  ENDIF.

*eject

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>DIRECTORY_EXIST
    EXPORTING
      DIRECTORY            = lv_file_path
    RECEIVING
      RESULT               = lv_result
    EXCEPTIONS
      CNTL_ERROR           = 1
      ERROR_NO_GUI         = 2
      WRONG_PARAMETER      = 3
      NOT_SUPPORTED_BY_GUI = 4
      others               = 5.

  lv_rc = sy-subrc.

  IF   ( ( lv_rc NE 0 ) OR ( lv_result IS INITIAL ) ).

    PERFORM  f_error_in_process    USING    0         lv_rc
                                            'ZFI01'   'E'       131
                                            text-131  gv_filename
                                            SPACE     SPACE.

    RETURN.
  ENDIF.

*eject
  CLEAR    lv_rc.

* Upload an EXCEL file
  IF     ( gv_fd_lit                     EQ gc_fdl_excel ).

    CALL FUNCTION 'KCD_EXCEL_OLE_TO_INT_CONVERT'
      EXPORTING
        filename                = gv_filename
        i_begin_col             = 1
        i_begin_row             = 1
        i_end_col               = 1
        i_end_row               = 99999
      TABLES
        intern                  = lt_intern
      EXCEPTIONS
        inconsistent_parameters = 1
        upload_ole              = 2
        OTHERS                  = 3.

    lv_rc = sy-subrc.

    IF     ( lv_rc NE 0 ).

      CALL FUNCTION 'POPUP_FOR_INTERACTION'
        EXPORTING
          HEADLINE = text-135
          TEXT1    = text-136
          TEXT2    = text-138
          TEXT3    = ' '
          TEXT4    = text-139
          BUTTON_1 = 'OK'.

      PERFORM  f_error_in_process  USING    0         lv_rc
                                            'ZFI01'   'E'       136
                                            text-136  gv_filename
                                            SPACE     SPACE.

      RETURN.
    ENDIF.

*eject
* Transfer the data from the EXCEL table to the internal data table
    SORT     lt_intern         ASCENDING BY row col.

    CLEAR    ls_data_file.

    CLEAR                                   ls_intern.
    LOOP AT  lt_intern                 INTO ls_intern.

      CASE   ls_intern-col.
        WHEN  1.  MOVE ls_intern-value   TO ls_data_file-col01.
        WHEN  2.  MOVE ls_intern-value   TO ls_data_file-col02.
        WHEN  3.  MOVE ls_intern-value   TO ls_data_file-col03.
        WHEN  4.  MOVE ls_intern-value   TO ls_data_file-col04.
        WHEN  5.  MOVE ls_intern-value   TO ls_data_file-col05.
        WHEN  6.  MOVE ls_intern-value   TO ls_data_file-col06.
        WHEN  7.  MOVE ls_intern-value   TO ls_data_file-col07.
        WHEN OTHERS.
      ENDCASE.

      AT END OF row.
        APPEND                              ls_data_file
                                         TO lt_data_file.
        CLEAR                               ls_data_file.
      ENDAT.

      CLEAR  ls_intern.
    ENDLOOP.

*eject
* Upload a delimited file
  ELSE.

    CALL FUNCTION 'TEXT_CONVERT_XLS_TO_SAP'
      EXPORTING
        I_FIELD_SEPERATOR    = gc_fd_tab
*       I_LINE_HEADER        =
        I_TAB_RAW_DATA       = lt_itab
        I_FILENAME           = gv_filename
      TABLES
        I_TAB_CONVERTED_DATA = lt_data_file
      EXCEPTIONS
        CONVERSION_FAILED    = 1
        OTHERS               = 2.

    lv_rc = sy-subrc.

    IF     ( lv_rc NE 0 ).

      CALL FUNCTION 'POPUP_FOR_INTERACTION'
        EXPORTING
          HEADLINE = text-135
          TEXT1    = text-137
          TEXT2    = text-138
          TEXT3    = ' '
          TEXT4    = text-139
          BUTTON_1 = 'OK'.

      PERFORM  f_error_in_process  USING    0         lv_rc
                                            'ZFI01'   'E'       137
                                            text-137  gv_filename
                                            SPACE     SPACE.

      RETURN.
    ENDIF.

  ENDIF.

*eject
  IF   ( ( gv_fd_lit                     EQ gc_fdl_comma ) OR
         ( gv_fd_lit                     EQ gc_fdl_vbar  )    ).

    CLEAR                                   ls_data_file.
    LOOP AT  lt_data_file              INTO ls_data_file.
      CLEAR                                 ls_data_file_p.
      SPLIT  ls_data_file-col01          AT gv_fd_val
                                       INTO ls_data_file_p-col01
                                            ls_data_file_p-col02
                                            ls_data_file_p-col03
                                            ls_data_file_p-col04
                                            ls_data_file_p-col05
                                            ls_data_file_p-col06
                                            ls_data_file_p-col07.
      APPEND                                ls_data_file_p
                                         TO lt_data_file_p.
      CLEAR  ls_data_file.
    ENDLOOP.

    ct_data_file[] = lt_data_file_p[].

    RETURN.

  ENDIF.

  ct_data_file[] = lt_data_file[].

ENDFORM.                    " f_upload_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_read_data
*&---------------------------------------------------------------------*
*       Read the data from a file on the application server
*----------------------------------------------------------------------*
FORM f_read_data
  TABLES   ct_data_file                TYPE ty_it_data_file.

  DATA:    ls_data_file                TYPE ty_wa_data_file,
           lt_data_file                TYPE ty_it_data_file,
           ls_data_file_p              TYPE ty_wa_data_file,
           lt_data_file_p              TYPE ty_it_data_file.

  DATA:    lv_rc                       TYPE numc5,
           lv_stripped_name            TYPE STRING,
           lv_file_path                TYPE STRING,
           lv_text                     TYPE text1000.

  CLEAR    ct_data_file[].

  IF     ( gv_flag_err_proc IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lv_stripped_name.
  CLEAR    lv_file_path.

* Check the filepath\name
  CLEAR    lv_rc.

  IF     ( gv_filename                   IS INITIAL ).

    PERFORM  f_error_in_process    USING    0         lv_rc
                                            'ZFI01'   'E'       131
                                            text-131  SPACE
                                            SPACE     SPACE.

    RETURN.
  ENDIF.

*eject

  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      FULL_NAME     = gv_filename
    IMPORTING
      STRIPPED_NAME = lv_stripped_name
      FILE_PATH     = lv_file_path
    EXCEPTIONS
      X_ERROR       = 1
      OTHERS        = 2.

  lv_rc = sy-subrc.

  IF   ( ( lv_rc                         NE 0       ) OR
         ( lv_stripped_name              IS INITIAL ) OR
         ( lv_file_path                  IS INITIAL )    ).

    PERFORM  f_error_in_process    USING    0         lv_rc
                                            'ZFI01'   'E'       131
                                            text-131  gv_filename
                                            SPACE     SPACE.

    RETURN.
  ENDIF.

*eject
* Open the file in input mode
  OPEN     DATASET gv_filename FOR INPUT IN TEXT MODE ENCODING DEFAULT.

  lv_rc = sy-subrc.

  IF ( lv_rc NE 0 ).

    PERFORM  f_error_in_process    USING    0         lv_rc
                                            'ZFI01'   'E'       141
                                            text-141  gv_filename
                                            SPACE     SPACE.

    RETURN.
  ENDIF.

* Read the data
  DO.
    CLEAR                                   lv_text.
    READ     DATASET gv_filename       INTO lv_text.
    IF     ( sy-subrc EQ 0 ).
      CLEAR                                 ls_data_file.
      MOVE     lv_text                   TO ls_data_file-col01.
      APPEND                                ls_data_file
                                         TO lt_data_file.
    ELSE.
      EXIT.
    ENDIF.
  ENDDO.

* Close the file
  CLOSE    DATASET gv_filename.

*eject
  IF   ( ( gv_fd_lit                     EQ gc_fdl_comma ) OR
         ( gv_fd_lit                     EQ gc_fdl_tab   ) OR
         ( gv_fd_lit                     EQ gc_fdl_vbar  )    ).

    CLEAR                                   ls_data_file.
    LOOP AT  lt_data_file              INTO ls_data_file.
      CLEAR                                 ls_data_file_p.
      SPLIT  ls_data_file-col01          AT gv_fd_val
                                       INTO ls_data_file_p-col01
                                            ls_data_file_p-col02
                                            ls_data_file_p-col03
                                            ls_data_file_p-col04
                                            ls_data_file_p-col05
                                            ls_data_file_p-col06
                                            ls_data_file_p-col07.
      APPEND                                ls_data_file_p
                                         TO lt_data_file_p.
      CLEAR  ls_data_file.
    ENDLOOP.

    ct_data_file[] = lt_data_file_p[].

    RETURN.

  ENDIF.

  ct_data_file[] = lt_data_file[].

ENDFORM.                    " f_read_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_validate_data
*&---------------------------------------------------------------------*
*       Validate the data
*----------------------------------------------------------------------*
FORM f_validate_data
  TABLES   it_data_file                TYPE ty_it_data_file
           ct_data_mstr                TYPE ty_it_pa0105_ext.

  DATA:    ls_data_file                TYPE ty_wa_data_file,
           ls_data_mstr                TYPE ty_wa_pa0105_ext,
           lt_data_mstr                TYPE ty_it_pa0105_ext.

  DATA:    lv_tabix                    TYPE sytabix,
           lv_strln1                   TYPE syindex,
           lv_strln2                   TYPE syindex,
           lv_flag_valid               TYPE flag,
           lv_text1                    TYPE text240,
           lv_text2                    TYPE text200,
           lv_pernr                    TYPE persno,
           lv_pernr_c                  TYPE char8,
           lv_pernr_p                  TYPE persno.

  CLEAR    ct_data_mstr[].

  IF     ( gv_flag_err_proc IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                     ls_data_file.
  LOOP AT  it_data_file                INTO ls_data_file.
    lv_tabix = sy-tabix.

    IF     ( ls_data_file                IS INITIAL ).
      DELETE it_data_file             INDEX lv_tabix.
      CONTINUE.
    ENDIF.

    CLEAR    lv_text1.
    CLEAR    lv_text2.

*eject
* Validate the personnel number
    CLEAR                                   lv_flag_valid.
    MOVE     abap_true                   TO lv_flag_valid.

    SHIFT    ls_data_file-col01        LEFT DELETING LEADING SPACE.

    lv_strln1 = STRLEN( ls_data_file-col01 ).

    IF   ( ( lv_strln1                   LT 1            ) OR
           ( lv_strln1                   GT 8            ) OR
           ( ls_data_file-col01          CN '0123456789' )    ).

      CLEAR    lv_flag_valid.

    ElSE.

      CLEAR                                 lv_pernr_c.
      MOVE   ls_data_file-col01          TO lv_pernr_c.
      SHIFT                                 lv_pernr_c
                                      RIGHT DELETING TRAILING SPACE.
      TRANSLATE                             lv_pernr_c
                                      USING ' 0'.
      CLEAR                                 lv_pernr.
      MOVE   lv_pernr_c                  TO lv_pernr.

      CLEAR    lv_pernr_p.
      SELECT   pernr
        INTO   lv_pernr_p
        FROM   pa0000 UP TO 1 ROWS
       WHERE   pernr = lv_pernr.
      ENDSELECT.
      IF     ( sy-subrc NE 0 ).
        CLEAR  lv_flag_valid.
      ENDIF.

    ENDIF.

    IF     ( lv_flag_valid               IS INITIAL ).
      IF   ( lv_text2                    IS INITIAL ).
        CONCATENATE                         text-181
                                            ls_data_file-col01
                                       INTO lv_text2.
      ELSE.
        CONCATENATE                         lv_text2 ' * '
                                            text-181
                                            ls_data_file-col01
                                       INTO lv_text2.
      ENDIF.
    ENDIF.

* Validate the subtype
* Validate the end date
* Validate the start date
* Validate the sequence number
* Validate the credit card number

*eject
* Create the master data entry or the data error entry
    IF     ( lv_text2                    IS INITIAL ).

      CLEAR                                 ls_data_mstr.
      MOVE     lv_pernr                  TO ls_data_mstr-pernr.
      MOVE     gc_subty_cc               TO ls_data_mstr-subty.
      APPEND                                ls_data_mstr
                                         TO lt_data_mstr.

    ELSE.

      CLEAR                                 lv_text1.
      CONCATENATE                           ls_data_file-col01
                                            ls_data_file-col02
                                            ls_data_file-col03
                                            ls_data_file-col04
                                            ls_data_file-col05
                                            ls_data_file-col06
                                            ls_data_file-col07
                                       INTO lv_text1
                               SEPARATED BY gc_period.

      lv_strln1 = STRLEN( lv_text1 ).

      IF ( lv_strln1 LT 50 ).

        lv_strln2 = 50 - lv_strln1.

        MOVE   gc_periods(lv_strln2) TO lv_text1+lv_strln1(lv_strln2).

      ENDIF.

      PERFORM  f_error_in_data     USING    0         4
                                            'ZFI01'   'E'       180
                                            lv_text1+000(50)
                                            lv_text2+000(50)
                                            lv_text2+050(50)
                                            lv_text2+100(50).

    ENDIF.

    CLEAR  ls_data_file.
  ENDLOOP.

  SORT     lt_data_mstr ASCENDING.
  DELETE   ADJACENT DUPLICATES FROM lt_data_mstr.

  ct_data_mstr[] = lt_data_mstr[].

ENDFORM.                    " f_validate_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_filter_data
*&---------------------------------------------------------------------*
*       Filter the data using the selection criteria
*----------------------------------------------------------------------*
FORM f_filter_data
  TABLES   ct_data_mstr                TYPE ty_it_pa0105_ext.

  DATA:    lt_data_mstr                TYPE ty_it_pa0105_ext,
           lt_data_mstr_p              TYPE ty_it_pa0105_ext,
           ls_data_slct                TYPE ty_wa_pa0105_ext,
           lt_data_slct                TYPE ty_it_pa0105_ext,
           lt_data_slct_d              TYPE ty_it_pa0105_ext,
           lt_data_slct_k              TYPE ty_it_pa0105_ext,
           lt_data_slct_p              TYPE ty_it_pa0105_ext.

  DATA:    lv_index                    TYPE syindex,
           lv_index_lo                 TYPE syindex,
           lv_index_hi                 TYPE syindex.

  CONSTANTS:
           lc_blocksize                TYPE syindex
                                       VALUE 100.

  IF     ( gv_flag_err_proc IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CLEAR    lt_data_mstr[].
  CLEAR    lt_data_mstr_p[].
  CLEAR    lt_data_slct[].
  CLEAR    lt_data_slct_d[].
  CLEAR    lt_data_slct_k[].
  CLEAR    lt_data_slct_p[].

  lt_data_mstr[] = ct_data_mstr[].

*eject
* Process the cleared items in batches
  DO.

* Calculate the low and high indices for the batch
    lv_index    =     sy-index.
    lv_index_lo = ( ( lv_index - 1 ) * lc_blocksize ) + 1.
    lv_index_hi = (   lv_index       * lc_blocksize ).

* Build the batch
    CLEAR             lt_data_mstr_p[].
    APPEND   LINES OF lt_data_mstr
                 FROM lv_index_lo
                   TO lv_index_hi
                   TO lt_data_mstr_p.

    IF     ( lt_data_mstr_p[] IS INITIAL ).
      EXIT.
    ENDIF.

* Select the credit cards
    CLEAR    lt_data_slct[].
    SELECT   *
      INTO   TABLE lt_data_slct
      FROM   pa0105 FOR ALL ENTRIES IN lt_data_mstr_p
     WHERE   pernr = lt_data_mstr_p-pernr
       AND   subty = gc_subty_cc.
    IF     ( sy-subrc NE 0 ).
      CLEAR  lt_data_slct[].
    ENDIF.

* Filter the batch using the selection criteria
    CLEAR    lt_data_slct_d[].
    CLEAR    lt_data_slct_k[].

    CLEAR                                   ls_data_slct.
    LOOP AT  lt_data_slct              INTO ls_data_slct.

      IF   ( ( ls_data_slct-pernr        IN s_pernr ) AND
             ( ls_data_slct-subty        IN s_subty ) AND
             ( ls_data_slct-objps        IN s_objps ) AND
             ( ls_data_slct-sprps        IN s_sprps ) AND
             ( ls_data_slct-endda        IN s_endda ) AND
             ( ls_data_slct-begda        IN s_begda ) AND
             ( ls_data_slct-seqnr        IN s_seqnr )     ).
        IF ( ( ls_data_slct-endda        GE p_efcda ) AND
             ( ls_data_slct-begda        LE p_efcda )     ).
          APPEND ls_data_slct            TO lt_data_slct_k.
        ELSE.
          APPEND ls_data_slct            TO lt_data_slct_d.
        ENDIF.
      ENDIF.

      CLEAR  ls_data_slct.
    ENDLOOP.

*eject
* Identify the inactive credit cards; effective date out of range
    SORT     lt_data_slct_d    ASCENDING BY pernr usrid.
    DELETE         ADJACENT DUPLICATES FROM lt_data_slct_d
                                  COMPARING pernr usrid.

    CLEAR                                   ls_data_slct.
    LOOP AT  lt_data_slct_d            INTO ls_data_slct.
      READ   TABLE lt_data_slct_k  WITH KEY pernr = ls_data_slct-pernr
                                            usrid = ls_data_slct-usrid
                     TRANSPORTING NO FIELDS.
      IF     ( sy-subrc NE 0 ).

        PERFORM  f_error_in_data   USING    0         4
                                            'ZFI01'   'E'       185
                                            text-185
                                            ls_data_slct-pernr
                                            ls_data_slct-usrid
                                            SPACE.

      ENDIF.
      CLEAR  ls_data_slct.
    ENDLOOP.

* If there are multiple effective entries for a given personnel number
* and credit card combination, then determine the entry to update
    PERFORM  f_determine_update_entry
                                   TABLES   lt_data_slct_k.

    IF     ( lt_data_slct_k[]            IS NOT INITIAL ).
      APPEND   LINES OF lt_data_slct_k   TO lt_data_slct_p.
    ENDIF.

  ENDDO.

  CLEAR    ct_data_mstr[].

  ct_data_mstr[] = lt_data_slct_p[].

ENDFORM.                    " f_filter_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_determine_update_entry
*&---------------------------------------------------------------------*
*       If there are multiple effective entries for a given personnel
*       number and credit card combination, then determine the entry
*       to update
*----------------------------------------------------------------------*
FORM f_determine_update_entry
  TABLES   ct_data_slct                TYPE ty_it_pa0105_ext.

  DATA:    ls_data_slct                TYPE ty_wa_pa0105_ext,
           ls_data_slct_p              TYPE ty_wa_pa0105_ext,
           lt_data_slct_p              TYPE ty_it_pa0105_ext.

  DATA:    lv_lines1                   TYPE syindex,
           lv_lines2                   TYPE syindex.

  IF     ( ct_data_slct[]                IS INITIAL ).
    RETURN.
  ENDIF.

  DESCRIBE TABLE ct_data_slct         LINES lv_lines1.

  CLEAR    lt_data_slct_p[].
  CLEAR    ls_data_slct_p.

  SORT     ct_data_slct        ASCENDING BY pernr subty usrid objps
                                            sprps endda begda seqnr.

  CLEAR                                     ls_data_slct.
  LOOP AT  ct_data_slct                INTO ls_data_slct.

    IF       ( ( ls_data_slct-pernr      NE ls_data_slct_p-pernr ) OR
               ( ls_data_slct-subty      NE ls_data_slct_p-subty ) OR
               ( ls_data_slct-usrid      NE ls_data_slct_p-usrid )   ).

      IF     ( ( ls_data_slct_p-pernr    IS NOT INITIAL ) AND
               ( ls_data_slct_p-usrid    IS NOT INITIAL )     ).
        APPEND   ls_data_slct_p          TO lt_data_slct_p.
      ENDIF.

      CLEAR                                 ls_data_slct_p.
      MOVE       ls_data_slct            TO ls_data_slct_p.

    ELSEIF ( ( ( ls_data_slct-endda      GT ls_data_slct_p-endda ) ) OR
             ( ( ls_data_slct-endda      EQ ls_data_slct_p-endda ) AND
               ( ls_data_slct-seqnr      LT ls_data_slct_p-seqnr ) ) ).

      CLEAR                                 ls_data_slct_p.
      MOVE       ls_data_slct            TO ls_data_slct_p.

    ENDIF.

    CLEAR  ls_data_slct.
  ENDLOOP.

*eject
  IF     ( ( ls_data_slct_p-pernr        IS NOT INITIAL ) AND
           ( ls_data_slct_p-usrid        IS NOT INITIAL )     ).
    APPEND   ls_data_slct_p              TO lt_data_slct_p.
  ENDIF.

  DESCRIBE TABLE lt_data_slct_p       LINES lv_lines2.

  IF     ( lv_lines1                     EQ lv_lines2 ).
    RETURN.
  ENDIF.

  CLEAR    ct_data_slct[].

  ct_data_slct[] = lt_data_slct_p[].

ENDFORM.                    " f_determine_update_entry
*eject
*&---------------------------------------------------------------------*
*&      Form  f_post_data
*&---------------------------------------------------------------------*
*       Post the data
*----------------------------------------------------------------------*
FORM f_post_data
  TABLES   ct_data_mstr                TYPE ty_it_pa0105_ext.

  DATA:    ls_data_mstr                TYPE ty_wa_pa0105_ext,
           ls_record                   TYPE p0105.

  DATA:    lv_subrc                    TYPE sysubrc,
           lv_tabix                    TYPE sytabix,
           lv_infty                    TYPE infty.

  IF     ( gv_flag_err_proc IS NOT INITIAL ).
    RETURN.
  ENDIF.

  CLEAR                                     lv_infty.
  MOVE     gc_infty_cc                   TO lv_infty.

* Maintain the infotype
  CLEAR                                     ls_data_mstr.
  LOOP AT  ct_data_mstr                INTO ls_data_mstr.
    lv_tabix = sy-tabix.

    IF       ( ls_data_mstr-endda        EQ p_endd_p ).
      CLEAR    ls_data_mstr.
      CONTINUE.
    ENDIF.

    CLEAR                                   ls_record.
    MOVE-CORRESPONDING ls_data_mstr      TO ls_record.
    CLEAR                                   ls_record-infty.
    MOVE     lv_infty                    TO ls_record-infty.
    CLEAR                                   ls_record-endda.
    MOVE     p_endd_p                    TO ls_record-endda.
    CLEAR                                   ls_record-aedtm.
    MOVE     sy-datum                    TO ls_record-aedtm.
    CLEAR                                   ls_record-uname.
    MOVE     sy-uname                    TO ls_record-uname.

*eject
    CLEAR    lv_subrc.

    PERFORM  f_maintain_infotype   USING    ls_record
                                            lv_infty
                                            ls_data_mstr-pernr
                                            ls_data_mstr-subty
                                            ls_data_mstr-endda
                                            ls_data_mstr-begda
                                            ls_data_mstr-seqnr
                                   CHANGING lv_subrc.

    IF     ( lv_subrc EQ 0 ).
      CLEAR                                 ls_data_mstr-endda.
      MOVE     ls_record-endda           TO ls_data_mstr-endda.
      CLEAR                                 ls_data_mstr-aedtm.
      MOVE     ls_record-aedtm           TO ls_data_mstr-aedtm.
      CLEAR                                 ls_data_mstr-uname.
      MOVE     ls_record-uname           TO ls_data_mstr-uname.
      CLEAR                                 ls_data_mstr-flag_posted.
      MOVE     abap_true                 TO ls_data_mstr-flag_posted.
      MODIFY                                ct_data_mstr
                                       FROM ls_data_mstr
                                      INDEX lv_tabix
                               TRANSPORTING endda aedtm uname
                                            flag_posted.
    ENDIF.

    CLEAR  ls_data_mstr.
  ENDLOOP.

ENDFORM.                    " f_post_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_maintain_infotype
*&---------------------------------------------------------------------*
*       Maintain the infotype
*----------------------------------------------------------------------*
FORM f_maintain_infotype
  USING    is_record                   TYPE p0105
           iv_infty                    TYPE infty
           iv_pernr                    TYPE pernr_d
           iv_subty                    TYPE subty
           iv_endda                    TYPE endda
           iv_begda                    TYPE begda
           iv_seqnr                    TYPE seqnr
  CHANGING cv_subrc                    TYPE sysubrc.

  DATA:    ls_return                   TYPE bapireturn1.

  DATA:    lv_operation                TYPE actio,
           lv_tclas                    TYPE tclas,
           lv_dialog_mode              TYPE c,
           lv_nocommit                 TYPE bapi_ncomt,
           lv_usrid                    TYPE sysid,
           lv_text                     TYPE char50.

  CLEAR    cv_subrc.

  CLEAR                                     lv_operation.
  MOVE     'MOD'                         TO lv_operation.
  CLEAR                                     lv_tclas.
  MOVE     'A'                           TO lv_tclas.
  CLEAR                                     lv_dialog_mode.
  MOVE     '0'                           TO lv_dialog_mode.
  CLEAR                                     lv_nocommit.

  IF     ( cb_test                       IS NOT INITIAL ).
    MOVE   abap_true                     TO lv_nocommit.
  ELSE.
    MOVE   abap_false                    TO lv_nocommit.
  ENDIF.

  CLEAR                                     lv_usrid.
  MOVE     is_record-usrid               TO lv_usrid.

  CLEAR                                     lv_text.
  CONCATENATE                               iv_infty iv_pernr iv_subty
                                            iv_endda iv_begda iv_seqnr
                                            lv_usrid
                                       INTO lv_text
                               SEPARATED BY '-'.

*eject
* Enqueue the personnel number
  CLEAR    ls_return.

  CALL FUNCTION 'BAPI_EMPLOYEE_ENQUEUE'
    EXPORTING
      number = iv_pernr
    IMPORTING
      return = ls_return.

  IF   ( ( ls_return-type EQ gc_a ) OR ( ls_return-type EQ gc_e ) ).

    cv_subrc = 4.

    PERFORM  f_error_in_data       USING    190       4
                                            'ZFI01'   'E'       190
                                            text-191  lv_text
                                            SPACE     SPACE.

    PERFORM  f_error_in_data       USING    190       4
                                            ls_return-id
                                            ls_return-type
                                            ls_return-number
                                            ls_return-message_v1
                                            ls_return-message_v2
                                            ls_return-message_v3
                                            ls_return-message_v4.

    RETURN.
  ENDIF.

*eject
* Modify the infotype record
  CLEAR    ls_return.

  CALL FUNCTION 'HR_INFOTYPE_OPERATION'
    EXPORTING
      infty         = iv_infty
      number        = iv_pernr
      subtype       = iv_subty
      validityend   = iv_endda
      validitybegin = iv_begda
      recordnumber  = iv_seqnr
      record        = is_record
      operation     = lv_operation
      tclas         = lv_tclas
      dialog_mode   = lv_dialog_mode
      nocommit      = lv_nocommit
    IMPORTING
      return        = ls_return.

  IF   ( ( ls_return-type EQ gc_a ) OR ( ls_return-type EQ gc_e ) ).

    cv_subrc = 4.

    PERFORM  f_error_in_data       USING    190       4
                                            'ZFI01'   'E'       190
                                            text-193  lv_text
                                            SPACE     SPACE.

    PERFORM  f_error_in_data       USING    190       4
                                            ls_return-id
                                            ls_return-type
                                            ls_return-number
                                            ls_return-message_v1
                                            ls_return-message_v2
                                            ls_return-message_v3
                                            ls_return-message_v4.

  ENDIF.

*eject
* Dequeue the personnel number
  CLEAR    ls_return.

  CALL FUNCTION 'BAPI_EMPLOYEE_DEQUEUE'
    EXPORTING
      number = iv_pernr
    IMPORTING
      return = ls_return.

  IF   ( ( ls_return-type EQ gc_a ) OR ( ls_return-type EQ gc_e ) ).

    CLEAR    cv_subrc.

    PERFORM  f_error_in_data       USING    190       4
                                            'ZFI01'   'E'       190
                                            text-192  lv_text
                                            SPACE     SPACE.

    PERFORM  f_error_in_data       USING    190       4
                                            ls_return-id
                                            ls_return-type
                                            ls_return-number
                                            ls_return-message_v1
                                            ls_return-message_v2
                                            ls_return-message_v3
                                            ls_return-message_v4.

  ENDIF.

  CALL FUNCTION 'HR_PSBUFFER_INITIALIZE'.

ENDFORM.                    " f_maintain_infotype
*eject
*&---------------------------------------------------------------------*
*&      Form  f_error_in_process
*&---------------------------------------------------------------------*
*       Append an error to the process error table
*----------------------------------------------------------------------*
FORM f_error_in_process
  USING    iv_nbr_doc                  TYPE numc5
           iv_rc                       TYPE numc5
           iv_msgid                    TYPE symsgid
           iv_msgty                    TYPE symsgty
           iv_msgno                    TYPE any
           iv_msgv1                    TYPE any
           iv_msgv2                    TYPE any
           iv_msgv3                    TYPE any
           iv_msgv4                    TYPE any.

  DATA:    lv_type_fld                 TYPE char1,
           lv_msgno_c                  TYPE char3,
           lv_msgno                    TYPE symsgno,
           lv_text                     TYPE text240.

  DATA:    ls_errs_proc                TYPE ty_wa_errs.

  IF     ( iv_msgty                      CA 'AaEe' ).
    ADD    1                             TO gv_count_err_proc.
    CLEAR                                   gv_flag_err_proc.
    MOVE   gc_x                          TO gv_flag_err_proc.
    CLEAR                                   gv_flag_err_mstr.
    MOVE   gc_x                          TO gv_flag_err_mstr.
  ENDIF.

  CLEAR    lv_type_fld.
  CLEAR    lv_msgno_c.
  CLEAR    lv_msgno.
  CLEAR    lv_text.

  DESCRIBE FIELD iv_msgno              TYPE lv_type_fld.

  IF           ( lv_type_fld             CS gc_c ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_msgno
      IMPORTING
        output = lv_msgno_c.

    IF         ( lv_msgno_c              CO '0123456789' ).
      MOVE       lv_msgno_c              TO lv_msgno.
    ENDIF.
  ELSEIF       ( lv_type_fld             CA 'IN' ).
    MOVE         iv_msgno                TO lv_msgno.
  ENDIF.

*eject
  IF   ( iv_msgid IS INITIAL ).

    CONCATENATE                             iv_msgv1
                                            iv_msgv2
                                            iv_msgv3
                                            iv_msgv4
                                       INTO lv_text
                               SEPARATED BY space.

  ELSE.

    IF       ( iv_msgid                  EQ 'ZFI01' ).

      MESSAGE  ID iv_msgid             TYPE iv_msgty
           NUMBER 0                    INTO lv_text
             WITH iv_msgv1                  iv_msgv2
                  iv_msgv3                  iv_msgv4.

    ELSE.

      MESSAGE  ID iv_msgid             TYPE iv_msgty
           NUMBER lv_msgno             INTO lv_text
             WITH iv_msgv1                  iv_msgv2
                  iv_msgv3                  iv_msgv4.

    ENDIF.

  ENDIF.

  CLEAR                                     ls_errs_proc.
  MOVE     iv_nbr_doc                    TO ls_errs_proc-nbr_doc.
  MOVE     iv_rc                         TO ls_errs_proc-rc.
  MOVE     iv_msgid                      TO ls_errs_proc-msgid.
  MOVE     iv_msgty                      TO ls_errs_proc-msgty.
  MOVE     lv_msgno                      TO ls_errs_proc-msgno.
  MOVE     lv_text                       TO ls_errs_proc-text.
  MOVE     iv_msgv1                      TO ls_errs_proc-msgv1.
  MOVE     iv_msgv2                      TO ls_errs_proc-msgv2.
  MOVE     iv_msgv3                      TO ls_errs_proc-msgv3.
  MOVE     iv_msgv4                      TO ls_errs_proc-msgv4.
  APPEND   ls_errs_proc                  TO gt_errs_proc.

ENDFORM.                    " f_error_in_process
*eject
*&---------------------------------------------------------------------*
*&      Form  f_error_in_data
*&---------------------------------------------------------------------*
*       Append an error to the data error table
*----------------------------------------------------------------------*
FORM f_error_in_data
  USING    iv_nbr_doc                  TYPE numc5
           iv_rc                       TYPE numc5
           iv_msgid                    TYPE symsgid
           iv_msgty                    TYPE symsgty
           iv_msgno                    TYPE any
           iv_msgv1                    TYPE any
           iv_msgv2                    TYPE any
           iv_msgv3                    TYPE any
           iv_msgv4                    TYPE any.

  DATA:    lv_type_fld                 TYPE char1,
           lv_msgno_c                  TYPE char3,
           lv_msgno                    TYPE symsgno,
           lv_text                     TYPE text240.

  DATA:    ls_errs_data                TYPE ty_wa_errs.

  IF     ( iv_msgty                      CA 'AaEe' ).
    ADD    1                             TO gv_count_err_data.
    CLEAR                                   gv_flag_err_data.
    MOVE   gc_x                          TO gv_flag_err_data.
    CLEAR                                   gv_flag_err_mstr.
    MOVE   gc_x                          TO gv_flag_err_mstr.
  ENDIF.

  CLEAR    lv_type_fld.
  CLEAR    lv_msgno_c.
  CLEAR    lv_msgno.
  CLEAR    lv_text.

  DESCRIBE FIELD iv_msgno              TYPE lv_type_fld.

  IF           ( lv_type_fld             CS gc_c ).

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = iv_msgno
      IMPORTING
        output = lv_msgno_c.

    IF         ( lv_msgno_c              CO '0123456789' ).
      MOVE       lv_msgno_c              TO lv_msgno.
    ENDIF.
  ELSEIF       ( lv_type_fld             CA 'IN' ).
    MOVE         iv_msgno                TO lv_msgno.
  ENDIF.

*eject
  IF   ( iv_msgid IS INITIAL ).

    CONCATENATE                             iv_msgv1
                                            iv_msgv2
                                            iv_msgv3
                                            iv_msgv4
                                       INTO lv_text
                               SEPARATED BY space.

  ELSE.

    IF       ( iv_msgid                  EQ 'ZFI01' ).

      MESSAGE  ID iv_msgid             TYPE iv_msgty
           NUMBER 0                    INTO lv_text
             WITH iv_msgv1                  iv_msgv2
                  iv_msgv3                  iv_msgv4.

    ELSE.

      MESSAGE  ID iv_msgid             TYPE iv_msgty
           NUMBER lv_msgno             INTO lv_text
             WITH iv_msgv1                  iv_msgv2
                  iv_msgv3                  iv_msgv4.

    ENDIF.

  ENDIF.

  CLEAR                                     ls_errs_data.
  MOVE     iv_nbr_doc                    TO ls_errs_data-nbr_doc.
  MOVE     iv_rc                         TO ls_errs_data-rc.
  MOVE     iv_msgid                      TO ls_errs_data-msgid.
  MOVE     iv_msgty                      TO ls_errs_data-msgty.
  MOVE     lv_msgno                      TO ls_errs_data-msgno.
  MOVE     lv_text                       TO ls_errs_data-text.
  MOVE     iv_msgv1                      TO ls_errs_data-msgv1.
  MOVE     iv_msgv2                      TO ls_errs_data-msgv2.
  MOVE     iv_msgv3                      TO ls_errs_data-msgv3.
  MOVE     iv_msgv4                      TO ls_errs_data-msgv4.
  APPEND   ls_errs_data                  TO gt_errs_data.

ENDFORM.                    " f_error_in_data
*eject
*&---------------------------------------------------------------------*
*&      Form  f_report_results
*&---------------------------------------------------------------------*
*       Report the results
*----------------------------------------------------------------------*
FORM f_report_results
  TABLES   it_data_file                TYPE ty_it_data_file
           it_data_mstr                TYPE ty_it_pa0105_ext.

  DATA:    ls_data_mstr                TYPE ty_wa_pa0105_ext,
           ls_errs_proc                TYPE ty_wa_errs,
           ls_errs_data                TYPE ty_wa_errs.

  DATA:    lv_report_mode              TYPE char4,
           lv_count                    TYPE numc5,
           lv_count_errs_180           TYPE numc5,
           lv_count_errs_185           TYPE numc5,
           lv_count_errs_190           TYPE numc5,
           lv_count_recs_posted        TYPE numc5.

  CLEAR    gv_number_report.

  CLEAR    lv_report_mode.
  CLEAR    lv_count.

* Write the control totals section
  NEW-PAGE.
  WRITE: /001 sy-uline(52),
          054 text-r11,
          069 sy-uline(52).

* Write the report mode and record counts
  CLEAR                                     lv_report_mode.
  IF       ( cb_test IS INITIAL ).
    MOVE     text-rmp                    TO lv_report_mode.
  ELSE.
    MOVE     text-rmt                    TO lv_report_mode.
  ENDIF.

  lv_count = gv_count_err_proc +
             gv_count_err_data.

  SKIP    1.
  WRITE: /001 text-r21,
          020 lv_report_mode,
          080 text-r22,
          096 gv_count_err_proc,
         /083 text-r23,
          096 gv_count_err_data,
         /082 text-r24,
          096 lv_count.

*eject
* Write the input filename and input record count
  SKIP    1.
  WRITE: /001 text-r31,
          013 gv_filename.

  DESCRIBE TABLE it_data_file         LINES lv_count.

  SKIP    1.
  WRITE: /001 text-r32,
          016 lv_count.

* Write the errors section
  NEW-PAGE.
  WRITE: /001 sy-uline(56),
          058 text-r41,
          065 sy-uline(56).

  IF     ( gv_flag_err_proc              IS NOT INITIAL ).

    SKIP  1.
    WRITE: /001 text-r42.
    SKIP  1.

    CLEAR                                   ls_errs_proc.
    LOOP AT  gt_errs_proc              INTO ls_errs_proc.
      WRITE: /001 text-r51,             005 ls_errs_proc-rc,
              013 text-r52,             021 ls_errs_proc-msgid,
              042 text-r53,             052 ls_errs_proc-msgty,
              056 text-r54,             064 ls_errs_proc-msgno.
      WRITE: /001 ls_errs_proc-text(120).
      CLEAR  ls_errs_proc.
    ENDLOOP.

  ENDIF.

  IF     ( gv_flag_err_data              IS NOT INITIAL ).

    CLEAR    lv_count_errs_180.
    CLEAR    lv_count_errs_185.
    CLEAR    lv_count_errs_190.

    CLEAR                                   ls_errs_data.
    LOOP AT  gt_errs_data              INTO ls_errs_data.
      CASE     ls_errs_data-msgno.
        WHEN     180.
          ADD      1                     TO lv_count_errs_180.
        WHEN     185.
          ADD      1                     TO lv_count_errs_185.
        WHEN     190.
          ADD      1                     TO lv_count_errs_190.
        WHEN     OTHERS.
      ENDCASE.
      CLEAR  ls_errs_data.
    ENDLOOP.

*eject
    SKIP  1.
    WRITE: /001 text-r45,
            083 text-r25,
            096 lv_count_errs_180.
    SKIP  1.
    gv_number_report = 180.

    CLEAR                                   ls_errs_data.
    LOOP AT  gt_errs_data              INTO ls_errs_data
                                      WHERE msgno = 180.
      WRITE: /001 text-r51,             005 ls_errs_data-rc,
              013 text-r52,             021 ls_errs_data-msgid,
              042 text-r53,             052 ls_errs_data-msgty,
              056 text-r54,             064 ls_errs_data-msgno.
      WRITE: /001 ls_errs_data-text(120).
      CLEAR  ls_errs_data.
    ENDLOOP.

    SKIP  1.
    WRITE: /001 text-r46,
            083 text-r25,
            096 lv_count_errs_185.
    SKIP  1.
    gv_number_report = 185.

    CLEAR                                   ls_errs_data.
    LOOP AT  gt_errs_data              INTO ls_errs_data
                                      WHERE msgno = 185.
      WRITE: /001 text-r51,             005 ls_errs_data-rc,
              013 text-r52,             021 ls_errs_data-msgid,
              042 text-r53,             052 ls_errs_data-msgty,
              056 text-r54,             064 ls_errs_data-msgno.
      WRITE: /001 ls_errs_data-text(120).
      CLEAR  ls_errs_data.
    ENDLOOP.

    SKIP  1.
    WRITE: /001 text-r47,
            083 text-r25,
            096 lv_count_errs_190.
    SKIP  1.
    gv_number_report = 190.

    CLEAR                                   ls_errs_data.
    LOOP AT  gt_errs_data              INTO ls_errs_data
                                      WHERE nbr_doc = 190.
      WRITE: /001 text-r51,             005 ls_errs_data-rc,
              013 text-r52,             021 ls_errs_data-msgid,
              042 text-r53,             052 ls_errs_data-msgty,
              056 text-r54,             064 ls_errs_data-msgno.
      WRITE: /001 ls_errs_data-text(120).
      CLEAR  ls_errs_data.
    ENDLOOP.

  ENDIF.

*eject
* Write the updates section
  CLEAR    gv_number_report.

  NEW-PAGE.
  WRITE: /001 sy-uline(55),
          057 text-r61,
          065 sy-uline(56).

  CLEAR    lv_count_recs_posted.

  CLEAR                                     ls_data_mstr.
  LOOP AT  it_data_mstr                INTO ls_data_mstr
                                      WHERE flag_posted EQ gc_x.
    ADD      1                           TO lv_count_recs_posted.
    CLEAR    ls_data_mstr.
  ENDLOOP.

  SKIP  1.
  WRITE: /001 text-r65,
          082 text-r66,
          096 lv_count_recs_posted.
  SKIP  1.
  WRITE: /001 text-r71,
          013 text-r72,
          021 text-r73,
          035 text-r74,
          049 text-r75,
          056 text-r76,
          070 text-r77,
          086 text-r78.
  SKIP  1.
  gv_number_report = 200.

  CLEAR                                     ls_data_mstr.
  LOOP AT  it_data_mstr                INTO ls_data_mstr
                                      WHERE flag_posted EQ abap_true.

    WRITE: /001 ls_data_mstr-pernr,
            013 ls_data_mstr-subty,
            021 ls_data_mstr-endda,
            035 ls_data_mstr-begda,
            049 ls_data_mstr-seqnr,
            056 ls_data_mstr-aedtm,
            070 ls_data_mstr-uname,
            086 ls_data_mstr-usrid(30).

    CLEAR    ls_data_mstr.
  ENDLOOP.

  CLEAR    gv_number_report.

  SKIP  2.
  WRITE: /001 sy-uline(52),
          054 text-r81,
          068 sy-uline(53).

ENDFORM.                    " f_report_results
*eject
*&---------------------------------------------------------------------*
*&      Form  f_print_report_header
*&---------------------------------------------------------------------*
*       Print the top of page report header
*----------------------------------------------------------------------*
FORM f_print_report_header.

  CLEAR                                     gv_sysid.
  MOVE     sy-sysid                      TO gv_sysid.
  MOVE     sy-mandt                      TO gv_sysid+4(3).
  CLEAR                                     gv_uname.
  MOVE     sy-uname                      TO gv_uname.
  CLEAR                                     gv_pagno.
  MOVE     sy-pagno                      TO gv_pagno.
  CLEAR                                     gv_cprog.
  MOVE     sy-cprog                      TO gv_cprog.
  CLEAR                                     gv_datum.
  MOVE     sy-datum                      TO gv_datum.
  CLEAR                                     gv_uzeit.
  MOVE     sy-uzeit                      TO gv_uzeit.

  WRITE: /002 text-h11,
          013 gv_sysid,
          051 text-h12,
          062 gv_uname,
          107 text-h13,
          116 gv_pagno.

  WRITE: /001 text-h14,
          013 gv_cprog,
          052 text-h15,
          062 gv_datum,
          103 text-h16,
          113 gv_uzeit.

  WRITE: /001 sy-uline(120).

  SKIP       2.

*eject
  CASE     gv_number_report.
    WHEN     180.
      WRITE: /001 sy-uline(56),
              058 text-r41,
              065 sy-uline(56).
      SKIP     1.
      WRITE: /001 text-r45.
      SKIP     1.
    WHEN     185.
      WRITE: /001 sy-uline(56),
              058 text-r41,
              065 sy-uline(56).
      SKIP     1.
      WRITE: /001 text-r46.
      SKIP     1.
    WHEN     190.
      WRITE: /001 sy-uline(56),
              058 text-r41,
              065 sy-uline(56).
      SKIP     1.
      WRITE: /001 text-r47.
      SKIP     1.
    WHEN     200.
      WRITE: /001 sy-uline(55),
              057 text-r61,
              065 sy-uline(56).
      SKIP     1.
      WRITE: /001 text-r65.
      SKIP     1.
      WRITE: /001 text-r71,
              013 text-r72,
              021 text-r73,
              035 text-r74,
              049 text-r75,
              056 text-r76,
              070 text-r77,
              086 text-r78.
      SKIP  1.
    WHEN     OTHERS.
  ENDCASE.

ENDFORM.                    " f_print_report_header
