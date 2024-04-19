*&---------------------------------------------------------------------*
*&  Include           ZHTVE001_MAINT_IT0105_CC_TOP
*&---------------------------------------------------------------------*

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZHTVE001_MAINT_IT0105_CC                          *
*  Include:          ZHTVE001_MAINT_IT0105_CC_TOP                      *
*  Author:           John Hartung                                      *
*  Date:             January 11, 2016                                  *
*  Application Area: HR TV (Travel & Expense)                          *
*                                                                      *
*  Description:      Travel & Expense InfoType 0105 SubType 0011       *
*                    Credit Card Maintenance                           *
*                                                                      *
*                    TOP Include - Data Declarations                   *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 01/11/16 JRHARTUNG D30K926519 - Tkt ACR-120 Initial program          *
*----------------------------------------------------------------------*
************************************************************************

*eject
TABLES: pa0105.                                  "HR Master Rec: IT 0105

************************************************************************
*                          Custom Data Types                           *
************************************************************************
TYPES: BEGIN OF ty_wa_vrm_value,                 "VRM Values           "
        key(40)          TYPE c,                 "VRM Key              "
        text(80)         TYPE c,                 "VRM Value            "
       END   OF ty_wa_vrm_value.

TYPES:  ty_it_vrm_values TYPE STANDARD TABLE OF ty_wa_vrm_value.

TYPES: BEGIN OF ty_wa_data_file,                 "Data Table           "
        col01            TYPE STRING,
        col02            TYPE STRING,
        col03            TYPE STRING,
        col04            TYPE STRING,
        col05            TYPE STRING,
        col06            TYPE STRING,
        col07            TYPE STRING,
       END   OF ty_wa_data_file.

TYPES:  ty_it_data_file  TYPE STANDARD TABLE OF ty_wa_data_file.

TYPES: BEGIN OF ty_wa_pa0105_ext.                "HR Master Rec: IT 0105
INCLUDE     STRUCTURE pa0105.
TYPES:  flag_posted      TYPE flag.
TYPES: END   OF ty_wa_pa0105_ext.

TYPES:  ty_it_pa0105_ext TYPE STANDARD TABLE OF ty_wa_pa0105_ext.

TYPES: BEGIN OF ty_wa_errs,                      "Errors               "
        nbr_doc          TYPE numc5,             "Doc Sequence Number  "
        rc               TYPE numc5,             "Return Code          "
        msgid            TYPE symsgid,           "Message Type         "
        msgty            TYPE symsgty,           "Message ID           "
        msgno            TYPE symsgno,           "Message Number       "
        text             TYPE text240,           "Text                 "
        msgv1            TYPE symsgv,            "Message Parameter 1  "
        msgv2            TYPE symsgv,            "Message Parameter 2  "
        msgv3            TYPE symsgv,            "Message Parameter 3  "
        msgv4            TYPE symsgv,            "Message Parameter 4  "
       END   OF ty_wa_errs.

TYPES:  ty_it_errs       TYPE STANDARD TABLE OF ty_wa_errs.

*eject
************************************************************************
*                              Constants                               *
************************************************************************
CONSTANTS:
        gc_a             TYPE char1              "A / Abnormal Terminatn
                         VALUE 'A',
        gc_c             TYPE char1              "C / Character        "
                         VALUE 'C',
        gc_e             TYPE char1              "E / Error            "
                         VALUE 'E',
        gc_x             TYPE char1              "X / Yes / True       "
                         VALUE 'X',
        gc_modif_id_dsp  TYPE char3              "ModifID-Display Only "
                         VALUE 'DSP',
        gc_infty_cc      TYPE infty              "InfoType-Credit Card "
                         VALUE '0105',
        gc_subty_cc      TYPE subty              "SubType-Credit Card  "
                         VALUE '0011',
        gc_flddlm        TYPE vrm_id             "Field Delim Scrn Name"
                         VALUE 'P_FLDDLM',
        gc_fdl_comma     TYPE char12             "Field Delim Lit-Comma"
                         VALUE 'COMMA',
        gc_fd_comma      TYPE char1              "Field Delimiter-Comma"
                         VALUE ',',
        gc_fdl_excel     TYPE char12             "File Delim Lit-Excel "
                         VALUE 'EXCEL',
        gc_fd_excel      TYPE char1              "File Delimiter-Excel "
                         VALUE 'X',
        gc_fdl_tab       TYPE char12             "Field Delim Lit-Tab  "
                         VALUE 'TAB',
        gc_fd_tab        TYPE char1              "Field Delimiter-Tab  "
                         VALUE cl_abap_char_utilities=>horizontal_tab,
        gc_fdl_vbar      TYPE char12             "Field Delim Lit-V.Bar"
                         VALUE 'VERTICAL BAR',
        gc_fd_vbar       TYPE char1              "Field Delimiter-V.Bar"
                         VALUE '|',
        gc_period        TYPE char1              "Period               "
                         VALUE '.',
        gc_periods       TYPE char50 VALUE       "Periods              "
                  '..................................................'.

*eject
************************************************************************
*                              Variables                               *
************************************************************************
DATA:   gv_filename      TYPE localfile,         "Filename             "
        gv_fd_lit        TYPE char12,            "Field Delimiter Literl
        gv_fd_val        TYPE char1,             "Field Delimiter Value"
        gv_number_report TYPE i.                 "Report Number        "

DATA:   gv_count_err_data                        "Count-Errors-Data    "
                         TYPE numc5,
        gv_count_err_proc                        "Count-Errors-Process "
                         TYPE numc5,
        gv_flag_err_data TYPE flag,              "Flag-Errors-Data     "
        gv_flag_err_proc TYPE flag,              "Flag-Errors-Process  "
        gv_flag_err_mstr TYPE flag.              "Flag-Errors-Master   "

DATA:   gv_sysid         TYPE sysysid,           "Name Of The SAP Systm"
        gv_uname         TYPE syuname,           "User Name            "
        gv_pagno         TYPE sypagno,           "Current List Page    "
        gv_cprog         TYPE sycprog,           "Calling Program      "
        gv_datum         TYPE sydatum,           "Current Date of Applc"
        gv_uzeit         TYPE syuzeit.           "Current Time of Applc"

************************************************************************
*                           Internal Tables                            *
************************************************************************
DATA:   gt_flddlm        TYPE ty_it_vrm_values.  "Delimeter Values     "

DATA:   gt_errs_data     TYPE ty_it_errs.        "Errors-Data          "

DATA:   gt_errs_proc     TYPE ty_it_errs.        "Errors-Process       "

*eject
************************************************************************
*                           Selection Screen                           *
************************************************************************
* Select Options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb1 WITH FRAME TITLE text-sb1.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_infty  TYPE infty            "InfoType             "
                           OBLIGATORY
                           MODIF ID DSP
                           DEFAULT gc_infty_cc.
SELECTION-SCREEN: SKIP 1.
SELECT-OPTIONS    s_pernr  FOR  pa0105-pernr.    "Personnel Number     "
SELECT-OPTIONS    s_subty  FOR  pa0105-subty     "SubType              "
                           OBLIGATORY
                           MODIF ID DSP.
SELECT-OPTIONS    s_objps  FOR  pa0105-objps     "Object Identification"
                           NO-DISPLAY.
SELECT-OPTIONS    s_sprps  FOR  pa0105-sprps     "Lock Indicator       "
                           NO-DISPLAY.
SELECT-OPTIONS    s_endda  FOR  pa0105-endda.    "End Date             "
SELECT-OPTIONS    s_begda  FOR  pa0105-begda.    "Start Date           "
SELECT-OPTIONS    s_seqnr  FOR  pa0105-seqnr.    "Sequence Number      "
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_efcda  TYPE datum            "Effective As Of Date "
                           DEFAULT sy-datum.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK ssb1.

* Run Options
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: BEGIN OF BLOCK ssb2 WITH FRAME TITLE text-sb2.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       p_endd_p TYPE endda            "End Date-prime       "
                           DEFAULT '99991231'.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       cb_test  AS CHECKBOX           "Test Flag            "
                           DEFAULT gc_x.
SELECTION-SCREEN: SKIP 1.
PARAMETERS:       rb_appl  RADIOBUTTON GROUP rbg1  "Application Server "
                           DEFAULT 'X'
                           USER-COMMAND cmd.
PARAMETERS:       rb_pres  RADIOBUTTON GROUP rbg1. "Presentation Server"
PARAMETERS:       p_fname  TYPE localfile.       "Filename-Input       "
PARAMETERS:       p_flddlm TYPE zparamkey        "Field Delimiter      "
                           AS LISTBOX VISIBLE LENGTH 15
                           DEFAULT gc_fdl_comma.
SELECTION-SCREEN: SKIP 1.
SELECTION-SCREEN: END   OF BLOCK ssb2.
