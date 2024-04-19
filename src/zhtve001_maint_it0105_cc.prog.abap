*&---------------------------------------------------------------------*
*& Report  ZHTVE001_MAINT_IT0105_CC
*&---------------------------------------------------------------------*
REPORT  zhtve001_maint_it0105_cc    MESSAGE-ID zfi01
                                    LINE-COUNT 65
                                    LINE-SIZE 132
                                    NO STANDARD PAGE HEADING.

************************************************************************
*                            Spectra Energy                            *
************************************************************************
*  Program:          ZHTVE001_MAINT_IT0105_CC                          *
*  Author:           John Hartung                                      *
*  Date:             January 11, 2016                                  *
*  Application Area: HR TV (Travel & Expense)                          *
*                                                                      *
*  Description:      Travel & Expense InfoType 0105 SubType 0011       *
*                    Credit Card Maintenance                           *
*                                                                      *
************************************************************************
*----------------------- CHANGE LOG -----------------------------------*
*  Date    By        Description                                       *
* -------- --------- ------------------------------------------------- *
* 01/11/16 JRHARTUNG D30K926519 - Tkt ACR-120 Initial program          *
*----------------------------------------------------------------------*
************************************************************************

*eject
************************************************************************
*                              Top Include                             *
************************************************************************
INCLUDE zhtve001_maint_it0105_cc_top.

************************************************************************
*                            Initialization                            *
************************************************************************
INITIALIZATION.

* Initialization
  PERFORM  f_initialization.

************************************************************************
*                         At Selection-Screen                          *
************************************************************************
AT SELECTION-SCREEN OUTPUT.

* Alter the selection screen
  PERFORM  f_alter_sel_screen.

* Set the selection screen field delimiter dropdown list
  PERFORM  f_set_field_delimiter.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.

  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      field_name = 'P_FNAME'
    IMPORTING
      file_name  = p_fname.

AT SELECTION-SCREEN.

*eject
************************************************************************
*                          Start Of Selection                          *
************************************************************************
START-OF-SELECTION.

* Check the selection-screen
  PERFORM  f_validate_selection_screen.

* Initial the data elements
  PERFORM  f_initial_data_elements.

************************************************************************
*                           End Of Selection                           *
************************************************************************
END-OF-SELECTION.

* Main process
  PERFORM  f_process_main.

************************************************************************
*                             Top-Of-Page                              *
************************************************************************
TOP-OF-PAGE.

* Print the top of page report header
  PERFORM  f_print_report_header.

************************************************************************
*                             Forms Include                            *
************************************************************************
  INCLUDE zhtve001_maint_it0105_cc_f01.
