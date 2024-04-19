*&---------------------------------------------------------------------*
*& Report  ZHPAI001_UPLOAD_INFOTYP_0105
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  ZHPAI001_UPLOAD_INFOTYP_0105 NO STANDARD PAGE HEADING LINE-SIZE 255.

* Types Declarations *

TYPES : BEGIN OF t_upload,

           string TYPE string,

        END OF t_upload ,

        BEGIN OF t_input_data,

           pernr TYPE char8,
           begda TYPE char10,
           endda TYPE char10,
           usrty TYPE char4,
           usrid TYPE char30,

        END OF t_input_data.

* Constant Declarations *


* Data Declarations *
DATA: sep(1)      TYPE c,
      flag(1)     TYPE c,
      g_sessname  TYPE apq_grpn VALUE 'ZHRC0105'.


*--Internal Table Declarations *.
DATA: it_upload       TYPE STANDARD TABLE OF t_upload,
      it_input_data   TYPE STANDARD TABLE OF t_input_data,
      it_upload_bdc   TYPE STANDARD TABLE OF bdcdata.


* Work Area Declarations *
DATA: wa_upload       TYPE t_upload,
      wa_input_data   TYPE t_input_data,
      wa_upload_bdc   TYPE bdcdata.

************************************************************************
* Selection screen *
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS : p_fname TYPE rlgrap-filename OBLIGATORY,
             p_sep   TYPE char1.

SELECTION-SCREEN END OF BLOCK b1.

************************************************************************
* AT SELECTION-SCREEN processes
************************************************************************
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_fname.

*-- Form to  Select a Legacy File From a Particular Location
  PERFORM get_filename_f4 .

************************************************************************
* initialization *
************************************************************************


************************************************************************
* Start-of-selection *
************************************************************************
START-OF-SELECTION.

*** To open Session
  PERFORM open_group USING g_sessname.

*** To upload flat file data into the internal table.
  PERFORM file_upload.

  IF it_upload[] IS NOT INITIAL.

    flag = 'X'.

*** Split data from flat file into an internal table
    PERFORM split_data.

*** Upload data in BDC table
    PERFORM upload_data.

  ENDIF.

END-OF-SELECTION.

  IF flag IS NOT INITIAL.

*** Close Session group created
    PERFORM close_group.

*** Display the details after execution
    PERFORM display_details.

  ELSE.

*** To display Error Message if no data available
    MESSAGE i000(0k) WITH text-002.

  ENDIF.


************************************************************************

*&---------------------------------------------------------------------*
*&      Form  get_filename_f4
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_filename_f4 .

  CALL FUNCTION 'KD_GET_FILENAME_ON_F4'
    EXPORTING
      program_name  = syst-repid
      dynpro_number = syst-dynnr
    CHANGING
      file_name     = p_fname
    EXCEPTIONS
      mask_too_long = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " get_filename_f4

*&---------------------------------------------------------------------*
*&      Form  open_group
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM open_group  USING g_sessname TYPE apq_grpn.

  CALL FUNCTION 'BDC_OPEN_GROUP'
    EXPORTING
      client              = sy-mandt
      group               = g_sessname
      keep                = 'X'
      user                = sy-uname
    EXCEPTIONS
      client_invalid      = 1
      destination_invalid = 2
      group_invalid       = 3
      group_is_locked     = 4
      holddate_invalid    = 5
      internal_error      = 6
      queue_error         = 7
      running             = 8
      system_lock_error   = 9
      user_invalid        = 10
      OTHERS              = 11.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " open_group


*&---------------------------------------------------------------------*
*&      Form  file_upload
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM file_upload .

  DATA : l_file TYPE string.

  l_file = p_fname.

  CALL FUNCTION 'GUI_UPLOAD'
    EXPORTING
      filename                = l_file
      filetype                = 'ASC'
*     has_field_separator     = 'X'
    TABLES
      data_tab                = it_upload
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      OTHERS                  = 17.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.


ENDFORM.                    " file_upload


*&---------------------------------------------------------------------*
*&      Form  split_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM split_data .


  IF p_sep IS INITIAL.

    CLASS cl_abap_char_utilities DEFINITION LOAD.
    sep = cl_abap_char_utilities=>horizontal_tab.

  ELSE.

    sep = p_sep.

  ENDIF.

  LOOP AT it_upload INTO wa_upload.

    SPLIT wa_upload-string AT sep INTO

      wa_input_data-pernr
      wa_input_data-begda
      wa_input_data-endda
      wa_input_data-usrty
      wa_input_data-usrid.

    APPEND wa_input_data TO it_input_data.
    CLEAR wa_upload.

  ENDLOOP.

ENDFORM.                    " split_data


*&---------------------------------------------------------------------*
*&      Form  upload_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM upload_data .

  DATA: l_date   TYPE datum,
        l_date_s TYPE char10.


  LOOP AT it_input_data INTO wa_input_data.


    PERFORM bdc_dynpro      USING 'SAPMP50A' '1000'.
    PERFORM bdc_field       USING 'BDC_OKCODE'                              '/00'.
    PERFORM bdc_field       USING 'RP50G-PERNR'                              wa_input_data-pernr.
*    PERFORM bdc_field       USING 'RP50G-TIMR6'                              record-timr6_002.
    PERFORM bdc_field       USING 'BDC_CURSOR'                              'RP50G-CHOIC'.
    PERFORM bdc_field       USING 'RP50G-CHOIC'                              '105'.

    PERFORM bdc_dynpro      USING 'SAPMP50A' '1000'.
    PERFORM bdc_field       USING 'BDC_OKCODE'                              '=INS'.
    PERFORM bdc_field       USING 'RP50G-PERNR'                              wa_input_data-pernr.
*    PERFORM bdc_field       USING 'RP50G-TIMR6'                              record-timr6_005.
    PERFORM bdc_field       USING 'BDC_CURSOR'                              'RP50G-SUBTY'.
    PERFORM bdc_field       USING 'RP50G-CHOIC'                              '105'.
    PERFORM bdc_field       USING 'RP50G-SUBTY'                              wa_input_data-usrty.

    PERFORM bdc_dynpro      USING 'MP010500' '2000'.
    PERFORM bdc_field       USING 'BDC_CURSOR'                              'P0105-USRID'.
    PERFORM bdc_field       USING 'BDC_OKCODE'                              'UPD'.

    CONCATENATE wa_input_data-begda+6(4)
                wa_input_data-begda+0(2)
                wa_input_data-begda+3(2) INTO l_date.

    WRITE l_date TO l_date_s MM/DD/YYYY.

    PERFORM bdc_field       USING 'P0105-BEGDA'                              l_date_s.

    CLEAR: l_date, l_date_s.

    CONCATENATE wa_input_data-endda+6(4)
                wa_input_data-endda+0(2)
                wa_input_data-endda+3(2) INTO l_date.

    WRITE l_date TO l_date_s MM/DD/YYYY.

    PERFORM bdc_field       USING 'P0105-ENDDA'                              l_date_s.

    IF wa_input_data-usrid IS NOT INITIAL.
      PERFORM bdc_field       USING 'P0105-USRID'                              wa_input_data-usrid.
    ENDIF.

    PERFORM insert_session USING 'PA30'.

    CLEAR wa_input_data.

  ENDLOOP.

ENDFORM.                    " upload_data


*&---------------------------------------------------------------------*
*&      Form  close_group
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM close_group .

  CALL FUNCTION 'BDC_CLOSE_GROUP'
    EXCEPTIONS
      not_open    = 1
      queue_error = 2
      OTHERS      = 3.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " close_group


*&---------------------------------------------------------------------*
*&      Form  display_details
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_details .

  DATA : l_cnt TYPE i.

  DESCRIBE TABLE it_upload LINES l_cnt.

  WRITE : / 'Number of Records in Input File : ', l_cnt.

  WRITE : / 'Number of transactions created in the BDC session : ', l_cnt.

  WRITE : / 'Session Name to be executed in ''SM35'' : ', g_sessname .


ENDFORM.                    " display_details

*&---------------------------------------------------------------------*
*&      Form  bdc_dynpro
*&---------------------------------------------------------------------*
FORM bdc_dynpro  USING    name
                          value.

  CLEAR wa_upload_bdc.

  MOVE:  name  TO wa_upload_bdc-program,
         value TO wa_upload_bdc-dynpro,
         'X'   TO wa_upload_bdc-dynbegin.

  APPEND wa_upload_bdc TO it_upload_bdc.
  CLEAR  wa_upload_bdc.

ENDFORM.                    " bdc_dynpro

*&---------------------------------------------------------------------*
*&      Form  bdc_field
*&---------------------------------------------------------------------*
FORM bdc_field  USING    fnam
                         fval.

  CLEAR wa_upload_bdc.

  MOVE:  fnam TO wa_upload_bdc-fnam,
         fval TO wa_upload_bdc-fval.

  APPEND wa_upload_bdc TO it_upload_bdc.
  CLEAR wa_upload_bdc.

ENDFORM.                    " bdc_field

*&---------------------------------------------------------------------*
*&      Form  insert_session
*&---------------------------------------------------------------------*
FORM insert_session  USING    g_tcode TYPE tcode.

  CALL FUNCTION 'BDC_INSERT'
    EXPORTING
      tcode            = g_tcode
    TABLES
      dynprotab        = it_upload_bdc
    EXCEPTIONS
      internal_error   = 1
      not_open         = 2
      queue_error      = 3
      tcode_invalid    = 4
      printing_invalid = 5
      posting_invalid  = 6
      OTHERS           = 7.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

  REFRESH it_upload_bdc[].

ENDFORM.                    "insert_session
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
