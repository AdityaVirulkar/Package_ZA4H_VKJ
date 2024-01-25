*&---------------------------------------------------------------------*
*& Report ZAADT_TTYP_UP_VKJ
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZAADT_TTYP_UP_VKJ.

DATA : lwa_ttyp_defn    TYPE dd40v,
       lwa_ttyp_key     TYPE /SAPCND/T_DD42V,
       lwa_ttyp_structure TYPE DD43V_TAB,
       lv_ttyp_name TYPE ddobjname,
       lv_new_ttyp      TYPE dd40v-typename,
       lv_file        TYPE char10,
       f_path1        TYPE string.

DATA:lit_seltexts TYPE TABLE OF rsseltexts,
     lwa_seltexts TYPE rsseltexts.

*DATA:lit_data   TYPE zaadt_format_tt,
 data : lit_data_f TYPE   zaadt_format_tt,
*       lit_data_f TYPE STANDARD TABLE OF  zaadt_format_tt,
     lwa_data_f TYPE  zaadt_format.

SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE aaa.
PARAMETERS:r_xl   RADIOBUTTON GROUP rg1 DEFAULT 'X',
*           r_xml  RADIOBUTTON GROUP rg1 DISPLAY,
           r_text RADIOBUTTON GROUP rg1.
*PARAMETERS: f_path type string  OBLIGATORY.
PARAMETERS: f_path TYPE ibipparms-path  OBLIGATORY.
*           f_name TYPE string VISIBLE LENGTH 30 OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK blk1.

INITIALIZATION.
*REFRESH : lit_data.
  aaa = 'file upload properties'.

  CLEAR lwa_seltexts.
  lwa_seltexts-name = 'F_PATH'.
  lwa_seltexts-kind = 'P'.
  lwa_seltexts-text = 'File upload path'.
  APPEND lwa_seltexts TO lit_seltexts.

*    CLEAR lwa_seltexts.
*  lwa_seltexts-name = 'F_NAME'.
*  lwa_seltexts-kind = 'P'.
*  lwa_seltexts-text = 'File name'.
*  APPEND lwa_seltexts TO lit_seltexts.

  CLEAR lwa_seltexts.
  lwa_seltexts-name = 'R_TEXT'.
  lwa_seltexts-kind = 'P'.
  lwa_seltexts-text = 'Text'.
  APPEND lwa_seltexts TO lit_seltexts.

  CLEAR lwa_seltexts.
  lwa_seltexts-name = 'R_XL'.
  lwa_seltexts-kind = 'P'.
  lwa_seltexts-text = 'Excel'.
  APPEND lwa_seltexts TO lit_seltexts.

  CALL FUNCTION 'SELECTION_TEXTS_MODIFY'
    EXPORTING
      program                     = sy-cprog
    TABLES
      seltexts                    = lit_seltexts
    EXCEPTIONS
      program_not_found           = 1
      program_cannot_be_generated = 2
      OTHERS                      = 3.
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR f_path.
  CALL FUNCTION 'F4_FILENAME'
    EXPORTING
      program_name  = syst-cprog
      dynpro_number = syst-dynnr
      field_name    = 'f_path'
    IMPORTING
      file_name     = f_path.

START-OF-SELECTION.
  IF r_xl = 'X'.

    CALL FUNCTION 'TRINT_FILE_GET_EXTENSION'
      EXPORTING
        filename  = f_path
        uppercase = 'X'
      IMPORTING
        extension = lv_file.

    IF lv_file = 'XLS'.
      f_path1 = f_path.
      CALL METHOD cl_gui_frontend_services=>gui_upload
        EXPORTING
          filename                = f_path1
          filetype                = 'ASC'
          has_field_separator     = cl_abap_char_utilities=>horizontal_tab
        CHANGING
          data_tab                = lit_data_f
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
          not_supported_by_gui    = 17
          error_no_gui            = 18
          OTHERS                  = 19.

      IF sy-subrc = 0.
* Implement suitable error handling here
      ENDIF.

    ELSEIF r_text = 'X'.
      f_path1 = f_path.
      CALL FUNCTION 'GUI_UPLOAD'
        EXPORTING
          filename                = f_path1
          filetype                = 'ASC'
        TABLES
          data_tab                = lit_data_f
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

      IF sy-subrc = 0.
* Implement suitable error handling here
      ENDIF.
    ENDIF.
  ENDIF.

  IF lit_data_f IS NOT INITIAL.
DATA:lo_data TYPE REF TO zaadt_data_ttyp_opr.
CREATE OBJECT lo_data EXPORTING i_obj_type = zaadt_data_ttyp_opr=>ttype.
CALL METHOD lo_data->combine_data_up
  EXPORTING
    i_data             = lit_data_f
  IMPORTING
    e_ttyp_defn        = lwa_ttyp_defn
    e_ttyp_key         = lwa_ttyp_key
    e_ttyp_structure   = lwa_ttyp_structure
  EXCEPTIONS
    i_obj_typ_not_supp = 1
    e_ttyp_defn_miss   = 2
    others             = 3.

IF sy-subrc = 0.
* LOOP AT lit_data_f INTO lwa_data_f.
   READ TABLE lit_data_f INDEX 1 into lwa_data_f.
 MOVE lwa_data_f-obj_name TO lv_ttyp_name.
*   MOVE lwa_data_f-obj_data TO lwa_do_defn.
        lv_new_ttyp = lwa_ttyp_defn-typename.
 data : lo_ttyp TYPE REF TO zaadt_ttyp_opr.
 CREATE OBJECT lo_ttyp EXPORTING i_ttyp_name = lv_ttyp_name.
CALL METHOD lo_ttyp->set_data
  EXPORTING
    i_ttyp_defn             = lwa_ttyp_defn
    i_ttyp_key              = lwa_ttyp_key
    i_ttyp_structure        = lwa_ttyp_structure
    i_cre_alt               = ABAP_TRUE
  IMPORTING
    e_new_ttyp              = lv_new_ttyp
  EXCEPTIONS
    e_existence_check_error = 1
    e_ttyp_data_set_error   = 2
    e_ttyp_active           = 3
    e_ttyp_exists_inactive  = 4
    e_activation_error      = 5
    others                  = 6.
*     CLEAR : lwa_do_defn,
*                lit_do_values.
*        ENDLOOP.
endif.
IF sy-subrc = 0.
 REFRESH:lit_data_f.
          CLEAR:lwa_ttyp_defn,
          lwa_ttyp_key,
          lwa_ttyp_structure,
          lwa_data_f.
          FREE lo_data.
          FREE lo_ttyp.
ENDIF.

*ENDIF.
ENDIF.
