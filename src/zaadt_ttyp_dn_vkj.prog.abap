*&---------------------------------------------------------------------*
*& Report ZAADT_TTYP_DN_VKJ
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaadt_ttyp_dn_vkj.
TABLES:e071.
DATA:lwa_ttyp_defn      TYPE dd40v,
     lwa_ttyp_key       TYPE /sapcnd/t_dd42v,
     lwa_ttyp_structure TYPE dd43v_tab,
     lv_ttyp_name       TYPE ddobjname,
     lit_custom_dep     TYPE zaadt_custom_obj_dep_tt.

CONSTANTS:c_sslash TYPE char1 VALUE '\',
          c_dslash TYPE char2 VALUE '\\'.

DATA:lit_seltexts TYPE TABLE OF rsseltexts,
     lwa_seltexts TYPE rsseltexts.

DATA:lit_data   TYPE zaadt_format_tt,
     lit_data_f TYPE zaadt_format_tt.


SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE aaa.
SELECT-OPTIONS:s_ttype FOR e071-obj_name NO INTERVALS OBLIGATORY.
SELECTION-SCREEN: END   OF BLOCK blk1.

SELECTION-SCREEN: BEGIN OF BLOCK blk2 WITH FRAME TITLE bbb.
PARAMETERS:r_xl   RADIOBUTTON GROUP rg1,
*           r_xml  RADIOBUTTON GROUP rg1 DISPLAY,
           r_text RADIOBUTTON GROUP rg1.
PARAMETERS:f_path TYPE string OBLIGATORY,
           f_name TYPE string VISIBLE LENGTH 30 OBLIGATORY.
SELECTION-SCREEN: END OF BLOCK blk2.

INITIALIZATION.
  REFRESH:lit_data.
  aaa = 'Please enter the table type to be downloaded'.
  bbb = 'File download properties'.
  CLEAR lwa_seltexts.
  lwa_seltexts-name = 'S_TTYPE'.
  lwa_seltexts-kind = 'S'.
  lwa_seltexts-text = 'Table type'.
  APPEND lwa_seltexts TO lit_seltexts.

  CLEAR lwa_seltexts.
  lwa_seltexts-name = 'F_NAME'.
  lwa_seltexts-kind = 'P'.
  lwa_seltexts-text = 'File name'.
  APPEND lwa_seltexts TO lit_seltexts.

  CLEAR lwa_seltexts.
  lwa_seltexts-name = 'F_PATH'.
  lwa_seltexts-kind = 'P'.
  lwa_seltexts-text = 'File download path'.
  APPEND lwa_seltexts TO lit_seltexts.

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

*  CLEAR lwa_seltexts.
*  lwa_seltexts-name = 'R_XML'.
*  lwa_seltexts-kind = 'P'.
*  lwa_seltexts-text = 'Xml'.
*  APPEND lwa_seltexts TO lit_seltexts.

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
  CALL METHOD cl_gui_frontend_services=>directory_browse
    EXPORTING
      window_title         = 'Please select folder to download file'
      initial_folder       = 'C:\'
    CHANGING
      selected_folder      = f_path
    EXCEPTIONS
      cntl_error           = 1
      error_no_gui         = 2
      not_supported_by_gui = 3
      OTHERS               = 4.

START-OF-SELECTION.

  LOOP AT s_ttype.
    MOVE s_ttype-low TO lv_ttyp_name.
    DATA : lo_ttyp TYPE REF TO zaadt_ttyp_opr.
    CREATE OBJECT lo_ttyp EXPORTING i_ttyp_name = lv_ttyp_name.

    CALL METHOD lo_ttyp->get_data
      IMPORTING
        e_ttyp_defn             = lwa_ttyp_defn
        e_ttyp_key              = lwa_ttyp_key
        e_ttyp_structure        = lwa_ttyp_structure
        e_custom_dep            = lit_custom_dep
      EXCEPTIONS
        e_existence_check_error = 1
        e_ttyp_data_get_error   = 2
        e_ttyp_not_active       = 3
        OTHERS                  = 4.

    IF sy-subrc = 0.

      DATA:lo_data TYPE REF TO zaadt_data_ttyp_opr.
      CREATE OBJECT lo_data EXPORTING i_obj_type = zaadt_data_ttyp_opr=>ttype. "TTYP
      CALL METHOD lo_data->combine_data
        EXPORTING
          i_ttyp_defn        = lwa_ttyp_defn
          i_ttyp_key         = lwa_ttyp_key
          i_ttyp_structure   = lwa_ttyp_structure
        RECEIVING
          r_data             = lit_data
        EXCEPTIONS
          e_obj_typ_not_supp = 1
          e_ttyp_defn_miss   = 2
          OTHERS             = 3.

      IF sy-subrc = 0.
        APPEND LINES OF lit_data TO lit_data_f.
        REFRESH:lit_data.
        CLEAR:lwa_ttyp_defn,
              lwa_ttyp_key,
              lwa_ttyp_structure.
        FREE lo_data.
        FREE lo_ttyp.
      ENDIF.
    ENDIF.
  ENDLOOP.

 CASE abap_true.
    WHEN sy-batch.
        CALL METHOD cl_salv_table=>factory
          EXPORTING
            list_display = if_salv_c_bool_sap=>false
          IMPORTING
            r_salv_table = DATA(lo_alv)
          CHANGING
            t_table      = lit_data_f.
          lo_alv->display( ).
    WHEN r_xl.
      IF lit_data_f IS NOT INITIAL.
        CONCATENATE f_name '.xls' INTO f_name.
        CONCATENATE f_path f_name INTO f_name SEPARATED BY c_sslash.
        IF f_name CS c_dslash.
          REPLACE ALL OCCURRENCES OF c_dslash IN f_name WITH c_sslash.
        ENDIF.
        CALL FUNCTION 'GUI_DOWNLOAD'
          EXPORTING
            filename                = f_name
            filetype                = 'ASC'
            write_field_separator   = cl_abap_char_utilities=>horizontal_tab
          TABLES
            data_tab                = lit_data_f
          EXCEPTIONS
            file_write_error        = 1
            no_batch                = 2
            gui_refuse_filetransfer = 3
            invalid_type            = 4
            no_authority            = 5
            unknown_error           = 6
            header_not_allowed      = 7
            separator_not_allowed   = 8
            filesize_not_allowed    = 9
            header_too_long         = 10
            dp_error_create         = 11
            dp_error_send           = 12
            dp_error_write          = 13
            unknown_dp_error        = 14
            access_denied           = 15
            dp_out_of_memory        = 16
            disk_full               = 17
            dp_timeout              = 18
            file_not_found          = 19
            dataprovider_exception  = 20
            control_flush_error     = 21
            OTHERS                  = 22.
        IF sy-subrc = 0.
*  Success message
        ENDIF.
      ENDIF.
    WHEN r_text.
      CONCATENATE f_name '.txt' INTO f_name.
      CONCATENATE f_path f_name INTO f_name SEPARATED BY c_sslash.
      IF f_name CS c_dslash.
        REPLACE ALL OCCURRENCES OF c_dslash IN f_name WITH c_sslash.
      ENDIF.
      CALL FUNCTION 'GUI_DOWNLOAD'
        EXPORTING
          filename                = f_name
          filetype                = 'ASC'
        TABLES
          data_tab                = lit_data_f
        EXCEPTIONS
          file_write_error        = 1
          no_batch                = 2
          gui_refuse_filetransfer = 3
          invalid_type            = 4
          no_authority            = 5
          unknown_error           = 6
          header_not_allowed      = 7
          separator_not_allowed   = 8
          filesize_not_allowed    = 9
          header_too_long         = 10
          dp_error_create         = 11
          dp_error_send           = 12
          dp_error_write          = 13
          unknown_dp_error        = 14
          access_denied           = 15
          dp_out_of_memory        = 16
          disk_full               = 17
          dp_timeout              = 18
          file_not_found          = 19
          dataprovider_exception  = 20
          control_flush_error     = 21
          OTHERS                  = 22.
      IF sy-subrc = 0.
*  Success message
      ENDIF.
*    WHEN r_xml.
  ENDCASE.
