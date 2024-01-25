*&---------------------------------------------------------------------*
*& Report ZAADT_TC_DN_VKJ
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zaadt_tc_dn_vkj.
TABLES:e071.
*TYPES : BEGIN OF zfields,
*    lwa_TRANSACTION type tstc-tcode,
*    lwa_program type TRDIR-NAME,
*    lwa_DYNPRO TYPE D020S-DNUM,
*    lwa_LANGUAGE TYPE SY-LANGU,
*    lwa_WITH_DOCU TYPE RGLIF-WITH_DOCU,
*    lwa_DOCUTYPE TYPE RGLIF-DOCUTYPE,
*    lwa_DEVELOPMENT_CLASS TYPE RGLIF-DEVCLASS,
*    lwa_TRANSPORT_NUMBER TYPE RGLIF-TRKORR,
*    lwa_TRANSACTION_TYPE TYPE RGLIF-DOCUTYPE,
*    lwa_SHORTTEXT TYPE TSTCT-TTEXT,
*    lwa_CALLED_TRANSACTION TYPE TSTC-TCODE,
*    lwa_CALLED_TRANSACTION_SKIP TYPE CHAR01,
*    lwa_VARIANT TYPE TCVARIANT,
*    lwa_CL_INDEPENDEND TYPE CHAR01,
*    lwa_EASY_WEB_TRANSACTION TYPE S_EWT,
*    lwa_PROFESSIONEL_USER_TRANSACTIO TYPE S_PROF,
*    lwa_HTML_ENABLED TYPE S_WEBGUI,
*    lwa_JAVA_ENABLED TYPE S_PLATIN,
*    lwa_WINGUI_ENABLED TYPE S_WIN32,
*    lwa_SERVICEFILE TYPE IACSERVIC_,
*    lwa_GENFLAG TYPE TADIR-GENFLAG,
*    lwa_SUPPRESS_CORR_INSERT TYPE CHAR1,
*    END OF zfields .

CONSTANTS:c_sslash TYPE char1 VALUE '\',
          c_dslash TYPE char2 VALUE '\\'.
DATA : lwa_transaction             TYPE tstc-tcode,
       lwa_program                 TYPE trdir-name,
       lwa_dynpro                  TYPE d020s-dnum,
       lwa_language                TYPE sy-langu,
       lwa_with_docu               TYPE rglif-with_docu,
       lwa_docutype                TYPE rglif-docutype,
       lwa_development_class       TYPE rglif-devclass,
       lwa_transport_number        TYPE rglif-trkorr,
       lwa_transaction_type        TYPE rglif-docutype,
       lwa_shorttext               TYPE tstct-ttext,
       lwa_called_transaction      TYPE tstc-tcode,
       lwa_called_transaction_skip TYPE char01,
       lwa_variant                 TYPE tcvariant,
       lwa_cl_independend          TYPE char01,
       lwa_easy_web_transaction    TYPE s_ewt,
       lwa_professionel            TYPE s_prof,
       lwa_html_enabled            TYPE s_webgui,
       lwa_java_enabled            TYPE s_platin,
       lwa_wingui_enabled          TYPE s_win32,
       lwa_servicefile             TYPE iacservic_,
       lwa_genflag                 TYPE tadir-genflag,
       lwa_suppress_corr_insert    TYPE char1,
       lv_tran_name                TYPE ddobjname,
       lit_custom_dep              TYPE zaadt_custom_obj_dep_tt.

DATA : lit_seltexts TYPE TABLE OF rsseltexts,
       lwa_seltexts TYPE rsseltexts.

DATA : lit_data   TYPE zaadt_format_tt,
       lit_data_f TYPE zaadt_format_tt.



SELECTION-SCREEN: BEGIN OF BLOCK blk1 WITH FRAME TITLE aaa.
SELECT-OPTIONS:s_tran FOR e071-obj_name NO INTERVALS OBLIGATORY.
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
  aaa = 'Please enter the Transactions to be downloaded'.
  bbb = 'File download properties'.
  CLEAR lwa_seltexts.
  lwa_seltexts-name = 'S_TRAN'.
  lwa_seltexts-kind = 'S'.
  lwa_seltexts-text = 'T-code'.
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
  DATA : lo_tran TYPE REF TO zaadt_tc_opr.
* WAIT UP TO 120 SECONDS.
  LOOP AT s_tran.
    MOVE s_tran-low TO lv_tran_name.
    CREATE OBJECT lo_tran EXPORTING i_tran_name = lv_tran_name.
    CALL METHOD lo_tran->get_data
      IMPORTING
        e_transaction                  = lwa_transaction
        e_program                      = lwa_program
        e_dynpro                       = lwa_dynpro
        e_language                     = lwa_language
        e_with_docu                    = lwa_with_docu
        e_docutype                     = lwa_docutype
        e_development_class            = lwa_development_class
        e_transport_number             = lwa_transport_number
        e_transaction_type             = lwa_transaction_type
        e_shorttext                    = lwa_shorttext
        e_called_transaction           = lwa_called_transaction
        e_called_transaction_skip      = lwa_called_transaction_skip
        e_variant                      = lwa_variant
        e_cl_independend               = lwa_cl_independend
        e_easy_web_transaction         = lwa_easy_web_transaction
        e_professionel_user_transactio = lwa_professionel
        e_html_enabled                 = lwa_html_enabled
        e_java_enabled                 = lwa_java_enabled
        e_wingui_enabled               = lwa_wingui_enabled
        e_servicefile                  = lwa_servicefile
        e_genflag                      = lwa_genflag
        e_suppress_corr_insert         = lwa_suppress_corr_insert
        e_custom_dep                   = lit_custom_dep
      EXCEPTIONS
        e_existence_check_error        = 1
        e_tran_data_get_error          = 2
        e_tran_not_active              = 3
        OTHERS                         = 4.

    IF sy-subrc = 0.

      DATA : lo_data TYPE REF TO zaadt_data_tc_opr.
      CREATE OBJECT lo_data EXPORTING i_obj_type = zaadt_data_tc_opr=>tran. "'TRAN'
      CALL METHOD lo_data->combine_data
        EXPORTING
          i_transaction                  = lwa_transaction
          i_program                      = lwa_program
          i_dynpro                       = lwa_dynpro
          i_language                     = sy-langu
          i_with_docu                    = ' '
          i_docutype                     = 'T'
          i_development_class            = '$TMP'
          i_transport_number             = lwa_transport_number
          i_transaction_type             = 'R'
          i_shorttext                    = lwa_shorttext
          i_called_transaction           = lwa_called_transaction
          i_called_transaction_skip      = lwa_called_transaction_skip
          i_variant                      = lwa_variant
          i_cl_independend               = lwa_cl_independend
          i_easy_web_transaction         = lwa_easy_web_transaction
          i_professionel_user_transactio = lwa_professionel
          i_html_enabled                 = lwa_html_enabled
          i_java_enabled                 = lwa_java_enabled
          i_wingui_enabled               = lwa_wingui_enabled
          i_servicefile                  = lwa_servicefile
          i_genflag                      = space
          i_suppress_corr_insert         = space
        RECEIVING
          r_data                         = lit_data
        EXCEPTIONS
          e_obj_typ_not_supp             = 1
          e_tran_defn_miss               = 2
          OTHERS                         = 3.




      IF sy-subrc = 0.
        APPEND LINES OF lit_data TO lit_data_f.
        REFRESH : lit_data.
        CLEAR : lwa_transaction,
                lwa_program,
                lwa_dynpro,
                lwa_language,
                lwa_with_docu,
                lwa_docutype,
                lwa_development_class,
                lwa_transport_number,
                lwa_transaction_type,
                lwa_shorttext,
                lwa_called_transaction,
                lwa_called_transaction_skip,
                lwa_variant,
                lwa_cl_independend,
                lwa_easy_web_transaction,
                lwa_professionel,
                lwa_html_enabled,
                lwa_java_enabled,
                lwa_wingui_enabled,
                lwa_servicefile,
                lwa_genflag,
                lwa_suppress_corr_insert.
        FREE lo_data.
        FREE lo_tran.
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
