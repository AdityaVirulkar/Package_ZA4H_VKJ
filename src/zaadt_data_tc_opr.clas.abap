class ZAADT_DATA_TC_OPR definition
  public
  final
  create public .

public section.

  types:
    BEGIN OF zfields,
    TRANSACTION type tstc-tcode,
    program type TRDIR-NAME,
    DYNPRO TYPE D020S-DNUM,
    LANGUAGE TYPE SY-LANGU,
    WITH_DOCU TYPE RGLIF-WITH_DOCU,
    DOCUTYPE TYPE RGLIF-DOCUTYPE,
    DEVELOPMENT_CLASS TYPE RGLIF-DEVCLASS,
    TRANSPORT_NUMBER TYPE RGLIF-TRKORR,
    TRANSACTION_TYPE TYPE RGLIF-DOCUTYPE,
    SHORTTEXT TYPE TSTCT-TTEXT,
    CALLED_TRANSACTION TYPE TSTC-TCODE,
    CALLED_TRANSACTION_SKIP TYPE CHAR01,
    VARIANT TYPE TCVARIANT,
    CL_INDEPENDEND TYPE CHAR01,
    EASY_WEB_TRANSACTION TYPE S_EWT,
    PROFESSIONEL_USER_TRANSACTIO TYPE S_PROF,
    HTML_ENABLED TYPE S_WEBGUI,
    JAVA_ENABLED TYPE S_PLATIN,
    WINGUI_ENABLED TYPE S_WIN32,
    SERVICEFILE TYPE IACSERVIC_,
    GENFLAG TYPE TADIR-GENFLAG,
    SUPPRESS_CORR_INSERT TYPE CHAR1,
    END OF zfields .

  data OBJ_TYPE type TROBJTYPE .
  constants TRAN type E071-OBJECT value 'TRAN' ##NO_TEXT.

  methods COMBINE_DATA
    importing
      value(I_TRANSACTION) type TSTC-TCODE
      value(I_PROGRAM) type TRDIR-NAME optional
      value(I_DYNPRO) type D020S-DNUM optional
      value(I_LANGUAGE) type SY-LANGU default SY-LANGU
      value(I_WITH_DOCU) type RGLIF-WITH_DOCU default ' '
      value(I_DOCUTYPE) type RGLIF-DOCUTYPE default 'T'
      value(I_DEVELOPMENT_CLASS) type RGLIF-DEVCLASS default '$TMP'
      value(I_TRANSPORT_NUMBER) type RGLIF-TRKORR optional
      value(I_TRANSACTION_TYPE) type RGLIF-DOCUTYPE default 'R'
      value(I_SHORTTEXT) type TSTCT-TTEXT
      value(I_CALLED_TRANSACTION) type TSTC-TCODE optional
      value(I_CALLED_TRANSACTION_SKIP) type CHAR01 optional
      value(I_VARIANT) type TCVARIANT optional
      value(I_CL_INDEPENDEND) type CHAR01 optional
      value(I_EASY_WEB_TRANSACTION) type S_EWT optional
      value(I_PROFESSIONEL_USER_TRANSACTIO) type S_PROF optional
      value(I_HTML_ENABLED) type S_WEBGUI optional
      value(I_JAVA_ENABLED) type S_PLATIN optional
      value(I_WINGUI_ENABLED) type S_WIN32 optional
      value(I_SERVICEFILE) type IACSERVIC_ optional
      !I_GENFLAG type TADIR-GENFLAG default SPACE
      !I_SUPPRESS_CORR_INSERT type CHAR1 default SPACE
    returning
      value(R_DATA) type ZAADT_FORMAT_TT
    exceptions
      E_OBJ_TYP_NOT_SUPP
      E_TRAN_DEFN_MISS .
  methods COMBINE_DATA_UP
    importing
      value(I_DATA) type ZAADT_TC_FORMAT_TT
    exporting
      value(E_TRANSACTION) type TSTC-TCODE
      value(E_PROGRAM) type TRDIR-NAME
      value(E_DYNPRO) type D020S-DNUM
      value(E_LANGUAGE) type SY-LANGU
      value(E_WITH_DOCU) type RGLIF-WITH_DOCU
      value(E_DOCUTYPE) type RGLIF-DOCUTYPE
      value(E_DEVELOPMENT_CLASS) type RGLIF-DEVCLASS
      value(E_TRANSPORT_NUMBER) type RGLIF-TRKORR
      value(E_TRANSACTION_TYPE) type RGLIF-DOCUTYPE
      value(E_SHORTTEXT) type TSTCT-TTEXT
      value(E_CALLED_TRANSACTION) type TSTC-TCODE
      value(E_CALLED_TRANSACTION_SKIP) type CHAR01
      value(E_VARIANT) type TCVARIANT
      value(E_CL_INDEPENDEND) type CHAR01
      value(E_EASY_WEB_TRANSACTION) type S_EWT
      value(E_PROFESSIONEL_USER_TRANSACTIO) type S_PROF
      value(E_HTML_ENABLED) type S_WEBGUI
      value(E_JAVA_ENABLED) type S_PLATIN
      value(E_WINGUI_ENABLED) type S_WIN32
      value(E_SERVICEFILE) type IACSERVIC_
      !E_GENFLAG type TADIR-GENFLAG
      !E_SUPPRESS_CORR_INSERT type CHAR1
    exceptions
      TRANSACTION_MISS .
  methods CONSTRUCTOR
    importing
      value(I_OBJ_TYPE) type TROBJTYPE .
  methods DOWNLOAD_XL
    importing
      value(R_DATA) type DATA optional .
protected section.
private section.
ENDCLASS.



CLASS ZAADT_DATA_TC_OPR IMPLEMENTATION.


  METHOD combine_data.
    DATA : lwa_data LIKE LINE OF r_data.
*             cinfo type SYHEX01. "i_tran_tcode-cinfo
*          lwa_tran_defn LIKE LINE OF i_tran_defn,
*          lwa_tran_attrib LIKE LINE OF i_tran_attrib.
    CASE obj_type.
      WHEN tran.
        IF I_TRANSACTION IS NOT INITIAL.

          CLEAR lwa_data.
          lwa_data-obj_type = tran.
          lwa_data-obj_name = i_TRANSACTION.
          lwa_data-obj_ref = 'DEFN'.
          lwa_data-LINE_NO = sy-tabix.
*          lwa_data-tcode = I_TRANSACTION.
*          lwa_data-prog = I_PROGRAM .
*          lwa_data-dynpro = I_DYNPRO.
*          lwa_data-language = I_LANGUAGE.
*          lwa_data-with_docu = I_WITH_DOCU.
*          lwa_data-docutype = I_DOCUTYPE.
*          lwa_data-developement_class = I_DEVELOPMENT_CLASS.
*          lwa_data-transport_number = I_TRANSPORT_NUMBER.
*          lwa_data-transaction_type = I_TRANSACTION_TYPE.
*          lwa_data-shorttext = I_SHORTTEXT.
*          lwa_data-called_transaction = I_CALLED_TRANSACTION.
*          lwa_data-called_transaction_skip = I_CALLED_TRANSACTION_SKIP.
*          lwa_data-variant = I_VARIANT.
*          lwa_data-cl_independend = I_CL_INDEPENDEND.
*          lwa_data-easy_web_transaction = I_EASY_WEB_TRANSACTION.
*          lwa_data-professionel_user_transaction = I_PROFESSIONEL_USER_TRANSACTIO.
*          lwa_data-html_enabled = I_HTML_ENABLED.
*          lwa_data-java_enabled = I_JAVA_ENABLED.
*          lwa_data-wingui_enabled = I_WINGUI_ENABLED.
*          lwa_data-servicefile = I_SERVICEFILE.
*          lwa_data-genflag = I_GENFLAG.
*          lwa_data-suppress_corr_insert = I_SUPPRESS_CORR_INSERT.
          CONCATENATE I_TRANSACTION I_PROGRAM I_DYNPRO I_LANGUAGE I_WITH_DOCU I_DOCUTYPE I_DEVELOPMENT_CLASS I_TRANSPORT_NUMBER I_TRANSACTION_TYPE
         I_SHORTTEXT I_CALLED_TRANSACTION I_CALLED_TRANSACTION_SKIP I_VARIANT I_CL_INDEPENDEND I_EASY_WEB_TRANSACTION I_PROFESSIONEL_USER_TRANSACTIO
         I_HTML_ENABLED I_JAVA_ENABLED I_WINGUI_ENABLED I_SERVICEFILE I_GENFLAG I_SUPPRESS_CORR_INSERT
         into lwa_data-obj_data SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
*          move i_fields to lwa_data-obj_data.
*          CONCATENATE i_fields-i_tran_tcode i_fields-PROGRAM  i_fields-DYNPRO  i_fields-LANGUAGE i_fields-WITH_DOCU
*          i_fields-DOCUTYPE i_fields-DEVELOPMENT_CLASS i_fields-TRANSPORT_NUMBER i_fields-TRANSACTION_TYPE

*append LINES OF i_tran_tcode to lwa_data-obj_data.
*CONCATENATE i_tran_defn-tcode i_tran_defn-pgmna i_tran_defn-dypno i_tran_defn-menue '80' i_tran_defn-arbgb
*               INTO lwa_data-obj_data SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB. "i_tran_tcode-cinfo = 80
*          MOVE i_tran_tcode TO lwa_data-obj_data.
*          READ TABLE i_tran_tcode INTO lwa_tran_tcode INDEX 1.
*          MOVE lwa_tran_tcode to lwa_data-obj_data.
          APPEND lwa_data TO r_data.
*          IF i_tran_attrib IS NOT INITIAL.
*            CLEAR: lwa_data-obj_ref,
*                   lwa_data-obj_data.
*            lwa_data-obj_ref = 'ATTRIBUTE'.
*            CONCATENATE i_tran_attrib-tcode i_tran_attrib-s_webgui i_tran_attrib-s_win32 i_tran_attrib-s_platin
*          i_tran_attrib-s_service i_tran_attrib-s_pervas i_tran_attrib-s_dummy i_tran_attrib-s_dummy1 i_tran_attrib-s_dummy2
*          into lwa_data-obj_data SEPARATED BY CL_ABAP_CHAR_UTILITIES=>HORIZONTAL_TAB.
***            MOVE i_tran_attrib TO lwa_data-obj_data.
*            APPEND lwa_data TO r_data.
*          ENDIF.
        ELSE.
          RAISE e_tran_defn_miss.
        ENDIF.
      WHEN OTHERS.
        RAISE e_obj_typ_not_supp.
    ENDCASE.
  ENDMETHOD.


  method COMBINE_DATA_UP.
     DATA:lwa_data LIKE LINE OF i_data.

case obj_type.
  when tran.
    loop at i_data INTO lwa_data.
     IF lwa_data-obj_ref = 'DEFN'.
        lwa_data-obj_type = tran.
        lwa_data-obj_name = lwa_data-tcode.
        lwa_data-obj_ref = 'DEFN'.
        lwa_data-line_no = sy-tabix.
        move lwa_data-tcode to E_TRANSACTION.
*        lwa_data-tcode = E_TRANSACTION.
          move lwa_data-prog to E_PROGRAM .
          move lwa_data-dynpro to E_DYNPRO.
          move lwa_data-language to E_LANGUAGE.
          move lwa_data-with_docu to E_WITH_DOCU.
          move lwa_data-docutype to E_DOCUTYPE.
          move lwa_data-development_class to E_DEVELOPMENT_CLASS.
          move lwa_data-transport_number to E_TRANSPORT_NUMBER.
          move lwa_data-transaction_type to E_TRANSACTION_TYPE.
          move lwa_data-shorttext to E_SHORTTEXT.
          move lwa_data-called_transaction to E_CALLED_TRANSACTION.
          move lwa_data-called_transaction_skip to E_CALLED_TRANSACTION_SKIP.
          move lwa_data-variant to E_VARIANT.
          move lwa_data-cl_independend to E_CL_INDEPENDEND.
          move lwa_data-easy_web_transaction to E_EASY_WEB_TRANSACTION.
          move lwa_data-professionel_user_transaction to E_PROFESSIONEL_USER_TRANSACTIO.
          move lwa_data-html_enabled to E_HTML_ENABLED.
          move lwa_data-java_enabled to E_JAVA_ENABLED.
          move lwa_data-winguI_enabled to E_WINGUI_ENABLED.
          move lwa_data-servicefile to E_SERVICEFILE.
          move lwa_data-genflag to E_GENFLAG.
          move lwa_data-suppress_corr_insert to E_SUPPRESS_CORR_INSERT.
*        move lwa_data-obj_data to e_fields.
        ELSE.
          RAISE transaction_miss.
          ENDIF.
          ENDLOOP.
ENDCASE.

  endmethod.


  method CONSTRUCTOR.
    MOVE i_obj_type TO obj_type.
  endmethod.


  method DOWNLOAD_XL.
  endmethod.
ENDCLASS.
