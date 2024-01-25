class ZAADT_TC_OPR definition
  public
  final
  create public .

public section.

  data I_TRAN_TCODE type TSTC-TCODE .
  constants TRANSACTION type CHAR4 value 'TRAN' ##NO_TEXT.
  data TRAN_NAME type DDOBJNAME .
  data EXISTS type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  data TEXT_LANGUAGE type SY-LANGU value 'E' ##NO_TEXT.

  methods GET_DATA
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
      value(E_CUSTOM_DEP) type ZAADT_CUSTOM_OBJ_DEP_TT
    exceptions
      E_EXISTENCE_CHECK_ERROR
      E_TRAN_DATA_GET_ERROR
      E_TRAN_NOT_ACTIVE .
  methods SET_DATA
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
      value(I_CRE_ALT) type CHAR1 default ABAP_TRUE
    exporting
      value(E_NEW_TRAN) type TSTC-TCODE
    exceptions
      E_TCODE_DATA_SET_ERROR .
  methods CONSTRUCTOR
    importing
      value(I_TRAN_NAME) type DDOBJNAME
      value(I_TEXT_LANGUAGE) type SY-LANGU default 'E' .
protected section.
private section.

  methods CHECK_EXIST
    returning
      value(R_EXISTENCE) type SY-SUBRC
    exceptions
      E_EXISTENCE_CHECK_ERROR
      E_TCODE_DOES_NOT_EXIST .
ENDCLASS.



CLASS ZAADT_TC_OPR IMPLEMENTATION.


  method CHECK_EXIST.
data : t_code type /SMB/BP_EDT12.

    CALL FUNCTION '/SMB/CHECK_REMOTE_TCODE_EXIST'
      EXPORTING
        tcode         = t_code
     IMPORTING
       RET           = r_existence.

       IF sy-subrc <> 0 .
      RAISE e_existence_check_error.
    ELSEIF r_existence IS INITIAL.
      RAISE e_tcode_does_not_exist.
    ENDIF.

  endmethod.


  method CONSTRUCTOR.
    DATA: lv_state type sy-subrc. "lv_tcode type tcode.
    MOVE i_tran_name TO i_tran_tcode .
    MOVE i_tran_name TO tran_name.
    MOVE i_text_language TO text_language.

CALL METHOD me->check_exist
  RECEIVING
    r_existence             = lv_state
  EXCEPTIONS
    e_existence_check_error = 1
    e_tcode_does_not_exist  = 2
    others                  = 3.

IF lv_state = 0.
   exists = abap_true.
   ELSEIF sy-subrc = 1 or sy-subrc = 4.
     exists = abap_false.
     else.
       exists = lv_state.
ENDIF.


*if i_tran_name is not INITIAL.
*  SELECT single tcode from tstc into lv_tcode WHERE tcode = i_tran_name.
*ELSEIF sy-subrc <> 0 .
*  MESSAGE 'ENTER T-CODE' TYPE 'I'.
*    endif.
  endmethod.


  method GET_DATA.
     DATA:lv_state       TYPE char1,
         lwa_custom_dep LIKE LINE OF e_custom_dep,
         lt_tcodes TYPE STANDARD TABLE OF tstc,
         ls_tcodes TYPE tstc,
         lt_attrib TYPE STANDARD TABLE OF tstcc,
         ls_attrib TYPE tstcc,
         ls_tstct type tstct-ttext.
*if e_tran_defn IS NOT INITIAL.
*if lt_tcodes IS NOT INITIAL.
**APPEND e_tran_defn to lt_tcodes.
*APPEND lt_tcodes to e_tran_defn.
*ENDIF.
**if e_tran_attrib is not INITIAL.
*if lt_attrib is not INITIAL.
**  APPEND e_tran_attrib to lt_attrib.
*  APPEND lt_attrib to e_tran_attrib.
*  ENDIF.
      CALL FUNCTION 'RPY_TRANSACTION_READ'
       EXPORTING
         TRANSACTION            = i_tran_tcode
         PROGRAM                = e_program
         DYNPRO                 = e_dynpro
         TRANSACTION_TYPE       = e_transaction_type
       TABLES
         TCODES                 = lt_tcodes[]
         GUI_ATTRIBUTES         = lt_attrib[]
       EXCEPTIONS
         PERMISSION_ERROR       = 1
         CANCELLED              = 2
         NOT_FOUND              = 3
         OBJECT_NOT_FOUND       = 4
         OTHERS                 = 5
                .
      IF sy-subrc <> 0.
     RAISE e_tran_data_get_error.
     ELSE.
       if i_tran_tcode is not INITIAL.
         if i_tran_tcode(1) = 'Z' .
           LOOP AT lt_tcodes INTO ls_tcodes.
*             MOVE-CORRESPONDING ls_tcodes to e_tran_defn.
             move ls_tcodes-tcode to e_transaction.
             move ls_tcodes-pgmna to e_program.
             move ls_tcodes-dypno to e_dynpro.
ENDLOOP.
select SINGLE ttext from tstct INTO ls_tstct WHERE tcode = i_tran_tcode.
  move ls_tstct to e_shorttext.

              LOOP at lt_attrib INTO ls_attrib.
*                MOVE-CORRESPONDING ls_attrib to e_tran_attrib.
                move ls_attrib-s_webgui to e_html_enabled.
                move ls_attrib-s_platin to e_java_enabled.
                move ls_attrib-s_win32 to e_wingui_enabled.
                move ls_attrib-S_SERVICE to e_servicefile.
                ENDLOOP.

           CLEAR lwa_custom_dep.
            lwa_custom_dep-master_obj_typ = transaction.
            lwa_custom_dep-master_obj_name = me->tran_name.
            lwa_custom_dep-dep_obj_type = transaction.
            lwa_custom_dep-dep_obj_name = i_tran_tcode .
            APPEND lwa_custom_dep TO e_custom_dep.
      ENDIF.
*       if e_tran_prog = 'Z' .
*           CLEAR lwa_custom_dep.
*            lwa_custom_dep-master_obj_typ = transaction.
*            lwa_custom_dep-master_obj_name = me->tran_name.
*            lwa_custom_dep-dep_obj_type = 'tran'.
*            lwa_custom_dep-dep_obj_name = e_tran_defn.
*            APPEND lwa_custom_dep TO e_custom_dep.
*      ENDIF.
endif.
endif.
*endif.
  endmethod.


  method SET_DATA.
if i_tran_tcode is not INITIAL.

CALL FUNCTION 'RPY_TRANSACTION_INSERT'
  EXPORTING
    transaction                         = i_transaction
   PROGRAM                             = i_program
   DYNPRO                              = i_dynpro
   LANGUAGE                            = i_language
   WITH_DOCU                           = i_with_docu
   DOCUTYPE                            = i_docutype
   DEVELOPMENT_CLASS                   = i_development_class
   TRANSPORT_NUMBER                    = i_transport_number
   TRANSACTION_TYPE                    = i_transaction_type
    shorttext                           = i_shorttext
   CALLED_TRANSACTION                  = i_called_transaction
   CALLED_TRANSACTION_SKIP             = i_called_transaction_skip
   VARIANT                             = i_variant
   CL_INDEPENDEND                      = i_cl_independend
   EASY_WEB_TRANSACTION                = i_easy_web_transaction
   PROFESSIONEL_USER_TRANSACTION       = i_professionel_user_transactio
   HTML_ENABLED                        = i_html_enabled
   JAVA_ENABLED                        = i_java_enabled
   WINGUI_ENABLED                      = i_wingui_enabled
   SERVICEFILE                         = i_servicefile
   GENFLAG                             = i_genflag
   SUPPRESS_CORR_INSERT                = i_suppress_corr_insert
* TABLES
*   DOCU_TABLE_USER                     =
*   DOCU_TABLE_TECH                     =
*   PARAM_VALUES                        =
 EXCEPTIONS
   CANCELLED                           = 1
   ALREADY_EXIST                       = 2
   PERMISSION_ERROR                    = 3
   NAME_NOT_ALLOWED                    = 4
   NAME_CONFLICT                       = 5
   ILLEGAL_TYPE                        = 6
   OBJECT_INCONSISTENT                 = 7
   DB_ACCESS_ERROR                     = 8
   OTHERS                              = 9
          .
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

ELSEIF i_cre_alt eq abap_false.
RAISE e_tcode_data_set_error.
endif.

  endmethod.
ENDCLASS.
