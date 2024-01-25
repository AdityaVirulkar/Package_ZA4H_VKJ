class ZAADT_TTYP_OPR definition
  public
  final
  create public .

public section.

  constants ACTIVE type CHAR1 value 'A' ##NO_TEXT.
  data TTYP_NAME type DDOBJNAME .
  data TEXT_LANGUAGE type SY-LANGU value 'E' ##NO_TEXT.
  data EXISTS type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  constants TTYPE type CHAR4 value 'TTYP' ##NO_TEXT.

  methods GET_DATA
    exporting
      value(E_TTYP_DEFN) type DD40V
      value(E_TTYP_KEY) type /SAPCND/T_DD42V
      value(E_TTYP_STRUCTURE) type DD43V_TAB
      value(E_CUSTOM_DEP) type ZAADT_CUSTOM_OBJ_DEP_TT
    exceptions
      E_EXISTENCE_CHECK_ERROR
      E_TTYP_DATA_GET_ERROR
      E_TTYP_NOT_ACTIVE .
  methods SET_DATA
    importing
      value(I_TTYP_DEFN) type DD40V
      value(I_TTYP_KEY) type /SAPCND/T_DD42V optional
      value(I_TTYP_STRUCTURE) type DD43V_TAB optional
      value(I_CRE_ALT) type CHAR1 default ABAP_TRUE
    exporting
      value(E_NEW_TTYP) type DD01V-DOMNAME
    exceptions
      E_EXISTENCE_CHECK_ERROR
      E_TTYP_DATA_SET_ERROR
      E_TTYP_ACTIVE
      E_TTYP_EXISTS_INACTIVE
      E_ACTIVATION_ERROR .
  methods ACTIVATE_DO
    exporting
      value(E_SUCCESS_FLAG) type SY-SUBRC
    exceptions
      E_ACTIVATION_ERROR .
  methods CONSTRUCTOR
    importing
      value(I_TTYP_NAME) type DDOBJNAME
      value(I_TEXT_LANGUAGE) type SY-LANGU default 'E' .
protected section.
private section.

  methods CHECK_EXIST
    returning
      value(R_EXISTENCE) type CHAR1
    exceptions
      E_EXISTENCE_CHECK_ERROR
      E_TTYP_DOES_NOT_EXIST .
ENDCLASS.



CLASS ZAADT_TTYP_OPR IMPLEMENTATION.


  METHOD activate_do.
    CALL FUNCTION 'DDIF_TTYP_ACTIVATE'
      EXPORTING
        name        = ttyp_name
*       PRID        = -1
      IMPORTING
        rc          = e_success_flag
      EXCEPTIONS
        not_found   = 1
        put_failure = 2
        OTHERS      = 3.

    IF sy-subrc <> 0.
      RAISE e_activation_error.
    ELSE.
      e_success_flag = 0.
    ENDIF.
  ENDMETHOD.


  method CHECK_EXIST.
   DATA:lv_ttyp_name TYPE e071-obj_name.
    MOVE ttyp_name TO lv_ttyp_name.
    CALL FUNCTION 'DD_OBJECT_EXISTS'
      EXPORTING
        class         = ttype
        name          = lv_ttyp_name
        state         = 'M'
      IMPORTING
        exists        = r_existence
      EXCEPTIONS
        illegal_input = 1.
    IF sy-subrc <> 0 .
      RAISE e_existence_check_error.
    ELSEIF r_existence IS INITIAL.
      RAISE e_ttyp_does_not_exist.
    ENDIF.
  endmethod.


  method CONSTRUCTOR.
    DATA: lv_state TYPE char1.
    MOVE i_ttyp_name TO ttyp_name.
    MOVE i_text_language TO text_language.

     CALL METHOD me->check_exist
      RECEIVING
        r_existence             = lv_state
      EXCEPTIONS
        e_existence_check_error = 1
        e_ttyp_does_not_exist  = 2
        OTHERS                  = 3.

    IF lv_state = active.
      exists = abap_true.
    ELSEIF sy-subrc = 1 OR sy-subrc = 2.
      exists = abap_false.
    ELSE.
      exists = lv_state.
    ENDIF.

  endmethod.


  METHOD get_data.
    DATA:lv_state       TYPE char1,
         lwa_custom_dep LIKE LINE OF e_custom_dep.
    IF exists = abap_true.
      CALL FUNCTION 'DDIF_TTYP_GET'
        EXPORTING
          name          = ttyp_name
          state         = active
          langu         = text_language
        IMPORTING
          gotstate      = lv_state
          dd40v_wa      = e_ttyp_defn
        TABLES
          dd42v_tab     = e_ttyp_key
          dd43v_tab     = e_ttyp_structure
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        RAISE e_ttyp_data_get_error.
      ELSE.
        IF e_ttyp_defn-typename IS NOT INITIAL.
          IF e_ttyp_defn-typename(1) = 'Z'.
            CLEAR lwa_custom_dep.
            lwa_custom_dep-master_obj_typ = ttype.
            lwa_custom_dep-master_obj_name = me->ttyp_name.
            lwa_custom_dep-dep_obj_type = 'TTYP'.
            lwa_custom_dep-dep_obj_name = e_ttyp_defn-typename .
            APPEND lwa_custom_dep TO e_custom_dep.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      RAISE e_ttyp_not_active.
    ENDIF.
  ENDMETHOD.


  METHOD set_data.
    DATA : lv_state          TYPE char1,
           lv_flag           TYPE sy-subrc,
           wa_ttyp_key       TYPE dd42v,
           wa_ttyp_structure TYPE dd43v.

 IF exists <> abap_true AND exists <> 'N'.
      i_ttyp_defn-as4user = sy-uname.
      i_ttyp_defn-as4date = sy-datum.
      i_ttyp_defn-as4time = sy-uzeit.

CALL FUNCTION 'DDIF_TTYP_PUT'
  EXPORTING
    name                    = i_ttyp_defn-typename
   DD40V_WA                =  i_ttyp_defn
 TABLES
   DD42V_TAB               = i_ttyp_key
   DD43V_TAB               = i_ttyp_structure
 EXCEPTIONS
   TTYP_NOT_FOUND          = 1
   NAME_INCONSISTENT       = 2
   TTYP_INCONSISTENT       = 3
   PUT_FAILURE             = 4
   PUT_REFUSED             = 5
   OTHERS                  = 6.

IF sy-subrc = 0.
        CALL METHOD activate_do(
          IMPORTING
            e_success_flag     = lv_flag
          EXCEPTIONS
            e_activation_error = 1 ).
        IF sy-subrc <> 0.
          RAISE e_activation_error.
        ELSE.
          e_new_ttyp = i_ttyp_defn-typename.
        ENDIF.
      ELSE.
        RAISE e_ttyp_data_set_error.
      ENDIF.
 ELSEIF ( exists = abap_true OR exists ='N' ) AND i_cre_alt EQ abap_false.

 IF exists = abap_true.
        RAISE e_ttyp_active.
      ELSE.
        RAISE e_ttyp_exists_inactive.
      ENDIF.
    ELSEIF  i_cre_alt = abap_true.
      CONCATENATE '1'  i_ttyp_defn-typename INTO  i_ttyp_defn-typename.

loop at i_ttyp_key INTO wa_ttyp_key.
  wa_ttyp_key-typename = i_ttyp_defn-typename.
  MODIFY i_ttyp_key FROM wa_ttyp_key TRANSPORTING typename.
ENDLOOP.

loop at i_ttyp_structure INTO wa_ttyp_structure.
  wa_ttyp_structure-typename = i_ttyp_defn-typename.
  MODIFY i_ttyp_structure FROM wa_ttyp_structure TRANSPORTING typename.
ENDLOOP.

  i_ttyp_defn-as4user = sy-uname.
      i_ttyp_defn-as4date = sy-datum.
      i_ttyp_defn-as4time = sy-uzeit.

CALL FUNCTION 'DDIF_TTYP_PUT'
  EXPORTING
    name                    = i_ttyp_defn-typename
   DD40V_WA                = i_ttyp_defn
 TABLES
   DD42V_TAB               = i_ttyp_key
   DD43V_TAB               = i_ttyp_structure
 EXCEPTIONS
   TTYP_NOT_FOUND          = 1
   NAME_INCONSISTENT       = 2
   TTYP_INCONSISTENT       = 3
   PUT_FAILURE             = 4
   PUT_REFUSED             = 5
   OTHERS                  = 6.

IF sy-subrc = 0.
 CALL METHOD me->activate_do
          IMPORTING
            e_success_flag     = lv_flag
          EXCEPTIONS
            e_activation_error = 1
            OTHERS             = 2.

        IF sy-subrc <> 0.
          RAISE e_activation_error.
        ENDIF.
        IF lv_flag = 0.
          e_new_ttyp = i_ttyp_defn-typename.
        ENDIF.
      ELSE.
        RAISE e_ttyp_data_set_error.
      ENDIF.
ENDIF.
  ENDMETHOD.
ENDCLASS.
