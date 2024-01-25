class ZAADT_TABL_OPR definition
  public
  final
  create public .

public section.

  constants ACTIVE type CHAR1 value 'A' ##NO_TEXT.
  data TABLE_NAME type DDOBJNAME .
  data TEXT_LANGUAGE type SY-LANGU value 'E' ##NO_TEXT.
  data EXISTS type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  constants TABLE type CHAR4 value 'TABL' ##NO_TEXT.

  methods GET_DATA
    exporting
      value(E_TABL_DEFN) type DD02V
      value(E_TABL_TECH) type DD09V
      value(E_TABL_TT) type /AIF/DD40V_TT
      value(E_TABL_FIELD) type DD03TTYP
      value(E_TABL_FKF) type DD05MTTYP
      value(E_TABL_FK) type DD08VTTYP
      value(E_TABL_TI) type DML_TT_DD12V
      value(E_TABL_IF) type /SAPCND/T_DD17V
      value(E_TABL_HSH) type DD35VTTYP
      value(E_TABL_ASH) type DD36MTTYP
      value(E_CUSTOM_DEP) type ZAADT_CUSTOM_OBJ_DEP_TT
    exceptions
      E_EXISTENCE_CHECK_ERROR
      E_TABL_DATA_GET_ERROR
      E_TABL_NOT_ACTIVE .
  methods SET_DATA
    importing
      value(I_TABL_DEFN) type DD02V
      value(I_TABL_TECH) type DD09V optional
      value(I_TABL_TT) type /AIF/DD40V_TT optional
      value(I_TABL_FIELD) type DD03TTYP optional
      value(I_TABL_FKF) type DD05MTTYP optional
      value(I_TABL_FK) type DD08VTTYP optional
      value(I_TABL_TI) type DML_TT_DD12V optional
      value(I_TABL_IF) type /SAPCND/T_DD17V optional
      value(I_TABL_HSH) type DD35VTTYP optional
      value(I_TABL_ASH) type DD36MTTYP optional
      value(I_CRE_ALT) type CHAR1 default ABAP_TRUE
    exporting
      value(E_NEW_TABL) type DD02V-TABNAME
    exceptions
      E_EXISTENCE_CHECK_ERROR
      E_TABL_DATA_SET_ERROR
      E_TABL_ACTIVE
      E_TABL_EXISTS_INACTIVE
      E_ACTIVATION_ERROR .
  methods ACTIVATE_DO
    exporting
      value(E_SUCCESS_FLAG) type SY-SUBRC
    exceptions
      E_ACTIVATION_ERROR .
  methods CONSTRUCTOR
    importing
      value(I_TABLE_NAME) type DDOBJNAME
      value(I_TEXT_LANGUAGE) type SY-LANGU default 'E' .
protected section.
private section.

  methods CHECK_EXIST
    returning
      value(R_EXISTENCE) type CHAR1
    exceptions
      E_EXISTENCE_CHECK_ERROR
      E_TABLE_DOES_NOT_EXIST .
ENDCLASS.



CLASS ZAADT_TABL_OPR IMPLEMENTATION.


  METHOD activate_do.
    CALL FUNCTION 'DDIF_TABL_ACTIVATE'
      EXPORTING
        name        = table_name
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


  METHOD check_exist.
    DATA:lv_table_name TYPE e071-obj_name.
    MOVE table_name TO lv_table_name.
    CALL FUNCTION 'DD_OBJECT_EXISTS'
      EXPORTING
        class         = table
        name          = lv_table_name
        state         = 'M'
      IMPORTING
        exists        = r_existence
      EXCEPTIONS
        illegal_input = 1.
    IF sy-subrc <> 0 .
      RAISE e_existence_check_error.
    ELSEIF r_existence IS INITIAL.
      RAISE e_table_does_not_exist.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    DATA: lv_state TYPE char1.
    MOVE i_table_name TO table_name.
    MOVE i_text_language TO text_language.
    CALL METHOD me->check_exist
      RECEIVING
        r_existence             = lv_state
      EXCEPTIONS
        e_existence_check_error = 1
        e_table_does_not_exist  = 2
        OTHERS                  = 3.

    IF lv_state = active.
      exists = abap_true.
    ELSEIF sy-subrc = 1 OR sy-subrc = 2.
      exists = abap_false.
    ELSE.
      exists = lv_state.
    ENDIF.

  ENDMETHOD.


  METHOD get_data.
    DATA: lv_state       TYPE char1,
          lwa_custom_dep LIKE LINE OF e_custom_dep.
    IF exists = abap_true.
      CALL FUNCTION 'DDIF_TABL_GET'
        EXPORTING
          name                = table_name
         STATE               = active
         LANGU               = text_language
       IMPORTING
         GOTSTATE            = lv_state
         DD02V_WA            = e_tabl_defn
         DD09L_WA            = e_tabl_tech
       TABLES
         DD03P_TAB           = e_tabl_field
         DD05M_TAB           = e_tabl_fkf
         DD08V_TAB           = e_tabl_fk
         DD12V_TAB           = e_tabl_ti
         DD17V_TAB           = e_tabl_if
         DD35V_TAB           = e_tabl_hsh
         DD36M_TAB           = e_tabl_ash
       EXCEPTIONS
         ILLEGAL_INPUT       = 1
         OTHERS              = 2.

      IF sy-subrc <> 0.
        RAISE e_tabl_data_get_error.
      ELSE.
        IF e_tabl_defn-tabname IS NOT INITIAL.
          IF e_tabl_defn-tabname(1) = 'z'.
            CLEAR lwa_custom_dep.
            lwa_custom_dep-master_obj_typ = table.
            lwa_custom_dep-master_obj_name = me->table_name.
            lwa_custom_dep-dep_obj_type = 'TABL'.
            lwa_custom_dep-dep_obj_name = e_tabl_defn-tabname.
            APPEND lwa_custom_dep TO e_custom_dep.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      RAISE e_tabl_not_active.
    ENDIF.
  ENDMETHOD.


  METHOD set_data.
    DATA : lv_state      TYPE char1,
           lv_flag       TYPE sy-subrc,
           wa_tabl_field TYPE dd03p,
           wa_tabl_fkf   TYPE dd05m,
           wa_tabl_fk    TYPE dd08v,
           wa_tabl_ti    TYPE dd12v,
           wa_tabl_if    TYPE dd17v,
           wa_tabl_hsh   TYPE dd35v,
           wa_tabl_ash   TYPE dd36m.

    IF exists <> abap_true AND exists <> 'N'.
      i_tabl_defn-as4user = sy-uname.
      i_tabl_defn-as4date = sy-datum.
      i_tabl_defn-as4time = sy-uzeit.

      CALL FUNCTION 'DDIF_TABL_PUT'
        EXPORTING
          name              = i_tabl_defn-tabname
          dd02v_wa          = i_tabl_defn
          dd09l_wa          = i_tabl_tech
        TABLES
          dd03p_tab         = i_tabl_field
          dd05m_tab         = i_tabl_fkf
          dd08v_tab         = i_tabl_fk
          dd35v_tab         = i_tabl_hsh
          dd36m_tab         = i_tabl_ash
        EXCEPTIONS
          tabl_not_found    = 1
          name_inconsistent = 2
          tabl_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.

      IF sy-subrc = 0.
        CALL METHOD me->activate_do
          IMPORTING
            e_success_flag     = lv_flag
          EXCEPTIONS
            e_activation_error = 1
            OTHERS             = 2.

        IF sy-subrc <> 0.
          RAISE e_activation_error.
        ELSE.
          e_new_tabl = i_tabl_defn-tabname.
        ENDIF.
      ELSE.
        RAISE e_tabl_data_set_error.
      ENDIF.
    ELSEIF ( exists = abap_true OR exists ='N' ) AND i_cre_alt EQ abap_false.

      IF exists = abap_true.
        RAISE e_tabl_active.
      ELSE.
        RAISE e_tabl_exists_inactive.
      ENDIF.
    ELSEIF  i_cre_alt = abap_true.
      CONCATENATE '1'  i_tabl_defn-tabname INTO  i_tabl_defn-tabname.

      LOOP AT i_tabl_field INTO wa_tabl_field.
        wa_tabl_field-tabname = i_tabl_defn-tabname.
        MODIFY i_tabl_field FROM wa_tabl_field TRANSPORTING tabname.
      ENDLOOP.

      LOOP AT i_tabl_fkf INTO wa_tabl_fkf.
        wa_tabl_fkf-tabname = i_tabl_defn-tabname.
        MODIFY i_tabl_fkf FROM wa_tabl_fkf TRANSPORTING tabname.
      ENDLOOP.

      LOOP AT i_tabl_fk INTO wa_tabl_fk.
        wa_tabl_fk-tabname = i_tabl_defn-tabname.
        MODIFY i_tabl_fk FROM wa_tabl_fk TRANSPORTING tabname.
      ENDLOOP.

      LOOP AT i_tabl_hsh INTO wa_tabl_hsh.
        wa_tabl_hsh-tabname = i_tabl_defn-tabname.
        MODIFY i_tabl_hsh FROM wa_tabl_hsh TRANSPORTING tabname.
      ENDLOOP.

      LOOP AT i_tabl_ash INTO wa_tabl_ash.
        wa_tabl_ash-tabname = i_tabl_defn-tabname.
        MODIFY i_tabl_ash FROM wa_tabl_ash TRANSPORTING tabname.
      ENDLOOP.

      i_tabl_defn-as4user = sy-uname.
      i_tabl_defn-as4date = sy-datum.
      i_tabl_defn-as4time = sy-uzeit.

      CALL FUNCTION 'DDIF_TABL_PUT'
        EXPORTING
          name                    = i_tabl_defn-tabname
         DD02V_WA                = i_tabl_defn
         DD09L_WA                = i_tabl_tech
       TABLES
         DD03P_TAB               = i_tabl_field
         DD05M_TAB               = i_tabl_fkf
         DD08V_TAB               = i_tabl_fk
         DD35V_TAB               = i_tabl_hsh
         DD36M_TAB               = i_tabl_ash
       EXCEPTIONS
         TABL_NOT_FOUND          = 1
         NAME_INCONSISTENT       = 2
         TABL_INCONSISTENT       = 3
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
          e_new_tabl = i_tabl_defn-tabname.
        ENDIF.
      ELSE.
        RAISE e_tabl_data_set_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
