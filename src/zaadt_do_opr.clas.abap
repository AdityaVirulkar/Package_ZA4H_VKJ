class ZAADT_DO_OPR definition
  public
  final
  create public .

public section.

  constants ACTIVE type CHAR1 value 'A' ##NO_TEXT.
  data DOMAIN_NAME type DDOBJNAME .
  data TEXT_LANGUAGE type SY-LANGU value 'E' ##NO_TEXT.
  data EXISTS type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  constants DOMAIN type CHAR4 value 'DOMA' ##NO_TEXT.

  methods GET_DATA
    exporting
      value(E_DO_DEFN) type DD01V
      value(E_DO_VALUES) type /AIF/DD07V_TT
      value(E_CUSTOM_DEP) type ZAADT_CUSTOM_OBJ_DEP_TT
    exceptions
      E_EXISTENCE_CHECK_ERROR
      E_DO_DATA_GET_ERROR
      E_DO_NOT_ACTIVE .
  methods SET_DATA
    importing
      value(I_DO_DEFN) type DD01V
      value(I_DO_VALUES) type /AIF/DD07V_TT optional
      value(I_CRE_ALT) type CHAR1 default ABAP_TRUE
    exporting
      value(E_NEW_DO) type DD01V-DOMNAME
    exceptions
      E_EXISTENCE_CHECK_ERROR
      E_DO_DATA_SET_ERROR
      E_DO_ACTIVE
      E_DO_EXISTS_INACTIVE
      E_ACTIVATION_ERROR .
  methods ACTIVATE_DO
    exporting
      value(E_SUCCESS_FLAG) type SY-SUBRC
    exceptions
      E_ACTIVATION_ERROR .
  methods CONSTRUCTOR
    importing
      value(I_DOMAIN_NAME) type DDOBJNAME
      value(I_TEXT_LANGUAGE) type SY-LANGU default 'E' .
protected section.
private section.

  methods CHECK_EXIST
    returning
      value(R_EXISTENCE) type CHAR1
    exceptions
      E_EXISTENCE_CHECK_ERROR
      E_DOMAIN_DOES_NOT_EXIST .
ENDCLASS.



CLASS ZAADT_DO_OPR IMPLEMENTATION.


  METHOD activate_do.
    CALL FUNCTION 'DDIF_DOMA_ACTIVATE'
      EXPORTING
        name        = domain_name
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
    DATA:lv_domain_name TYPE e071-obj_name.
    MOVE domain_name TO lv_domain_name.
    CALL FUNCTION 'DD_OBJECT_EXISTS'
      EXPORTING
        class         = domain
        name          = lv_domain_name
        state         = 'M'
      IMPORTING
        exists        = r_existence
      EXCEPTIONS
        illegal_input = 1.
    IF sy-subrc <> 0 .
      RAISE e_existence_check_error.
    ELSEIF r_existence IS INITIAL.
      RAISE e_domain_does_not_exist.
    ENDIF.
  ENDMETHOD.


  METHOD constructor.
    DATA:lv_state      TYPE char1.
    MOVE i_domain_name TO domain_name.
    MOVE i_text_language TO text_language.

    CALL METHOD check_exist(
      RECEIVING
        r_existence             = lv_state
      EXCEPTIONS
        e_existence_check_error = 1
        e_domain_does_not_exist = 2 ).

    IF lv_state = active.
      exists = abap_true.
    ELSEIF sy-subrc = 1 OR sy-subrc = 2.
      exists = abap_false.
    ELSE.
      exists = lv_state.
    ENDIF.
  ENDMETHOD.


  METHOD get_data.
    DATA:lv_state       TYPE char1,
         lwa_custom_dep LIKE LINE OF e_custom_dep.
    IF exists = abap_true.
      CALL FUNCTION 'DDIF_DOMA_GET'
        EXPORTING
          name          = domain_name
          state         = active
          langu         = text_language
        IMPORTING
          gotstate      = lv_state
          dd01v_wa      = e_do_defn
        TABLES
          dd07v_tab     = e_do_values
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.
      IF sy-subrc <> 0.
        RAISE e_do_data_get_error.
      ELSE.
        IF e_do_defn-entitytab IS NOT INITIAL.
          IF e_do_defn-entitytab(1) = 'Z'.
            CLEAR lwa_custom_dep.
            lwa_custom_dep-master_obj_typ = domain.
            lwa_custom_dep-master_obj_name = me->domain_name.
            lwa_custom_dep-dep_obj_type = 'DOMA'.
            lwa_custom_dep-dep_obj_name = e_do_defn-entitytab .
            APPEND lwa_custom_dep TO e_custom_dep.
          ENDIF.
          IF e_do_defn-convexit(1) = 'Z'.
            CLEAR lwa_custom_dep.
            lwa_custom_dep-master_obj_typ = domain.
            lwa_custom_dep-master_obj_name = me->domain_name.
            lwa_custom_dep-dep_obj_type = 'CONV'.
            lwa_custom_dep-dep_obj_name = e_do_defn-convexit .
            APPEND lwa_custom_dep TO e_custom_dep.
          ENDIF.
        ENDIF.
      ENDIF.
    ELSE.
      RAISE e_do_not_active.
    ENDIF.
  ENDMETHOD.


  METHOD set_data.
    DATA:lv_state     TYPE char1,
         lv_flag      TYPE sy-subrc,
*         e_domain_name TYPE e071-obj_name,
         wa_do_values TYPE dd07v.
*    MOVE i_do_defn-domname TO e_domain_name.


    IF exists <> abap_true AND exists <> 'N'.

*      Create Domain
      i_do_defn-as4user = sy-uname.
      i_do_defn-as4date = sy-datum.
      i_do_defn-as4time = sy-uzeit.

      CALL FUNCTION 'DDIF_DOMA_PUT'
        EXPORTING
          name              = i_do_defn-domname
          dd01v_wa          = i_do_defn
        TABLES
          dd07v_tab         = i_do_values
        EXCEPTIONS
          doma_not_found    = 1
          name_inconsistent = 2
          doma_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.

      IF sy-subrc = 0.
        CALL METHOD activate_do(
          IMPORTING
            e_success_flag     = lv_flag
          EXCEPTIONS
            e_activation_error = 1 ).
        IF sy-subrc <> 0.
          RAISE e_activation_error.
        ELSE.
          e_new_do = i_do_defn-domname.
        ENDIF.
      ELSE.
        RAISE e_do_data_set_error.
      ENDIF.
    ELSEIF ( exists = abap_true OR exists ='N' ) AND i_cre_alt EQ abap_false.

      IF exists = abap_true.
        RAISE e_do_active.
      ELSE.
        RAISE e_do_exists_inactive.
      ENDIF.
    ELSEIF  i_cre_alt = abap_true.
      CONCATENATE '1'  i_do_defn-domname INTO  i_do_defn-domname.
*      CONCATENATE '1'  i_do_values-domname INTO  i_do_values-domname. "mine
      LOOP AT i_do_values INTO wa_do_values.
        wa_do_values-domname = i_do_defn-domname.
*        wa_do_values-domname = i_do_values-domname.      "mine
        MODIFY i_do_values FROM wa_do_values TRANSPORTING domname.
      ENDLOOP.

      i_do_defn-as4user = sy-uname.
      i_do_defn-as4date = sy-datum.
      i_do_defn-as4time = sy-uzeit.

      CALL FUNCTION 'DDIF_DOMA_PUT'
        EXPORTING
          name              = i_do_defn-domname
          dd01v_wa          = i_do_defn
        TABLES
          dd07v_tab         = i_do_values
        EXCEPTIONS
          doma_not_found    = 1
          name_inconsistent = 2
          doma_inconsistent = 3
          put_failure       = 4
          put_refused       = 5
          OTHERS            = 6.

      IF sy-subrc = 0.
        CALL METHOD activate_do(
          IMPORTING
            e_success_flag     = lv_flag
          EXCEPTIONS
            e_activation_error = 1 ).
        IF sy-subrc <> 0.
          RAISE e_activation_error.
        ENDIF.
        IF lv_flag = 0.
          e_new_do = i_do_defn-domname.
        ENDIF.
      ELSE.
        RAISE e_do_data_set_error.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
