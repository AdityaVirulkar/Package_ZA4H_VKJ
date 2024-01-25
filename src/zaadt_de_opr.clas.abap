class ZAADT_DE_OPR definition
  public
  final
  create public .

public section.

  constants ACTIVE type CHAR1 value 'A' ##NO_TEXT.
  data DATAELEMENT_NAME type DDOBJNAME .
  data TEXT_LANGUAGE type SY-LANGU value 'E' ##NO_TEXT.
  data EXISTS type ABAP_BOOL value ABAP_FALSE ##NO_TEXT.
  constants DATAELEMENT type CHAR4 value 'DTEL' ##NO_TEXT.

  methods GET_DATA
    exporting
      value(E_DE_DEFN) type DD04V
      value(E_DE_TECH) type TPARA
      value(E_CUSTOM_DEP) type ZAADT_CUSTOM_OBJ_DEP_TT
    exceptions
      E_EXISTENCE_CHECK_ERROR
      E_DE_DATA_GET_ERROR
      E_DE_NOT_ACTIVE .
  methods SET_DATA
    importing
      value(I_DE_DEFN) type DD04V
      value(I_DE_TECH) type TPARA optional
      value(I_CRE_ALT) type CHAR1 default ABAP_TRUE
    exporting
      value(E_NEW_DE) type DD04V-ROLLNAME
    exceptions
      E_EXISTENCE_CHECK_ERROR
      E_DE_DATA_SET_ERROR
      E_DE_ACTIVE
      E_DE_EXISTS_INACTIVE
      E_ACTIVATION_ERROR .
  methods ACTIVATE_DO
    exporting
      value(E_SUCCESS_FLAG) type SY-SUBRC
    exceptions
      E_ACTIVATION_ERROR .
  methods CONSTRUCTOR
    importing
      value(I_DATAELEMENT_NAME) type DDOBJNAME
      value(I_TEXT_LANGUAGE) type SY-LANGU default 'E' .
protected section.
private section.

  methods CHECK_EXIST
    returning
      value(R_EXISTENCE) type CHAR1
    exceptions
      E_EXISTENCE_CHECK_ERROR
      E_DATAELEMENT_DOES_NOT_EXIST .
ENDCLASS.



CLASS ZAADT_DE_OPR IMPLEMENTATION.


  method ACTIVATE_DO.
    CALL FUNCTION 'DDIF_DTEL_ACTIVATE'
      EXPORTING
        name              = dataelement_name

     IMPORTING
       RC                = e_success_flag
     EXCEPTIONS
       NOT_FOUND         = 1
       PUT_FAILURE       = 2
       OTHERS            = 3
              .
    IF sy-subrc <> 0.
    RAISE e_activation_error.
    else.
      e_success_flag = 0.
    ENDIF.

  endmethod.


  METHOD check_exist.
    DATA: lv_dataelement_name TYPE e071-obj_name.
    MOVE dataelement_name TO lv_dataelement_name.
    CALL FUNCTION 'DD_OBJECT_EXISTS'
      EXPORTING
        class         = dataelement
        name          = lv_dataelement_name
        state         = 'M'
      IMPORTING
        exists        = r_existence
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      RAISE e_existence_check_error.
    ELSEIF r_existence IS INITIAL.
      RAISE e_dataelement_does_not_exist.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.
    DATA: lv_state TYPE char1.
    MOVE i_dataelement_name TO dataelement_name.
    MOVE i_text_language TO text_language.
    CALL METHOD me->check_exist
      RECEIVING
        r_existence                  = lv_state
      EXCEPTIONS
        e_existence_check_error      = 1
        e_dataelement_does_not_exist = 2
        OTHERS                       = 3.
    IF  lv_state = active.
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
      CALL FUNCTION 'DDIF_DTEL_GET'
        EXPORTING
          name          = dataelement_name
          state         = active
          langu         = text_language
        IMPORTING
          gotstate      = lv_state
          dd04v_wa      = e_de_defn
          tpara_wa      = e_de_tech
        EXCEPTIONS
          illegal_input = 1
          OTHERS        = 2.


      IF sy-subrc <> 0.
        RAISE e_de_data_get_error.
      ELSE.
        IF e_de_defn-rollname IS NOT INITIAL.
          IF e_de_defn-rollname(1) = 'Z'.
            CLEAR lwa_custom_dep.
            lwa_custom_dep-master_obj_typ = dataelement.
            lwa_custom_dep-master_obj_name = me->dataelement_name.
            lwa_custom_dep-dep_obj_type = 'DTEL'.
            lwa_custom_dep-dep_obj_name = e_de_defn-rollname.
            APPEND lwa_custom_dep TO e_custom_dep.
*      ENDIF.
*  IF E_DE_DEFN-CONVEXIT(1) = 'Z'.
*    CLEAR LWA_CUSTOM_DEP.
*     LWA_CUSTOM_DEP-MASTER_OBJ_TYP = DATAELEMENT.
*     LWA_CUSTOM_DEP-MASTER_OBJ_NAME = ME->DATAELEMENT_NAME.
*     LWA_CUSTOM_DEP-DEP_OBJ_TYPE = 'CONV'.
*     LWA_CUSTOM_DEP-DEP_OBJ_NAME = E_DE_DEFN-CONVEXIT.
*    APPEND LWA_CUSTOM_DEP TO E_CUSTOM_DEP.
          ENDIF.

        ELSE.
          RAISE e_de_not_active.
        ENDIF.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD set_data.
    DATA:lv_state TYPE char1,
         lv_flag  TYPE sy-subrc.

    IF exists <> abap_true AND exists <> 'N'.
      i_de_defn-as4user = sy-uname.
      i_de_defn-as4date = sy-datum.
      i_de_defn-as4time = sy-uzeit.

      CALL FUNCTION 'DDIF_DTEL_PUT'
        EXPORTING
          name              = i_de_defn-rollname
          dd04v_wa          = i_de_defn
        EXCEPTIONS
          dtel_not_found    = 1
          name_inconsistent = 2
          dtel_inconsistent = 3
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
          e_new_de = i_de_defn-rollname.
        ENDIF.
      ELSE.
        RAISE e_de_data_set_error.
      ENDIF.
    ELSEIF ( exists = abap_true OR exists = 'N' ) AND i_cre_alt EQ abap_false.
      IF exists = abap_true.
        RAISE e_de_active.
      ELSE.
        RAISE e_de_exists_inactive.
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
