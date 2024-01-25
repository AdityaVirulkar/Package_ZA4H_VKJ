class ZAADT_DATA_OPR definition
  public
  final
  create public .

public section.

  data OBJ_TYPE type TROBJTYPE .
  constants DOMAIN type E071-OBJECT value 'DOMA' ##NO_TEXT.

  methods COMBINE_DATA
    importing
      value(I_DO_DEFN) type DD01V optional
      value(I_DO_VALU) type /AIF/DD07V_TT optional
    returning
      value(R_DATA) type ZAADT_FORMAT_TT
    exceptions
      E_OBJ_TYP_NOT_SUPP
      E_DO_DEFN_MISS .
  methods COMBINE_DATA_UP
    importing
      value(I_DATA) type ZAADT_FORMAT_TT
    exporting
      value(E_DO_DEFN) type DD01V
      value(E_DO_VALU) type /AIF/DD07V_TT
    exceptions
      I_OBJ_TYP_NOT_SUPP
      E_DO_DEFN_MISS .
  methods CONSTRUCTOR
    importing
      value(I_OBJ_TYPE) type TROBJTYPE .
  methods DOWNLOAD_XL
    importing
      value(R_DATA) type DATA optional .
protected section.
private section.
ENDCLASS.



CLASS ZAADT_DATA_OPR IMPLEMENTATION.


  METHOD combine_data.
    DATA:lwa_data    LIKE LINE OF r_data,
         lwa_do_valu LIKE LINE OF i_do_valu.
    CASE obj_type.
      WHEN domain.
        IF i_do_defn IS NOT INITIAL.
          CLEAR lwa_data.
          lwa_data-obj_type = domain.
          lwa_data-obj_name = i_do_defn-domname.
          lwa_data-obj_ref = 'DEFN'.
          MOVE i_do_defn TO lwa_data-obj_data.
          APPEND lwa_data TO r_data.

          IF i_do_valu IS NOT INITIAL.
            CLEAR:lwa_data-obj_ref,
                  lwa_data-obj_data.
            lwa_data-obj_ref = 'VALU'.
            LOOP AT i_do_valu INTO lwa_do_valu.
              lwa_data-line_no = sy-tabix.
              MOVE lwa_do_valu TO lwa_data-obj_data.
              APPEND lwa_data TO r_data.
            ENDLOOP.
          ENDIF.
        ELSE.
          RAISE e_do_defn_miss.
        ENDIF.
      WHEN OTHERS.
        RAISE e_obj_typ_not_supp.
    ENDCASE.
  ENDMETHOD.


  METHOD combine_data_up.
    DATA:
          lwa_data    LIKE LINE OF i_data,
*          i_data    LIKE LINE OF i_data,
         lwa_do_valu LIKE LINE OF e_do_valu.

case obj_type.
  when domain.
    LOOP AT i_data INTO lwa_data.

      IF lwa_data-obj_ref EQ 'DEFN'.
        lwa_data-obj_type = domain.
        lwa_data-obj_name = e_do_defn-domname.
        lwa_data-obj_ref = 'DEFN'.
        MOVE lwa_data-obj_data TO e_do_defn.
      ELSEIF lwa_data-obj_ref EQ 'VALU'.
        MOVE lwa_data-obj_data TO lwa_do_valu.
        APPEND lwa_do_valu TO e_do_valu.
      ELSE.
        RAISE e_do_defn_miss.

      ENDIF.

    ENDLOOP.
ENDCASE.
  ENDMETHOD.


  METHOD constructor.
    MOVE i_obj_type TO obj_type.
  ENDMETHOD.


  method DOWNLOAD_XL.
  endmethod.
ENDCLASS.
