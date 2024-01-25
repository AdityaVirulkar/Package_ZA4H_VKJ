class ZAADT_DATA_DE_OPR definition
  public
  final
  create public .

public section.

  data OBJ_TYPE type TROBJTYPE .
  constants DATAELEMENT type E071-OBJECT value 'DTEL' ##NO_TEXT.

  methods COMBINE_DATA
    importing
      value(I_DE_DEFN) type DD04V optional
      value(I_DE_TECH) type TPARA optional
    returning
      value(R_DATA) type ZAADT_FORMAT_TT
    exceptions
      I_OBJ_TYP_NOT_SUPP
      I_DE_DEFN_MISS .
  methods COMBINE_DATA_UP
    importing
      value(I_DATA) type ZAADT_FORMAT_TT optional
    exporting
      value(E_DE_DEFN) type DD04V
      value(E_DE_TECH) type TPARA
    exceptions
      I_OBJ_TYP_NOT_SUPP
      E_DE_DEFN_MISS .
  methods CONSTRUCTOR
    importing
      value(I_OBJ_TYPE) type TROBJTYPE .
  methods DOWNLOAD_XL
    importing
      value(R_DATA) type DATA optional .
protected section.
private section.
ENDCLASS.



CLASS ZAADT_DATA_DE_OPR IMPLEMENTATION.


  METHOD combine_data.
    DATA : lwa_data LIKE LINE OF r_data.

    CASE obj_type.
      WHEN dataelement.
        IF i_de_defn IS NOT INITIAL.
          CLEAR lwa_data.
          lwa_data-obj_type = dataelement.
          lwa_data-obj_name = i_de_defn-rollname.
          lwa_data-obj_ref = 'DEFN'.
          lwa_data-line_no = sy-tabix.
          MOVE i_de_defn TO lwa_data-obj_data.
          APPEND lwa_data TO r_data.
        ELSE.
          RAISE i_de_defn_miss.
        ENDIF.
      WHEN OTHERS.
        RAISE i_obj_typ_not_supp.
    ENDCASE.
  ENDMETHOD.


  METHOD combine_data_up.
    DATA : lwa_data LIKE LINE OF i_data.
case obj_type.
  when dataelement.
    LOOP AT i_data INTO lwa_data.
      IF lwa_data-obj_ref = 'DEFN'.
        MOVE lwa_data-obj_data TO e_de_defn.
      ELSE.
        RAISE e_de_defn_miss.
      ENDIF.
    ENDLOOP.
ENDCASE.
  ENDMETHOD.


  method CONSTRUCTOR.
     MOVE i_obj_type TO obj_type.
  endmethod.


  method DOWNLOAD_XL.
  endmethod.
ENDCLASS.
