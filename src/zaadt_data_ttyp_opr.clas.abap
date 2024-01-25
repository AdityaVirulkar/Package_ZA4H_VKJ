class ZAADT_DATA_TTYP_OPR definition
  public
  final
  create public .

public section.

  data OBJ_TYPE type TROBJTYPE .
  constants TTYPE type E071-OBJECT value 'TTYP' ##NO_TEXT.

  methods COMBINE_DATA
    importing
      value(I_TTYP_DEFN) type DD40V optional
      value(I_TTYP_KEY) type /SAPCND/T_DD42V optional
      value(I_TTYP_STRUCTURE) type DD43V_TAB optional
    returning
      value(R_DATA) type ZAADT_FORMAT_TT
    exceptions
      E_OBJ_TYP_NOT_SUPP
      E_TTYP_DEFN_MISS .
  methods COMBINE_DATA_UP
    importing
      !I_DATA type ZAADT_FORMAT_TT
    exporting
      value(E_TTYP_DEFN) type DD40V
      value(E_TTYP_KEY) type /SAPCND/T_DD42V
      value(E_TTYP_STRUCTURE) type DD43V_TAB
    exceptions
      I_OBJ_TYP_NOT_SUPP
      E_TTYP_DEFN_MISS .
  methods CONSTRUCTOR
    importing
      value(I_OBJ_TYPE) type TROBJTYPE .
  methods DOWNLOAD_XL
    importing
      value(R_DATA) type DATA optional .
protected section.
private section.
ENDCLASS.



CLASS ZAADT_DATA_TTYP_OPR IMPLEMENTATION.


  method COMBINE_DATA.
    DATA:lwa_data    LIKE LINE OF r_data,
          lwa_ttyp_key LIKE LINE OF i_ttyp_key,
          lwa_ttyp_structure LIKE LINE OF i_ttyp_structure.
    CASE obj_type.
      WHEN ttype.
        IF i_ttyp_defn is not INITIAL.
          CLEAR lwa_data.
           lwa_data-obj_type = ttype.
          lwa_data-obj_name = i_ttyp_defn-typename.
          lwa_data-obj_ref = 'DEFN'.
       MOVE i_ttyp_defn to lwa_data-obj_data.
        APPEND lwa_data TO r_data.

        if i_ttyp_key is not INITIAL.
          CLEAR:lwa_data-obj_ref,
                  lwa_data-obj_data.
           lwa_data-obj_ref = 'key feilds'.
           LOOP at i_ttyp_key INTO lwa_ttyp_key.
             lwa_data-line_no = sy-tabix.
             MOVE lwa_ttyp_key TO lwa_data-obj_data.
             append lwa_data to r_data.
             ENDLOOP.
        ENDIF.

        if i_ttyp_structure is NOT INITIAL.
          CLEAR:lwa_data-obj_ref,
                  lwa_data-obj_data.
           lwa_data-obj_ref = 'structure'.
           LOOP at i_ttyp_structure INTO lwa_ttyp_structure.
             lwa_data-line_no = sy-tabix.
             MOVE lwa_ttyp_structure TO lwa_data-obj_data.
             append lwa_data to r_data.
             ENDLOOP.
             endif.
             ELSE.
             raise e_ttyp_defn_miss.
             ENDIF.
             when OTHERS.
               raise e_obj_typ_not_supp.
        ENDCASE.
  endmethod.


  METHOD combine_data_up.
    DATA : lwa_data           LIKE LINE OF i_data,
           lwa_ttyp_key       LIKE LINE OF e_ttyp_key,
           lwa_ttyp_structure LIKE LINE OF e_ttyp_structure.

    CASE obj_type.
      WHEN ttype.
        LOOP AT i_data INTO lwa_data.
          IF lwa_data-obj_ref EQ 'DEFN'.
            lwa_data-obj_type = ttype.
            lwa_data-obj_name = e_ttyp_defn-typename.
            lwa_data-obj_ref = 'DEFN'.
            MOVE lwa_data-obj_data TO e_ttyp_defn.
          ELSEIF lwa_data-obj_ref EQ 'KEY FIELDS'.
            MOVE lwa_data-obj_data TO lwa_ttyp_key.
          ELSEIF lwa_data-obj_ref EQ 'structure'.
            MOVE lwa_data-obj_data TO lwa_ttyp_structure.
          ELSE.
            RAISE e_ttyp_defn_miss.
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
