class ZAADT_DATA_TABL_OPR definition
  public
  final
  create public .

public section.

  data OBJ_TYPE type TROBJTYPE .
  constants TABLE type E071-OBJECT value 'Tabl' ##NO_TEXT.
  constants STRUCTURE type E071-OBJECT value 'STRU' ##NO_TEXT.

  methods COMBINE_DATA
    importing
      value(I_TABL_DEFN) type DD02V optional
      value(I_TABL_TECH) type DD09V optional
      value(I_TABL_TT) type /AIF/DD40V_TT optional
      value(I_TABL_FIELD) type DD03TTYP optional
      value(I_TABL_FKF) type DD05MTTYP optional
      value(I_TABL_FK) type DD08VTTYP optional
      value(I_TABL_TI) type DML_TT_DD12V optional
      value(I_TABL_IF) type /SAPCND/T_DD17V optional
      value(I_TABL_HSH) type DD35VTTYP optional
      value(I_TABL_ASH) type DD36MTTYP optional
    returning
      value(R_DATA) type ZAADT_FORMAT_TT
    exceptions
      E_OBJ_TYP_NOT_SUPP
      E_TABL_DEFN_MISS .
  methods COMBINE_DATA_UP
    importing
      value(I_DATA) type ZAADT_FORMAT_TT
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
    exceptions
      I_OBJ_TYP_NOT_SUPP
      E_TABL_DEFN_MISS .
  methods CONSTRUCTOR
    importing
      value(I_OBJ_TYPE) type TROBJTYPE .
  methods DOWNLOAD_XL
    importing
      value(R_DATA) type DATA optional .
protected section.
private section.
ENDCLASS.



CLASS ZAADT_DATA_TABL_OPR IMPLEMENTATION.


  METHOD combine_data.
    DATA : lwa_data       LIKE LINE OF r_data,
           lwa_tabl_field LIKE LINE OF i_tabl_field,
           lwa_tabl_fkf LIKE LINE OF i_tabl_fkf,
           lwa_tabl_fk LIKE LINE OF i_tabl_fk,
           lwa_tabl_ti LIKE LINE OF i_tabl_ti,
           lwa_tabl_if LIKE LINE OF i_tabl_if,
           lwa_tabl_hsh LIKE LINE OF i_tabl_hsh,
           lwa_tabl_ash LIKE LINE OF i_tabl_ash.

    CASE obj_type.
*      when structure.
*         if i_tabl_defn-TABCLASS = 'INTTAB'.
*            CLEAR : lwa_data-obj_type,
*                    lwa_data-obj_ref,
*                  lwa_data-obj_data.
*            lwa_data-obj_type = structure.
*            lwa_data-obj_name = i_tabl_defn-tabname.
*            lwa_data-obj_ref = 'STRU'.
*               MOVE i_tabl_defn TO lwa_data-obj_data.
*          APPEND lwa_data TO r_data.
*          endif.

      WHEN table.
        IF i_tabl_defn IS NOT INITIAL.
          if i_tabl_defn-TABCLASS = 'INTTAB'.
          CLEAR lwa_data.
          lwa_data-obj_type = structure.
            lwa_data-obj_name = i_tabl_defn-tabname.
            lwa_data-obj_ref = 'STRU'.
               MOVE i_tabl_defn TO lwa_data-obj_data.
          APPEND lwa_data TO r_data.
          ELSE.
             CLEAR : lwa_data-obj_type,
                    lwa_data-obj_ref,
                  lwa_data-obj_data.
          lwa_data-obj_type = table.
          lwa_data-obj_name = i_tabl_defn-tabname.
          lwa_data-obj_ref = 'TABL'.
*          lwa_data-line_no = sy-tabix.
          MOVE i_tabl_defn TO lwa_data-obj_data.
          APPEND lwa_data TO r_data.
         endif.
*          if i_tabl_defn-TABCLASS = 'INTTAB'.
*            CLEAR : lwa_data-obj_type,
*                    lwa_data-obj_ref,
*                  lwa_data-obj_data.
*            lwa_data-obj_type = structure.
*            lwa_data-obj_name = i_tabl_defn-tabname.
*            lwa_data-obj_ref = 'STRU'.
*               MOVE i_tabl_defn TO lwa_data-obj_data.
*          APPEND lwa_data TO r_data.
*          endif.


         IF i_tabl_field is not INITIAL.
           CLEAR: lwa_data-obj_ref,
                  lwa_data-obj_data.
           lwa_data-obj_ref = 'field'.
           LOOP AT i_tabl_field INTO lwa_tabl_field.
            lwa_data-line_no = sy-tabix.
            move lwa_tabl_field to lwa_data-obj_data.
            APPEND lwa_data to r_data.
           ENDLOOP.
         ENDIF.

          IF i_tabl_fkf is not INITIAL.
           CLEAR: lwa_data-obj_ref,
                  lwa_data-obj_data.
           lwa_data-obj_ref = 'key field'.
           LOOP AT i_tabl_fkf INTO lwa_tabl_fkf.
            lwa_data-line_no = sy-tabix.
            move lwa_tabl_fkf to lwa_data-obj_data.
            APPEND lwa_data to r_data.
           ENDLOOP.
         ENDIF.

               IF i_tabl_fk is not INITIAL.
           CLEAR: lwa_data-obj_ref,
                  lwa_data-obj_data.
           lwa_data-obj_ref = 'foreign keys'.
           LOOP AT i_tabl_fk INTO lwa_tabl_fk.
            lwa_data-line_no = sy-tabix.
            move lwa_tabl_fk to lwa_data-obj_data.
            APPEND lwa_data to r_data.
           ENDLOOP.
         ENDIF.

          IF i_tabl_ti is not INITIAL.
           CLEAR: lwa_data-obj_ref,
                  lwa_data-obj_data.
           lwa_data-obj_ref = 'table indexes'.
           LOOP AT i_tabl_ti INTO lwa_tabl_ti.
            lwa_data-line_no = sy-tabix.
            move lwa_tabl_ti to lwa_data-obj_data.
            APPEND lwa_data to r_data.
           ENDLOOP.
         ENDIF.

        IF i_tabl_if is not INITIAL.
           CLEAR: lwa_data-obj_ref,
                  lwa_data-obj_data.
           lwa_data-obj_ref = 'index field'.
           LOOP AT i_tabl_if INTO lwa_tabl_if.
            lwa_data-line_no = sy-tabix.
            move lwa_tabl_if to lwa_data-obj_data.
            APPEND lwa_data to r_data.
           ENDLOOP.
         ENDIF.

           IF i_tabl_hsh is not INITIAL.
           CLEAR: lwa_data-obj_ref,
                  lwa_data-obj_data.
           lwa_data-obj_ref = 'header search help'.
           LOOP AT i_tabl_hsh INTO lwa_tabl_hsh.
            lwa_data-line_no = sy-tabix.
            move lwa_tabl_hsh to lwa_data-obj_data.
            APPEND lwa_data to r_data.
           ENDLOOP.
         ENDIF.

        IF i_tabl_ash is not INITIAL.
           CLEAR: lwa_data-obj_ref,
                  lwa_data-obj_data.
           lwa_data-obj_ref = 'allocation search help'.
           LOOP AT i_tabl_ash INTO lwa_tabl_ash.
            lwa_data-line_no = sy-tabix.
            move lwa_tabl_ash to lwa_data-obj_data.
            APPEND lwa_data to r_data.
           ENDLOOP.
         ENDIF.

        ELSE.
          RAISE e_tabl_defn_miss.
        ENDIF.
      WHEN OTHERS.
        RAISE e_obj_typ_not_supp.
    ENDCASE.

  ENDMETHOD.


  METHOD combine_data_up.
    DATA : lwa_data       LIKE LINE OF i_data,
           lwa_tabl_field LIKE LINE OF e_tabl_field,
           lwa_tabl_fkf   LIKE LINE OF e_tabl_fkf,
           lwa_tabl_fk    LIKE LINE OF e_tabl_fk,
           lwa_tabl_ti    LIKE LINE OF e_tabl_ti,
           lwa_tabl_if    LIKE LINE OF e_tabl_if,
           lwa_tabl_hsh   LIKE LINE OF e_tabl_hsh,
           lwa_tabl_ash   LIKE LINE OF e_tabl_ash.
    case obj_type.
      when table.
    LOOP AT i_data INTO lwa_data.
      IF lwa_data-obj_ref = 'TABL'.
        MOVE lwa_data-obj_data TO e_tabl_defn.

        ELSEIF lwa_data-obj_ref = 'STRU'.
        MOVE lwa_data-obj_data TO e_tabl_defn.

      ELSEIF lwa_data-obj_ref = 'field'.
        MOVE lwa_data-obj_data TO lwa_tabl_field.
        APPEND lwa_tabl_field TO e_tabl_field.

      ELSEIF lwa_data-obj_ref = 'key field'.
        MOVE lwa_data-obj_data TO lwa_tabl_fkf.
        APPEND lwa_tabl_fkf TO e_tabl_fkf.

      ELSEIF lwa_data-obj_ref = 'foreign keys'.
        MOVE lwa_data-obj_data TO lwa_tabl_fkf.
        APPEND lwa_tabl_fk TO e_tabl_fk.

      ELSEIF lwa_data-obj_ref = 'table indexes'.
        MOVE lwa_data-obj_data TO lwa_tabl_ti.
        APPEND lwa_tabl_ti TO e_tabl_ti.

      ELSEIF lwa_data-obj_ref = 'index field'.
        MOVE lwa_data-obj_data TO lwa_tabl_if.
        APPEND lwa_tabl_if TO e_tabl_if.

      ELSEIF lwa_data-obj_ref = 'header search help'.
        MOVE lwa_data-obj_data TO lwa_tabl_hsh.
        APPEND lwa_tabl_hsh TO e_tabl_hsh.

      ELSEIF lwa_data-obj_ref = 'allocation search help'.
        MOVE lwa_data-obj_data TO lwa_tabl_ash.
        APPEND lwa_tabl_ash TO e_tabl_ash.

      ELSE.
        RAISE e_tabl_defn_miss.
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
