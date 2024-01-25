*&---------------------------------------------------------------------*
*& Report za4h_vkj_p1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT za4h_vkj_p1.

SELECT BELNR,
       GJAHR,
       BUKRS,
       BUDAT
 FROM BKPF INTO TABLE @DATA(git_bkpf) WHERE BUDAT GT @SY-datum.





 SELECT bp_id,
company_name,
so~currency_code,
SUM( so~gross_amount ) AS total_gross_amount
FROM snwd_so AS so
INNER JOIN snwd_bpa AS bpa
ON so~buyer_guid = bpa~node_key
INTO TABLE @DATA(lt_result)
GROUP BY bp_id, company_name, so~currency_code.

    TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = DATA(go_alv)
          CHANGING
            t_table      = lt_result.
      CATCH cx_salv_msg .
    ENDTRY.


    go_alv->display( ).
