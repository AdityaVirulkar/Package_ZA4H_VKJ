*&---------------------------------------------------------------------*
*& Report za4h_vkj_p2
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT za4h_vkj_p2.

SELECT bp_id,
company_name,
so~currency_code,
SUM( so~gross_amount )
AS total_amount
FROM snwd_so AS so
INNER JOIN snwd_bpa AS bpa
ON bpa~node_key = so~buyer_guid
INTO TABLE @DATA(lt_result)
WHERE so~delivery_status = ' '
GROUP BY
bp_id,
company_name,
so~currency_code
HAVING SUM( so~gross_amount ) > 1000.


TRY.
        CALL METHOD cl_salv_table=>factory
          IMPORTING
            r_salv_table = DATA(go_alv)
          CHANGING
            t_table      = lt_result.
      CATCH cx_salv_msg .
    ENDTRY.


    go_alv->display( ).
