CLASS zcl_carga_kardex_v2 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_rap_query_provider .

    TYPES: rng_matnr           TYPE RANGE OF matnr,
           rng_werks           TYPE RANGE OF werks_d,
           zc_kdx_kardex_v2_tt TYPE STANDARD TABLE OF zc_kdx_kardex_v2 WITH EMPTY KEY.

    METHODS:
      get_last_day_of_month
        IMPORTING iv_date       TYPE datum
        RETURNING VALUE(rv_end) TYPE datum,

      cargar_kardex_mes
        IMPORTING
          iv_year          TYPE gjahr
          iv_month         TYPE monat
          it_matnr_rng     TYPE rng_matnr
          it_werks_rng     TYPE rng_werks
        RETURNING
          VALUE(rt_result) TYPE zc_kdx_kardex_v2_tt.


  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_CARGA_KARDEX_V2 IMPLEMENTATION.


  METHOD cargar_kardex_mes.

    DATA: lv_fecha_ini TYPE datum,
          lv_fecha_fin TYPE datum.

    " Obtener fechas
    lv_fecha_ini = |{ iv_year }{ iv_month }01|.
    lv_fecha_fin = get_last_day_of_month( iv_date = lv_fecha_ini ).

    " HASHED TABLE para movimientos del mes
    TYPES: BEGIN OF ty_mov_mes,
             material                 TYPE zc_kdx_kardex_v2-material,
             plant                    TYPE zc_kdx_kardex_v2-plant,
             quantityinbaseunit       TYPE zc_kdx_kardex_v2-quantityinbaseunit,
             totalgoodsmvtamtincccrcy TYPE zc_kdx_kardex_v2-totalgoodsmvtamtincccrcy,
           END OF ty_mov_mes.

    TYPES: ty_mov_mes_tab TYPE HASHED TABLE OF ty_mov_mes WITH UNIQUE KEY material plant.

    DATA: lt_mov_mes TYPE ty_mov_mes_tab.

    " 1. Movimientos del mes
**    SELECT
**      material,
**      plant,
**      SUM( CASE WHEN debitcreditcode = 'S' THEN quantityinbaseunit
**                WHEN debitcreditcode = 'H' THEN - quantityinbaseunit ELSE 0 END ) AS quantityinbaseunit,
**      SUM( CASE WHEN debitcreditcode = 'S' THEN totalgoodsmvtamtincccrcy
**                WHEN debitcreditcode = 'H' THEN - totalgoodsmvtamtincccrcy ELSE 0 END ) AS totalgoodsmvtamtincccrcy
**          FROM i_materialdocumentitem_2
**      WHERE postingdate BETWEEN @lv_fecha_ini AND @lv_fecha_fin
**        AND material IN @it_matnr_rng
**        AND plant IN @it_werks_rng
**        AND consumptionposting NOT IN ( 'E', 'P' )
***        AND isautomaticallycreated NE 'X'
***        AND storagelocation NE @space
***        AND ISSGORRCVGMATERIAL EQ @space
**       AND GoodsMovementType NOT IN ( '342' )
**      GROUP BY material, plant
**      INTO TABLE @lt_mov_mes.

    SELECT
     product AS material,
     plant,
     SUM( quantity ) AS quantityinbaseunit,
     SUM( amountincompanycodecurrency ) AS totalgoodsmvtamtincccrcy
     FROM i_journalentryitem
     WHERE postingdate BETWEEN @lv_fecha_ini AND @lv_fecha_fin
     AND product IN @it_matnr_rng
     AND plant IN @it_werks_rng
     AND referencedocumenttype IN ( 'MKPF', 'RMRP' )
     AND sourceledger EQ '0L'
     AND financialaccounttype EQ 'M'
      GROUP BY product, plant
     INTO TABLE @lt_mov_mes.


    " 2. Última fecha antes del mes
    TYPES: BEGIN OF ty_ult_fecha,
             material    TYPE i_materialdocumentitem_2-material,
             plant       TYPE i_materialdocumentitem_2-plant,
             postingdate TYPE i_materialdocumentitem_2-postingdate,
           END OF ty_ult_fecha.

    TYPES: ty_ult_fecha_tt TYPE STANDARD TABLE OF ty_ult_fecha WITH EMPTY KEY.

    DATA: lt_ult_fecha TYPE ty_ult_fecha_tt.

**    SELECT material, plant, MAX( postingdate ) AS postingdate
**      FROM i_materialdocumentitem_2
**      WHERE postingdate < @lv_fecha_ini
**        AND material IN @it_matnr_rng
**        AND plant    IN @it_werks_rng
**      GROUP BY material, plant
**      INTO TABLE @lt_ult_fecha.

    SELECT product AS material, plant, MAX( postingdate ) AS postingdate
    FROM i_journalentryitem
    WHERE postingdate < @lv_fecha_ini
      AND product IN @it_matnr_rng
      AND plant    IN @it_werks_rng
    GROUP BY product, plant
    INTO TABLE @lt_ult_fecha.


    " 3. Movimientos históricos
    TYPES: BEGIN OF ty_hist,
             material                 TYPE i_materialdocumentitem_2-material,
             plant                    TYPE i_materialdocumentitem_2-plant,
             postingdate              TYPE i_materialdocumentitem_2-postingdate,
             debitcreditcode          TYPE i_materialdocumentitem_2-debitcreditcode,
             quantityinbaseunit       TYPE i_materialdocumentitem_2-quantityinbaseunit,
             totalgoodsmvtamtincccrcy TYPE i_materialdocumentitem_2-totalgoodsmvtamtincccrcy,
           END OF ty_hist.

*  DATA: lt_hist_2 TYPE STANDARD TABLE OF i_materialdocumentitem_2,
    TYPES: ty_kardex_saldo TYPE zc_kdx_kardex_v2.

    DATA: lt_saldos TYPE HASHED TABLE OF ty_kardex_saldo
                    WITH UNIQUE KEY material plant.

    DATA:     lt_hist   TYPE STANDARD TABLE OF ty_hist.
*        lt_saldos TYPE HASHED TABLE OF zc_kdx_kardex_v2
*                  WITH UNIQUE KEY material plant.

    IF lt_ult_fecha IS NOT INITIAL.
**      SELECT i_material~materialdocument,                    "@@WARN_OK SELECT_FOR_ALL_ENTRIES
**             i_material~materialdocumentitem,
**             i_material~material,
**             i_material~plant,
**             i_material~postingdate,
**             i_material~debitcreditcode,
**             i_material~quantityinbaseunit,
**             i_material~totalgoodsmvtamtincccrcy,
**             i_material~goodsmovementrefdoctype,
**             i_material~goodsreceipttype,
**             i_material~materialdocumentline
**        FROM @lt_ult_fecha as lt_fecha
***        FOR ALL ENTRIES IN @lt_ult_fecha
**        INNER JOIN i_materialdocumentitem_2 as i_material
**        ON   i_material~material    = lt_fecha~material
**        AND  i_material~plant       = lt_fecha~plant
**        AND  i_material~postingdate <= lt_fecha~postingdate
**        WHERE i_material~consumptionposting NOT IN ( 'E', 'P' )
**           AND i_material~GoodsMovementType NOT IN ( '342' )
**          INTO TABLE @DATA(lt_hist_2).


      SELECT "i_material~materialdocument,                    "@@WARN_OK SELECT_FOR_ALL_ENTRIES
             "i_material~materialdocumentitem,
             i_material~product AS material,
             i_material~plant,
             i_material~postingdate,
             i_material~quantity AS quantityinbaseunit,
             i_material~amountincompanycodecurrency AS totalgoodsmvtamtincccrcy
        FROM @lt_ult_fecha AS lt_fecha
*        FOR ALL ENTRIES IN @lt_ult_fecha
        INNER JOIN i_journalentryitem AS i_material
        ON   i_material~product    = lt_fecha~material
        AND  i_material~plant       = lt_fecha~plant
        AND  i_material~postingdate <= lt_fecha~postingdate
        AND i_material~referencedocumenttype IN ( 'MKPF', 'RMRP' )
        AND i_material~sourceledger EQ '0L'
        AND i_material~financialaccounttype EQ 'M'
          INTO TABLE @DATA(lt_hist_2).

      lt_hist = CORRESPONDING #( lt_hist_2 ).
    ENDIF.

    " 4. Calcular saldos iniciales
    LOOP AT lt_hist INTO DATA(ls_hist).

      DATA(lv_q_ini) = CONV decfloat16( ls_hist-quantityinbaseunit ).
      DATA(lv_v_ini) = CONV decfloat16( ls_hist-totalgoodsmvtamtincccrcy ).

      READ TABLE lt_saldos ASSIGNING FIELD-SYMBOL(<fs_saldo>)
           WITH TABLE KEY        material = ls_hist-material
                                 plant    = ls_hist-plant.

      IF sy-subrc = 0.
        <fs_saldo>-initialquantity += lv_q_ini.
        <fs_saldo>-initialvalue    += lv_v_ini.
      ELSE.
        INSERT VALUE #( material        = ls_hist-material
                        plant           = ls_hist-plant
                        calendaryear    = iv_year
                        calendarmonth   = iv_month
                        initialquantity = lv_q_ini
                        initialvalue    = lv_v_ini ) INTO TABLE lt_saldos.
      ENDIF.

    ENDLOOP.

    " 5. Resultado final (saldo inicial + movimientos del mes)
    rt_result = VALUE zc_kdx_kardex_v2_tt( ).

    " --- Tipos de claves y tabla de claves (EXPLÍCITOS) ---
    TYPES: BEGIN OF ty_key,
             material TYPE zc_kdx_kardex_v2-material,
             plant    TYPE zc_kdx_kardex_v2-plant,
           END OF ty_key.
    TYPES ty_key_tab TYPE SORTED TABLE OF ty_key WITH UNIQUE KEY material plant.

    DATA lt_keys TYPE ty_key_tab.

    " Claves desde movimientos del mes (tipo explícito en VALUE)
    lt_keys = VALUE ty_key_tab(
                FOR m IN lt_mov_mes
                ( material = m-material plant = m-plant )
              ).

    " Agregar claves desde saldos iniciales (fila por fila)
    LOOP AT lt_saldos INTO DATA(ls_saldo).
      INSERT VALUE ty_key(
               material = ls_saldo-material
               plant    = ls_saldo-plant ) INTO TABLE lt_keys.
      " si ya existe, sy-subrc = 4 y se ignora (tabla SORTED UNIQUE)
    ENDLOOP.
*    FIELD-SYMBOLS: "<fs_saldo> TYPE ty_kardex_saldo,
*                   <fs_mov>   TYPE ty_mov_mes.

    DATA: ls_mov TYPE ty_mov_mes.

    LOOP AT lt_keys INTO DATA(ls_key).

      CLEAR: ls_mov, ls_saldo.

      READ TABLE lt_saldos INTO ls_saldo
           WITH TABLE KEY material = ls_key-material plant = ls_key-plant.
      READ TABLE lt_mov_mes INTO ls_mov
           WITH TABLE KEY material = ls_key-material plant = ls_key-plant.

      DATA(lv_ini_qty) = CONV decfloat16( ls_saldo-initialquantity ).
      DATA(lv_ini_val) = CONV decfloat16( ls_saldo-initialvalue ).
      DATA(lv_mov_qty) = CONV decfloat16( ls_mov-quantityinbaseunit ).
      DATA(lv_mov_val) = CONV decfloat16( ls_mov-totalgoodsmvtamtincccrcy ).

      APPEND VALUE zc_kdx_kardex_v2(
        material                 = ls_key-material
        plant                    = ls_key-plant
        calendaryear             = iv_year
        calendarmonth            = iv_month
        initialquantity          = lv_ini_qty
        initialvalue             = lv_ini_val
        quantityinbaseunit       = lv_ini_qty + lv_mov_qty
        totalgoodsmvtamtincccrcy = lv_ini_val + lv_mov_val
      ) TO rt_result.

    ENDLOOP.
***    LOOP AT lt_saldos INTO DATA(ls_mov).
***
***      READ TABLE lt_mov_mes ASSIGNING FIELD-SYMBOL(<fs_mov>)
***           WITH TABLE KEY material = ls_mov-material
***                                  plant    = ls_mov-plant.
***
***      APPEND VALUE #(
***        material                 = ls_mov-material
***        plant                    = ls_mov-plant
***        calendaryear             = iv_year
***        calendarmonth            = iv_month
***        quantityinbaseunit       = COND #( WHEN sy-subrc = 0
***                                           THEN <fs_mov>-quantityinbaseunit + ls_mov-initialquantity
***                                           ELSE ls_mov-initialquantity )
***        totalgoodsmvtamtincccrcy = COND #( WHEN sy-subrc = 0
***                                           THEN <fs_mov>-totalgoodsmvtamtincccrcy + ls_mov-initialvalue
***                                           ELSE ls_mov-initialvalue )
***        initialquantity          =  ls_mov-initialquantity
***
***        initialvalue             =  ls_mov-initialvalue
***
***      ) TO rt_result.
***
***    ENDLOOP.

  ENDMETHOD.


  METHOD get_last_day_of_month.

    DATA lv_next_month  TYPE datum.

    lv_next_month = iv_date.
    lv_next_month+6(2) = '01'.
    lv_next_month+4(2) = lv_next_month+4(2) + 1.

    IF lv_next_month+4(2) > '12'.
      lv_next_month+4(2) = '01'.
      lv_next_month+0(4) = lv_next_month+0(4) + 1.
    ENDIF.

    rv_end = lv_next_month - 1.


  ENDMETHOD.


  METHOD if_rap_query_provider~select.


*    "--------------------------------------------------------------
*    " 1. Declaración de variables
*    "--------------------------------------------------------------
    DATA: lv_year      TYPE gjahr,
          lv_month     TYPE monat,
          lv_fecha_ini TYPE datum,
          lv_fecha_fin TYPE datum.
*
    DATA: lv_skip     TYPE i,
          lv_top      TYPE i,
          lv_max_rows TYPE i.
*
*    "--------------------------------------------------------------
*    " 2. Leer parámetros de entrada
*    "--------------------------------------------------------------
*
*
    DATA(lt_parameters) = io_request->get_parameters( ).

    READ TABLE lt_parameters INTO DATA(ls_param)
     WITH KEY parameter_name = 'P_MONTH'.
    IF sy-subrc = 0.
      lv_month = ls_param-value.
    ENDIF.

    READ TABLE lt_parameters INTO ls_param
  WITH KEY parameter_name = 'P_YEAR'.
    IF sy-subrc = 0.
      lv_year = ls_param-value.
    ENDIF.

    DATA: lt_matnr_rng TYPE RANGE OF matnr,
          lt_werks_rng TYPE RANGE OF werks_d.

    TRY.
        DATA(lt_filters) = io_request->get_filter( )->get_as_ranges( ).
      CATCH cx_rap_query_filter_no_range INTO DATA(lx_no_range).
        RETURN.
    ENDTRY.
*
    LOOP AT lt_filters INTO DATA(lo_filter).
      CASE lo_filter-name.
        WHEN 'MATERIAL'.
          LOOP AT lo_filter-range INTO DATA(ls_range).
            APPEND VALUE #( sign = ls_range-sign
                            option = ls_range-option
                            low = ls_range-low
                            high = ls_range-high ) TO lt_matnr_rng.
          ENDLOOP.

        WHEN 'PLANT'.
          LOOP AT lo_filter-range INTO ls_range.
            APPEND VALUE #( sign = ls_range-sign
                            option = ls_range-option
                            low = ls_range-low
                            high = ls_range-high ) TO lt_werks_rng.
          ENDLOOP.

      ENDCASE.
    ENDLOOP.



   DATA(lt_result) = cargar_kardex_mes(
                      iv_year      = lv_year
                      iv_month     = lv_month
                      it_matnr_rng = lt_matnr_rng
                      it_werks_rng = lt_werks_rng ).
*
*    DATA: lt_mov_mes TYPE STANDARD TABLE OF zc_kdx_kardex_v2 WITH EMPTY KEY.
*
*    " Obtener primer y ultimo dia del mes
*
*    lv_fecha_ini = |{ lv_year }{ lv_month }01|.
*    lv_fecha_fin   = get_last_day_of_month( iv_date = lv_fecha_ini ).
*
*    "--------------------------------------------------------------
*    " 3. Obtener parámetros de paginación
*    "--------------------------------------------------------------

    lv_skip     = io_request->get_paging( )->get_offset( ).
    lv_top      = io_request->get_paging( )->get_page_size( ).
    lv_max_rows = COND #( WHEN lv_top = if_rap_query_paging=>page_size_unlimited THEN 0 ELSE lv_top ).
*
*
*    "--------------------------------------------------------------
*    " 4. Obtener movimientos del mes
*    "--------------------------------------------------------------
*    TRY.
*        SELECT
*         material,
*         plant,
*         @lv_year  AS calendaryear,
*         @lv_month AS calendarmonth,
*         SUM(
*           CASE
*             WHEN debitcreditcode EQ 'S' THEN quantityinbaseunit
*             WHEN debitcreditcode EQ 'H' THEN - quantityinbaseunit
*             ELSE 0
*           END
*         ) AS quantityinbaseunit,
*         SUM(
*           CASE
*             WHEN debitcreditcode EQ 'S' THEN totalgoodsmvtamtincccrcy
*             WHEN debitcreditcode EQ 'H' THEN - totalgoodsmvtamtincccrcy
*             ELSE 0
*           END
*         ) AS totalgoodsmvtamtincccrcy
*         FROM i_materialdocumentitem_2
*         WHERE postingdate BETWEEN @lv_fecha_ini AND @lv_fecha_fin
*         AND   material IN @lt_matnr_rng
*         AND   plant    IN @lt_werks_rng
*         AND consumptionposting NOT IN ( 'A', 'E', 'P' )
**         AND goodsmovementrefdoctype not in ( 'B' )
**         AND goodsreceipttype NOT IN ( 'X' )
*         AND isautomaticallycreated NE 'X'
*         AND storagelocation NE @space
*         GROUP BY material, plant, fiscalyearperiod
*         ORDER BY material, plant, fiscalyearperiod
*         INTO TABLE @lt_mov_mes.
*
*
*        "--------------------------------------------------------------
*        " 5. Buscar última fecha anterior para saldo inicial
*        "--------------------------------------------------------------
*        SELECT material, plant, MAX( postingdate ) AS postingdate
*          FROM i_materialdocumentitem_2
*          WHERE postingdate < @lv_fecha_ini
**          WHERE postingdate < @lv_fecha_fin
*          AND   material IN @lt_matnr_rng
*          AND   plant    IN @lt_werks_rng
*          GROUP BY material, plant
*          INTO TABLE @DATA(lt_ult_fecha).
*
*        " Eliminar materiales con movimiento
**        LOOP AT lt_ult_fecha INTO DATA(ls_fecha).
**          READ TABLE lt_mov_mes WITH KEY material = ls_fecha-material plant = ls_fecha-plant TRANSPORTING NO FIELDS.
**          IF sy-subrc = 0.
**            DELETE lt_ult_fecha WHERE material = ls_fecha-material AND plant = ls_fecha-plant.
**          ENDIF.
**        ENDLOOP.
*
*        "----------------------------------------------------------
*        " 4. MOVIMIENTOS HISTÓRICOS (SALDO INICIAL)
*        "----------------------------------------------------------
*        "--------------------------------------------------------------
*        " 6. Obtener movimientos históricos (saldo inicial)
*        "--------------------------------------------------------------
*        TYPES: BEGIN OF ty_hist,
*                 material                 TYPE i_materialdocumentitem_2-material,
*                 plant                    TYPE i_materialdocumentitem_2-plant,
*                 postingdate              TYPE i_materialdocumentitem_2-postingdate,
*                 debitcreditcode          TYPE i_materialdocumentitem_2-debitcreditcode,
*                 quantityinbaseunit       TYPE i_materialdocumentitem_2-quantityinbaseunit,
*                 totalgoodsmvtamtincccrcy TYPE i_materialdocumentitem_2-totalgoodsmvtamtincccrcy,
*               END OF ty_hist.
*
*        DATA lt_hist TYPE STANDARD TABLE OF ty_hist.
*        TYPES: zc_kdx_kardex_v2_tt TYPE STANDARD TABLE OF zc_kdx_kardex_v2 WITH EMPTY KEY.
*
*        IF lt_ult_fecha IS NOT INITIAL.
*          SELECT
*           materialdocument,
*           materialdocumentitem,
*          material,
*          plant,
*          postingdate,
*          debitcreditcode,
*          quantityinbaseunit,
*          totalgoodsmvtamtincccrcy,
*          goodsmovementrefdoctype,
*          goodsreceipttype,
*          MaterialDocumentLine
*           FROM i_materialdocumentitem_2
*     FOR ALL ENTRIES IN @lt_ult_fecha
*     WHERE material    = @lt_ult_fecha-material
*              AND plant       = @lt_ult_fecha-plant
*       AND postingdate <= @lt_ult_fecha-postingdate
*       AND consumptionposting NOT IN ( 'A', 'E', 'P' )
*       AND isautomaticallycreated NE 'X'
*       AND storagelocation NE @space
**              AND ISSUINGORRECEIVINGSTOCKTYPE NOT IN ('06')
*     INTO TABLE @DATA(lt_hist_2).
*
**          LOOP AT lt_hist_2 INTO DATA(ls_hist_2).
**
**            DATA(lv_index) = sy-tabix.
**
**            IF  ls_hist_2-goodsmovementrefdoctype EQ 'B'
**            AND ls_hist_2-goodsreceipttype EQ 'X'.
**
**              DELETE lt_hist_2 INDEX lv_index.
**            ENDIF.
**          ENDLOOP.
*
**SELECT * from I_JournalEntryItem
**FOR ALL ENTRIES IN @lt_hist_2
**WHERE ReferenceDocument = @lt_hist_2-MaterialDocument
**AND   ReferenceDocumentItem = @lt_hist_2-MaterialDocumentLine
**INTO TABLE @DATA(lt_prueba).
*
*
*          lt_hist = CORRESPONDING #( lt_hist_2 ).
**          SELECT
**                 material,
**                 plant,
**                 postingdate,
**                 debitcreditcode,
**                 quantityinbaseunit,
**                 totalgoodsmvtamtincccrcy FROM i_materialdocumentitem_2
**            FOR ALL ENTRIES IN @lt_ult_fecha
**            WHERE material    = @lt_ult_fecha-material
***              AND plant       = @lt_ult_fecha-plant
**              AND postingdate <= @lt_ult_fecha-postingdate
**            INTO TABLE @lt_hist.
*        ENDIF.
*
*        " Calcular saldos
*        DATA lt_saldos_sinmov TYPE zc_kdx_kardex_v2_tt.
*        LOOP AT lt_hist INTO DATA(ls_hist).
*          DATA(lv_q_ini) = COND decfloat16(
*                              WHEN ls_hist-debitcreditcode = 'S' THEN  ls_hist-quantityinbaseunit
*                              WHEN ls_hist-debitcreditcode = 'H' THEN - ls_hist-quantityinbaseunit
*                              ELSE 0 ).
*          DATA(lv_v_ini) = COND decfloat16(
*                              WHEN ls_hist-debitcreditcode = 'S' THEN  ls_hist-totalgoodsmvtamtincccrcy
*                              WHEN ls_hist-debitcreditcode = 'H' THEN - ls_hist-totalgoodsmvtamtincccrcy
*                              ELSE 0 ).
*
*          READ TABLE lt_saldos_sinmov INTO DATA(ls_saldo_existente)
*               WITH KEY material = ls_hist-material
*                        plant    = ls_hist-plant.
*          DATA(lv_idx) = sy-tabix.
*
*          IF sy-subrc = 0 AND lv_idx > 0.
*            " Ya existe, acumular y modificar con INDEX
*            ls_saldo_existente-initialquantity += lv_q_ini.
*            ls_saldo_existente-initialvalue    += lv_v_ini.
*            MODIFY lt_saldos_sinmov FROM ls_saldo_existente INDEX lv_idx.
*          ELSE.
*            " Nuevo registro
*            APPEND VALUE zc_kdx_kardex_v2(
*              material        = ls_hist-material
*              plant           = ls_hist-plant
*              calendaryear    = lv_year
*              calendarmonth   = lv_month
*              initialquantity = lv_q_ini
*              initialvalue    = lv_v_ini ) TO lt_saldos_sinmov.
*          ENDIF.
*        ENDLOOP.
*
*        "--------------------------------------------------------------
*        " 7. Armar resultado final
*        "--------------------------------------------------------------
*        DATA(lt_result) = VALUE zc_kdx_kardex_v2_tt( ).
*
*        LOOP AT lt_saldos_sinmov INTO DATA(ls_mov).
*
*          READ TABLE  lt_mov_mes INTO DATA(ls_sin) WITH KEY material = ls_mov-material
*                                                                  plant    = ls_mov-plant.
*
*          IF sy-subrc = 0.
*
*            APPEND VALUE #( material                 = ls_mov-material
*                            plant                    = ls_mov-plant
**                      postingdate              = ls_mov-postingdate
*                            calendaryear             = lv_year
*                            calendarmonth            = lv_month
*                            quantityinbaseunit       = ls_sin-quantityinbaseunit + ls_mov-initialquantity "ls_mov-quantityinbaseunit
*                            totalgoodsmvtamtincccrcy = ls_sin-totalgoodsmvtamtincccrcy + ls_mov-initialvalue "ls_mov-totalgoodsmvtamtincccrcy
**                          initialquantity          = 0
**                          initialvalue             = 0 ) TO lt_result.
*                            initialquantity          = ls_mov-initialquantity "ls_sin-InitialQuantity
*                            initialvalue             = ls_mov-initialvalue ) TO lt_result. "ls_sin-InitialValue ) TO lt_result.
*          ELSE.
*            APPEND VALUE #( material                 = ls_mov-material
*                          plant                    = ls_mov-plant
**                      postingdate              = ls_mov-postingdate
*                          calendaryear             = lv_year
*                          calendarmonth            = lv_month
*                          quantityinbaseunit       = 0
*                          totalgoodsmvtamtincccrcy = 0 "ls_mov-totalgoodsmvtamtincccrcy
**                          initialquantity          = 0
**                          initialvalue             = 0 ) TO lt_result.
*                          initialquantity          = ls_mov-initialquantity "ls_sin-InitialQuantity
*                          initialvalue             = ls_mov-initialvalue ) TO lt_result. "ls_sin-InitialValue ) TO lt_result
*
*          ENDIF.
*        ENDLOOP.
*
**        APPEND LINES OF lt_saldos_sinmov TO lt_result.
*
*        "--------------------------------------------------------------
*        " 8. Paginación en memoria (solo si es necesario)
*        "--------------------------------------------------------------
        DATA(lt_paged_result) = lt_result.
        IF lv_max_rows > 0.
          lt_paged_result = VALUE #( FOR idx = lv_skip + 1 THEN idx + 1
                                     WHILE idx <= lines( lt_result ) AND
                                           idx <= lv_skip + lv_max_rows
                                     LET ls_row = lt_result[ idx ] IN
                                     ( ls_row ) ).
        ENDIF.
*
*        "--------------------------------------------------------------
*        " 9. Retornar datos y total
*        "--------------------------------------------------------------
        io_response->set_data( lt_paged_result ).

        IF io_request->is_total_numb_of_rec_requested( ).
          io_response->set_total_number_of_records(
            iv_total_number_of_records = lines( lt_result )
          ).
        ENDIF.
*        "--------------------------------------------------------------
*        " 10. Manejo de excepciones
*        "--------------------------------------------------------------
*      CATCH cx_rap_query_provider INTO DATA(lx_query).
*        " Registrar mensaje
*        RAISE EXCEPTION lx_query.
*
*
**
**      CATCH cx_root INTO DATA(lx_root).
**RAISE EXCEPTION lx_root.
*    ENDTRY.



  ENDMETHOD.
ENDCLASS.
