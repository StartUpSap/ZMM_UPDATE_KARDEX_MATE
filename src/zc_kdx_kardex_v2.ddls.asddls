//@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.query.implementedBy: 'ABAP:ZCL_CARGA_KARDEX_V2'
@EndUserText.label: 'Root custom'
//@Metadata.ignorePropagatedAnnotations: true
define root custom entity  ZC_KDX_KARDEX_V2
with parameters 
    p_year  : gjahr,
    p_month : monat
  
{
  key Material           : matnr;
  key Plant              : werks_d;
//  MaterialBaseUnit       : meins;
//  CompanyCodeCurrency    : waers;
  CalendarYear           : gjahr;
  CalendarMonth          : monat;
//  PostingDate            : budat;
//  GoodsMovementType      : bwart;
//  MovementCategory              : abap.char( 3 );
  QuantityInBaseUnit            : abap.dec( 15, 3 );
  TotalGoodsMvtAmtInCCCrcy      : abap.dec( 23, 2 );
  InitialQuantity               : abap.dec( 15, 3 );
  InitialValue                  : abap.dec( 23, 2 );
}
