//@AccessControl.authorizationCheck: #NOT_REQUIRED
@ObjectModel.query.implementedBy: 'ABAP:ZCL_CARGA_KARDEX'
@EndUserText.label: 'Version 3'
//@Metadata.ignorePropagatedAnnotations: true
define root custom entity ZI_MAT_QUERY_3
//define root view  ZC_MATDOC_ITEM
// as select from I_MaterialDocumentItem_2
with parameters  //p_matnr :matnr40,
//                 //p_werks :werks_d,
                 p_monat :monat,
                 p_gjahr :gjahr
                 
                 
{

//@UI.selectionField: [{ position: 10 }]

key  material         : matnr40;

//@UI.selectionField: [{ position: 20 }]
  plant         : werks_d;

StorageLocation : lgort_d;

//@UI.selectionField: [{ position: 30 }]

//@UI.selectionField: [{ position: 40 }]
FiscalYearPeriod         : monat;
MaterialDocumentYear         : gjahr;

 // @UI.lineItem    : [{ position: 20 }]
 // werks           : werks_d;

//  @UI.lineItem    : [{ position: 50 }]
  @Semantics.amount.currencyCode: 'currencycode'
  QuantityInEntryUnit   : abap.curr(23,2);
  
  currencycode   : abap.cuky(5);

 //runit :abap.unit( 3 );

//  @UI.lineItem    : [{ position: 60 }]
  @Semantics: { quantity : {unitOfMeasure: 'unit'} }
  quantity        : abap.quan(13,3);
  unit : abap.unit(3);
}
