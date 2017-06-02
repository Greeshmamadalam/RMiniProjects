require(dplyr)
require(RMySQL)
source('sqlFunctions.R')

koubel_franscine_connect_dplyr <- koubel_franscine_connect_dplyr()
koubel_connect_Rmysql <- koubel_connect_Rmysql()
koubel_connect_dplyr <- koubel_connect_dplyr()

## add project Ids for creating legalfillers for new records  (do not remove project ids from the below list unless there is a duplicate)
projectIds <- list(
  169,335,581,10011,10013,
  10014,10016,10051,10060,
  10078,10083,10084,10096,
  10108,10119,10155,10194,
  10200,10203,10209,10221,
  10227,10253,10319,10539,
  10563,10564,10664,10805,
  10822,10881,10882,10883,
  10943,11003,11028,11032,
  11049,11072,11098,11104,
  11139,11186,11235,11243
)
get.legalFillers <- function(projectId,koubel_connect_Rmysql){
userQuery <- paste0("SELECT
                    project.id as projectId, 
                    business.name as SupplierName,
                    business.description as SupplierDescription,
                    business.contact as SupplierContact,
                    business.phone as SupplierPhone,
                    business.registration_num as supplierRegistrationNum,
                    business.city as SupplierCity,
                    business.state as SupplierState,
                    business.province as SupplierProvince,
                    business.postal_code as SupplierPostalCode,
                    business.address as SupplierAddress,
                    business.type_id as SupplierTypeId,
                    business.company_type_id as SupplierCompanyTypeId,
                    business.organization_id as SupplierOrganizationId,
                    business.legal_registered_country_id as SupplierLegalRegisteredCountryId,
                    business.country_id as SupplierCountryId,
                    business.website as SupplierWebsite,
                    business.incorporation_date as SupplierIncorporationDate,
                    business.address2 as SupplierAddress2,
                    business.contract_signor as SupplierContractSignor,
                    business.contract_signor_title as SupplierContractSignorTitle,
                    business.bank_account_number as SupplierBankAccountNumber,
                    bc.code as SupplierBankAccountCurrency,
                    business.bank_name_on_account as SupplierBankNameOnAccount,
                    business.bank_contact as SupplierBankContact,
                    business.logo_url as SupplierLogoURL,
                    business.bank_name as SupplierBankName,
                    payable.amount as PayableAmount,
                    pc.code as PayableCurrency,
                    payable.due_date as PayableDueDate,
                    payable.bank_account_name as PayableBankAccountName,
                    payable.bank_account_type_id as PayableBankAccountTypeId,
                    payable.account_contact_name as PayableAccountContactName,
                    payable.account_contact_email as PayableAccountContactEmail,
                    payable.account_bank_country_id as PayableAccountBankCountryID,
                    payable.account_number as PayableAccountNumber,
                    payable.account_routing_number as PayableAccountRoutingNumber,
                    payable.account_swift_code as PayableAccountSwiftCode,
                    payable.collateral_desc as PayableCollateralDesc,
                    payable.collateral_rating as PayableCollateralRating,
                    payable.incoterm_desc as PayableIncotermDesc,
                    payable.shipping_company as PayableShippingCompany,
                    payable.shipping_desc as PayableShippingDesc,
                    payable.shipping_starting_city as PayableShippingStartingCity,
                    payable.shipping_ending_city as PayableShippingEndingCity,
                    payable.shipping_starting_country_id as PayableShippingStartingCountry,
                    payable.shipping_ending_country_id as PayableShippingEndingCountry,
                    payable.clearing_agent_name as PayableClearingAgentName,
                    payable.importer_of_record as PayableImporterofRecord
                    FROM
                    project_business_assoc
                    RIGHT JOIN project ON project_business_assoc.project_id= project.id
                    LEFT JOIN business ON business.id = project_business_assoc.business_id
                    LEFT JOIN payable ON payable.project_id= project.id and payable.business_id=business.id
                    LEFT JOIN currency pc ON pc.id= payable.currency_id
                    LEFT JOIN currency bc ON bc.id= business.bank_account_currency_id
                    where project_business_assoc.relationship='supplier'and project_business_assoc.deleted_at is NULL and project.id=",projectId,"
                    ");
tbl.project_supplier_payable <- koubel_connect_Rmysql %>% dbGetQuery(userQuery)
return(tbl.project_supplier_payable)
}
tbl.legalFillers <- bind_rows(lapply(projectIds,get.legalFillers,koubel_connect_Rmysql))
drop_in_koubel_franscine("dw_SupplierProjectPayable")
createtable(koubel_franscine_connect_dplyr,tbl.legalFillers,"dw_SupplierProjectPayable")


getwd()
write.csv(tbl.legalFillers, file = "tbl.legalFillers.csv",row.names=FALSE, na="")
