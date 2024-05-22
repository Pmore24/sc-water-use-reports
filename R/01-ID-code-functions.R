#' @title REGEX (Regular Expression)
#' @description Defines the structure of a character vector, here a sourceid or
# a userid
#' @export
userIDregex <- "^[0-4]\\d(OT|IN|GC|PN|PT|IR|AQ|WS|PH|MI)\\d\\d\\d$"
sourceIDregex <- "^[0-4]\\d(OT|IN|GC|PN|PT|IR|AQ|WS|PH|MI)\\d\\d\\d(G|S|P|E)\\d\\d$"

#' @title Validating sourceid
#' @description Checks if there are 10 characters in sourceid and their validity
#  i.e if the input sourceid is structured as it is defined by the
# 'sourceIDregex'.
#' @importFrom stringr str_detect
#' @param x Character vector of SourceIDs
#' @return A logical vector (TRUE or FALSE)
#' @export
sourceid_val <- function(x){
  dplyr::if_else(
    condition = !is.na(x),
    false = FALSE,
    missing = FALSE,
    true = stringr::str_detect(x, sourceIDregex) &
      nchar(x) == 10 &
      !(stringr::str_sub(x, start=1, end=2) %in% c("47","48","49","00")))
  }

#' @title Validating userid
#' @description Checks if there are 7 characters in a userid and their validity
#  i.e if the input userid is structured as it is defined by the
# 'userIDregex'.
#' @importFrom stringr str_detect
#' @param x Character vector of UserIDs
#' @return A logical vector (TRUE or FALSE)
#' @export
userid_val <- function(x){
  stringr::str_detect(x, userIDregex) & nchar(x) == 7 & !{
    stringr::str_sub(x, start=1, end=2) %in% c("47","48","49","00")
  }
}

#' @title Parse userids
#' @description Decode userids and add columns with the coded information: CategoryCode, Category, and UserCounty.
#' @export
#' @param x a table with a userid column
#' @param userid_column the name of the userid column (as a character string)
#' @return the table with added columns derived from the userids.
parse_userid <- function(x, userid_column='userid') {
  userids <- x[,userid_column, drop=T]
  x[,"CategoryCode"] <- substring(userids, 3,4)
  x[,"Category"] <- dplyr::recode(
    x$CategoryCode, GC="Golf", MI="Mining",
    IN="Industry", WS="Water Supply", IR="Agriculture",
    PT="Thermal Power", PN="Nuclear Power", PH="Hydro Power",
    OT="Other", AQ="Aquaculture")
  x[,"UserCounty"] <- dplyr::recode(
    substring(userids, 1,2),
    `01`= "Abbeville", `02`="Aiken",`03`='Allendale',
    `04`= 'Anderson',`05`='Bamberg',`06`= 'Barnwell',`07`= 'Beaufort',
    `08`='Berkeley',`09`='Calhoun',`10`='Charleston',`11`='Cherokee',
    `12`='Chester',`13`='Chesterfield',`14`='Clarendon',`15`='Colleton',
    `16`='Darlington',`17`='Dillon',`18`='Dorchester',`19`='Edgefield',
    `20`='Fairfield',`21`='Florence',`22`='Georgetown',`23`='Greenville',
    `24`='Greenwood',`25`='Hampton',`26`='Horry',`27`='Jasper',`28`='Kershaw',
    `29`='Lancaster',`30`='Laurens',`31`='Lee',`32`='Lexington',`33`='Marion',
    `34`='Marlboro',`35`='McCormick',`36`='Newberry',`37`='Oconee',`38`='Orangeburg',
    `39`='Pickens',`40`='Richland',`41`='Saluda',`42`='Spartanburg',`43`='Sumter',
    `44`='Union',`45`='Williamsburg',`46`='York')
  return(x)
}

#' @title Parse sourceids
#' @description Decode sourceids and add columns with the coded information:
#' userid, SourceTypeCode, SourceType, CategoryCode, Category, and UserCounty.
#' @export
#' @param x a table with a sourceid column
#' @param sourceid_column the name of the sourceid column (as a character string)
#' @return the table with added columns derived from the sourceids.
#' @export
parse_sourceid <- function(x, sourceid_column='sourceid') {
  sourceids <- x[,sourceid_column, drop=T]
  dplyr::mutate(x,
                userid=stringr::str_sub(sourceids, 1, 7),
                SourceTypeCode=stringr::str_sub(sourceids,8,8),
                SourceType=dplyr::recode(SourceTypeCode,
                                   G="Ground Withdrawal",
                                   S="Surface Withdrawal",
                                   E="Increased Evaporation",
                                   P="Purchase")) |>
    parse_userid()
}
