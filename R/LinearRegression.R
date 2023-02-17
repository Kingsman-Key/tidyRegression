#' A function to summary linear model regression set table
#'
#' This function allows you to generate linear regression table
#' @param model linear model passed to the function
#' @param pType Linear regression table type, defaults to "withPValue"
#' @param estimateType Linear regression table type, defaults to "betase"
#' @param pType whether put P into final table, default to withPValue, another selection is withMark
#' @param digits Digit number of the estimate
#' @param pDigits Digit number of the P value
#' @param ... other parameters to pass into tidy function from broom
#' @keywords Model
#' @return regression table with reference level added
#' @export
#' @examples
#' model <- lm(formula = Sepal.Width ~ Petal.Length, data = iris)
#' clean.lm(model)
clean.lm <- function(model, pType = "withPValue", estimateType = "betase", digits = 2, pDigits = 4, ...){
  ####
  # parameter check part  ----------------------------------------------------------------------------------
  ####

  ModelIsNull <- is.na(model)|is.null(model)
  pTypeIsNull <- is.na(pType)|is.null(pType)
  estimateTypeIsNull <- is.na(estimateType)|is.null(estimateType)
  digitsIsNull <- is.na(digits)|is.null(digits)
  pDigitsIsNull <- is.na(pDigits)|is.null(pDigits)
  if(any(ModelIsNull, pTypeIsNull, estimateTypeIsNull, digitsIsNull, pDigitsIsNull)){
    stop()
    print("Error, one of the paramater is empty and is without default value. Please check the parameters")
  }
  pTypeAcceptable <- pType %in% c("withPValue", "withMark")
  if(!pTypeAcceptable){
    pType <- "withPValue"
    print("WARNING, no P Type selected, set default to withPValue")
  }
  estimateTypeAcceptable  <- estimateType  %in% c("betase", "betaci")
  if(!estimateTypeAcceptable){
    estimateType <- "betase"
    print("WARNING, no estimate Type selected, set default to betase")
  }
  estimate <- std.error <- conf.low <- conf.high <- p.value <- NULL # This line is to pass note check in R package buliding


  ## generate possible result
  regressionTableBeforeClean <- broom::tidy(x = model, conf.int = T, ...)
  regressionTableAfterClean <- regressionTableBeforeClean %>%
    dplyr::mutate(
      betase = paste0(
        sprintf(paste0("%.", digits, "f"), estimate),
        " (",
        sprintf(paste0("%.", digits, "f"), std.error),
        ")"
      ),
      betaci = paste0(
        sprintf(paste0("%.", digits, "f"), estimate),
        " (",
        sprintf(paste0("%.", digits, "f"), conf.low),
        ", ",
        sprintf(paste0("%.", digits, "f"), conf.high),
        ")"
      ),
      pValue = sprintf(paste0("%.", pDigits, "f"), p.value),
      betaseWithMark = dplyr::case_when(
        pValue < 0.05 & pValue >= 0.01 ~ paste0(betase, "*"),
        pValue < 0.01 & pValue >= 0.001 ~ paste0(betase, "**"),
        pValue < 0.001 ~ paste0(betase, "***"),
        TRUE ~ NA_character_
      ),
      betaciWithMark = dplyr::case_when(
        pValue < 0.05 & pValue >= 0.01 ~ paste0(betase, "*"),
        pValue < 0.01 & pValue >= 0.001 ~ paste0(betase, "**"),
        pValue < 0.001 ~ paste0(betase, "***"),
        TRUE ~ NA_character_
      )
    )

  ## output selection
  if(estimateType == "betase"){
    tablePart1 = regressionTableAfterClean[,"betase"]
    return(tablePart1)
  }else if(estimateType == "betaci"){
    tablePart1 = regressionTableAfterClean[,"betaci"]
    return(tablePart1)
  }

  if(pType == "withPValue" & estimateType == "betase"){
    tablePart2 = regressionTableAfterClean[,"pvalue"]
    tableFinal <- cbind(tablePart1, tablePart2)
    names(tableFinal) <- c("beta (se)", "P")
  }else if(pType == "withPValue" & estimateType == "betaci"){
    tablePart2 = regressionTableAfterClean[,"pvalue"]
    tableFinal <- cbind(tablePart1, tablePart2)
    names(tableFinal) <- c("beta (95%CI)", "P")
  }else if(pType == "withMark" & estimateType == "betase"){
    tablePart1 <- regressionTableAfterClean[,"betaseWithMark"]
    tableFinal <- tablePart1
    names(tableFinal) <- c("beta (se)")
  }else if(pType == "withMark" & estimateType == "betaci"){
    tablePart1 <- regressionTableAfterClean[,"betaciWithMark"]
    tableFinal <- tablePart1
    names(tableFinal) <- c("beta (95%CI)")
  }
  return(tableFinal)

}


tidy.linear <-










