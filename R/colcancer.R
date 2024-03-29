#' Colon Cancer Dataset
#' 
#' A dataset describing colon cancer patients. The data is based on real data from a hospital-based cancer registry but many
#' values are changed to ensure anonymity. Each row is a single case,
#' while the columns represent patients' health conditions and physical parameters.
#' 
#' @details 
#' The columns of the data set are:
#' \itemize{
#' \item followup. numeric. Follow-up time since surgery in days. The time the patient was observed.
#' \item logfollowup. numeric. The follow-up time, but logarithmic.
#' \item death. integer. Indicates whether the patient died. If death occured it is set to 1, otherwise 0.
#' \item sex. factor. Level: "f", "m". The sex of the patient. In this case "f" stands for female, and "m" represents male patients.
#' \item LNE. numeric. The number of examined lymph nodes.
#' \item LNR. numeric, ranges from 0 to 1. The number of cancerous lymph nodes divided by the total number (LNE).
#' \item pUICC. factor. Levels: "I", "II", "III", "IV". Pathological cancer stage. The UICC staging system was used.
#' \item CTX. factor. Levels: "0", "1". Chemotherapy (no / yes)
#' \item ASA.score. factor. Levels: "mild", "severe". An ASA score smaller than 3 is considered a mild general illness, 3 or greater is considered a severe general illness. The ASA scoring system of patients was originally proposed by the American Society of Anesthesiologists. 
#' \item R.status factor. Level: "0", "12". Residual tumor after surgery. 0 stands for no residual tumor. 12 stands either for microscopic (R1) or macroscopic residues (R2).
#' \item preexisting.cancer. integer. If there was a history of cancer before the colon cancer. Set to 1 if there has been a cancer in the past and to 0 if not.
#' \item age. numeric. The age of the patient in years.
#' }
#' 
#' 
#' @format A data.frame with 546 observations with colon cancer cases. The 12 columns describe different parameters of patients' conditions.
#' 
#' @usage data("colcancer")
#' 
#' @docType data
#' @keywords data
#' 
"colcancer"