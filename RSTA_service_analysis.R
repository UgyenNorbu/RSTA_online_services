library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(xlsx)
library(WriteXLS)

file_list <- list.files()

my_theme <- theme(axis.title = element_text(family = "Times", size = 12),
          axis.text = element_text(family = "Times", size = 10),
          plot.title = element_text(family = "Times", size = 14, face = "bold", hjust = 0.5))

# 1. DL CANCELLATION ------------------------------------------------------

DL_cancellaton <- readxl::read_xlsx("DLDetails_Cancellation_Jan-Dec2019.xlsx")

DL_cancellaton_grouped <-  DL_cancellaton %>% 
    group_by(region) %>%
    summarise(no_of_service= n())

DL_cancellaton_grouped %>% 
    ggplot(aes(x = reorder(region, -no_of_service), y = no_of_service, fill = region)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.6) +
    scale_fill_brewer(palette = "Pastel1") +
    labs(x = "Region",
         y = "No. of services provided", 
         title = "Cancellation of driving license (Jan-Dec, 2019)") +
    coord_flip() +
    theme_minimal() +
    my_theme +
    theme(legend.position = "none")
ggsave("1_DL_cancellation.jpg", width = 25, height = 15, units = "cm")

WriteXLS(DL_cancellaton_grouped, "1_DL_cancellation_grouped.xlsx")

# 2. DL ENDORSEMENT -------------------------------------------------------
DL_endorsement <- readxl::read_xlsx("DLDetails_Endorsement_Jan-Dec2019.xlsx")

DL_endorsement <-  DL_endorsement %>% 
    mutate(region_name = ifelse(is.na(region_name), "Thimphu", region_name))

DL_endorsement_grouped <- DL_endorsement %>% 
    group_by(region_name) %>% 
    summarise(no_of_service = n())

WriteXLS(DL_endorsement_grouped, "2_DL_endorsement_grouped.xlsx")

DL_endorsement_grouped %>% 
    ggplot(aes(reorder(region_name, -no_of_service), no_of_service, 
               fill = region_name)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
    coord_flip()+
    # scale_y_log10() +
    scale_fill_brewer(palette = "Pastel1") +
    labs(x = "Region",
         y = "No. of services provided",
         title = "Endorsement of driving license (Jan-Dec, 2019)") +
    theme_minimal() +
    my_theme +
    theme(legend.position = "none")
ggsave("2_DL_endorsement.jpg", width = 25, height = 15, units = "cm")

# 3. NEW DRIVING LICENSE --------------------------------------------------
new_DL <- readxl::read_xlsx("DLDetails_New_Jan-Dec2019.xlsx")

new_DL_grouped <- new_DL %>% 
    group_by(region) %>% 
    summarise(no_of_service_provided = n())

WriteXLS(new_DL_grouped, "3_new_DL_grouped.xlsx")

new_DL_grouped %>% 
    ggplot(aes(reorder(region, - no_of_service_provided), 
               no_of_service_provided, fill = region))+
    geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
    coord_flip() +
    ylim(0, 8000) +
    scale_fill_brewer(palette = "Pastel1") +
    labs(x = "Region", 
         y = "No. of services provided", 
         title = "Issuance of new driving license (Jan-Dec, 2019)") +
    theme_minimal() +
    my_theme +
    theme(legend.position = "none")

ggsave("3_Issuance_of_new_DL.jpg", width = 25, height = 15, units = "cm")

# 4. RENEWAL OF DL --------------------------------------------------------
renew_DL <- readxl::read_xlsx("DLDetails_Renewal_Jan-Dec2019.xlsx")

Tphu_staff_list <- c("dbgurung", "choida", "jamyangdorji", "tashipenjor", "TASHIPENJOR", "TashiPenjor", 
                     "mtlepcha", "karmaleki", "pemawangdi", "npem", "pthinley", "slhamo", "rinzInp", 
                     "pwangchuk", "rjangsem", "Kphuntsho", "kphuntsho", "schenzom", "rinzinp",
                     "ywangmo", "ngyeltshen", "szangmo", "stshering", "sangaylham")
Pling_staff_list <- c("karchung", "Npkarki", "nado", "unwangmo", "ugyenr", "ynorbu", "swangdi",
                      "ktshering", "Ktshering", "bsharma", "sdorji")
Gphu_staff_list <- c("kinleynamgay", "YANGZOM", "dkhandu", "rjamtsho", "thrungrabdorji", "njamtsho",
                     "ndhendup", "tchogyal", "Pkhandu", "pkhandu", "kwangmo", "jdorji",
                     "ddhaurali", "tashid", "ttobgay", "sangaylhamo", "karmak")
Sj_staff_list <- c("uwangchuk", "thinleytobgay", "tashidorji", "PSYONJEN", "psyonjen", "tgyeltshen",
                    "tjamtsho", "tnorbu", "keeong")
Mgr_staff_list <- c("Ysonam", "ysonam", "sonamdorji", "uwangdi", "stshomo", "nnorbu", "ddorji",
                    "tphuentsho", "kdorji","Kdorji", "pnorbu", "ldorji", "tpeldon",  "twangmo")

renew_DL <- renew_DL %>%
    mutate(payment_mode = ifelse(str_length(Receipt_No) == 12, "Online", "Cash")) %>% 
    mutate(region_name = ifelse(Created_By %in% Tphu_staff_list, "Thimphu", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% Pling_staff_list, "Phuentsholing", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% Gphu_staff_list, "Gelephu", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% Sj_staff_list, "Samdrupjongkhar", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% Mgr_staff_list, "Monggar", region_name)) %>% 
    mutate(region_name = ifelse(is.na(region_name), "Thimphu", region_name))

renew_DL_grouped <- renew_DL %>% 
    group_by(region_name, payment_mode) %>% 
    summarise(no_of_service = n())

renew_DL_grouped %>% 
    ggplot(aes(x = payment_mode, y = no_of_service, fill = payment_mode))+
    geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
    facet_grid(region_name ~.) +
    coord_flip() +
    labs(x = "Payment mode",
         y = "No. of services provided", 
         title = "Renewal of driving license (Jan-Dec, 2019)") +
    theme_bw() +
    theme(legend.position = "none") +
    my_theme
ggsave("4_renewal_of_DL.jpg", width = 25, height = 15, units = "cm")

WriteXLS(renew_DL_grouped, "4_renew_DL_grouped.xlsx")

# 5. DL REPLACEMENT -------------------------------------------------------

DL_replacement <- readxl::read_xlsx("DLDetails_Replacement_Jan-Dec2019.xlsx")

DL_replacement <-  DL_replacement %>%
    mutate(payment_mode = ifelse(str_length(Receipt_No)==12, "Online", "Cash"))

DL_replacement_grouped <-  DL_replacement %>% 
    group_by(region_name, payment_mode) %>% 
    summarise(no_of_services = n())

WriteXLS(DL_replacement_grouped, "5_DL_replacement_grouped.xlsx")

DL_replacement_grouped %>% 
    ggplot(aes(x = payment_mode, y = no_of_services, fill = payment_mode)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
    facet_grid(region_name~.) +
    coord_flip() +
    labs( x = "Payment mode",
          y = "No. of services",
          title = "Replacement of driving license (Jan-Dec, 2019)"
    ) +
    theme_bw() +
    my_theme +
    theme(legend.position = "none") 
ggsave("5_Replacemen_of_DL.jpg", width = 25, height = 15, units = "cm")

# 6. DL SUSPENSION --------------------------------------------------------
DL_suspension <- readxl::read_xlsx("DLDetails_Suspension_Jan-Dec2019.xlsx")

ACC_case <- DL_suspension %>% 
    filter(grepl("ACC", Reason_For_Suspension, ignore.case = FALSE)) %>% 
    select(Reason_For_Suspension) %>% 
    unique() %>% 
    pull()

Traffic_disp <- DL_suspension %>% 
    filter(grepl("TDCM", Reason_For_Suspension, ignore.case = TRUE)) %>% 
    select(Reason_For_Suspension) %>% 
    unique() %>% 
    pull()

drugs <- DL_suspension %>% 
    filter(grepl("drugs", Reason_For_Suspension, ignore.case = TRUE)) %>% 
    select(Reason_For_Suspension) %>% 
    unique() %>% 
    pull()

DL_suspension <- DL_suspension %>% 
    mutate(Reason_For_Suspension = ifelse(Reason_For_Suspension %in% ACC_case,
                                          "ACC case", Reason_For_Suspension)) %>% 
    mutate(Reason_For_Suspension = ifelse(Reason_For_Suspension %in% Traffic_disp,
                                          "TDCM decision", Reason_For_Suspension)) %>% 
    mutate(Reason_For_Suspension = ifelse(Reason_For_Suspension %in% drugs,
                                          "Driving under influence", 
                                          Reason_For_Suspension)) %>% 
    mutate(Reason_For_Suspension= ifelse(Reason_For_Suspension == 
                                             "contact Pling RSTA", "TDCM decision",
                                         Reason_For_Suspension)) %>% 
    mutate(Reason_For_Suspension= ifelse(Reason_For_Suspension == "Referred to BNCA for treatment,forwarded by RBP", "Driving under influence",
                                         Reason_For_Suspension)) %>% 
    mutate(Reason_For_Suspension= ifelse(Reason_For_Suspension == "tested positive for substance abuse", "Driving under influence",
                                         Reason_For_Suspension))

DL_suspension_grouped <- DL_suspension %>% 
    group_by(Reason_For_Suspension) %>% 
    summarise(no_of_cases = n())

DL_suspension_grouped %>% 
    ggplot(aes(x = reorder(Reason_For_Suspension, -no_of_cases), y = no_of_cases, 
               fill = Reason_For_Suspension))+
    geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
    labs(x = "Reason for suspension of driving license",
         y = "No. of cases",
         title = "Suspension of driving license (Jan-Dec, 2019)") +
    scale_fill_brewer(palette = "Pastel1") +
    theme_minimal() +
    theme(legend.position = "none") +
    my_theme 
ggsave("6_DL_suspension.jpg", width = 25, height = 15, units = "cm")

WriteXLS(DL_suspension_grouped, "6_DL_suspension_grouped.xlsx")

# 7. NEW LEARNERS LICENSE -----------------------------------------------------
new_LL <- readxl::read_xlsx("LLDetails_New_Jan-Dec2019.xlsx")

new_LL <- new_LL %>% 
    mutate(region_name = ifelse(Created_By == "pnorbu", "Monggar", region_name)) %>% 
    mutate(region_name = ifelse(Created_By == "ynorbu", "Phuentsholing", region_name))

new_LL_grouped <- new_LL %>% 
    group_by(region_name) %>% 
    summarise(number = n())

WriteXLS(new_LL_grouped, "7_new_LL_grouped.xlsx")

new_LL_grouped %>% 
    ggplot(aes(x = reorder(region_name, -number), y = number, fill = region_name))+
    geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
    labs(x = "Region",
         y = "Number",
         title = "Issuance of new learner license (Jan-Dec, 2019)") +
    scale_fill_brewer(palette = "Pastel1") +
    theme_minimal() +
    coord_flip() +
    theme(legend.position = "none") +
    my_theme 
ggsave("7_new_LL.jpg", width = 25, height = 15, units = "cm")

# 8. RENEWAL OF LL --------------------------------------------------------
renew_LL <- readxl::read_xlsx("LLDetails_Renewal_Jan-Dec2019.xlsx")

renew_LL <- renew_LL %>% 
    mutate(payment_mode = ifelse(str_length(Receipt_No) == 12,
                                 "Online", "Cash"))

renew_LL_grouped <- renew_LL %>% 
    group_by(region_name, payment_mode) %>% 
    summarise(number = n())

WriteXLS(renew_LL_grouped, "8_renew_LL_grouped.xlsx")

renew_LL_grouped %>% 
    ggplot(aes(x = reorder(payment_mode, -number), y = number, fill = payment_mode))+
    geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
    facet_grid(region_name~.) +
    labs(x = "Payment mode",
         y = "Number",
         title = "Renewal of learner license (Jan-Dec, 2019)") +
    theme_minimal() +
    coord_flip() +
    theme(legend.position = "none") +
    my_theme 
ggsave("8_renew_LL.jpg", width = 25, height = 15, units = "cm")

# 9. REPLACEMENT OF LL ----------------------------------------------------
repl_LL <- readxl::read_xlsx("LLDetails_Replacement_Jan-Dec2019.xlsx")

repl_LL <- repl_LL %>% 
    mutate(payment_mode = ifelse(str_length(Receipt_No) == 12,"Online", "Cash"))

repl_LL_grouped <- repl_LL %>% 
    group_by(region_name, payment_mode) %>% 
    summarise(number = n())

WriteXLS(repl_LL_grouped, "9_replacement_LL_grouped.xlsx")

repl_LL_grouped %>% 
    ggplot(aes(x = reorder(payment_mode, -number), y = number, fill = payment_mode))+
    geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
    facet_grid(region_name~.) +
    labs(x = "Payment mode",
         y = "Number",
         title = "Replacement of learner license (Jan-Dec, 2019)") +
    theme_minimal() +
    coord_flip() +
    theme(legend.position = "none") +
    my_theme 
ggsave("9_replacement_LL.jpg", width = 25, height = 15, units = "cm")

# 10. TRAFFIC RULE VIOLATIONS ---------------------------------------------
TIN_details <- readxl::read_xlsx("raw_data/OffenceDetails_Jan-Dec2019.xlsx")

TIN_details <- TIN_details %>% 
    mutate(payment_mode = ifelse(str_length(Receipt_Number) == 12,"Online", "Cash")) %>% 
    mutate(region_name = ifelse(is.na(region_name), "Thimphu", region_name)) %>% 
    mutate(payment_mode = ifelse(is.na(Receipt_Number), "Not paid", payment_mode))
 
TIN_grouped <- TIN_details %>% 
    group_by(region_name, payment_mode) %>% 
    summarise(number = n())

WriteXLS(TIN_grouped, "agg_data/10_payment_of_fine.xlsx")

TIN_grouped %>% 
    ggplot(aes(x = payment_mode, y = number, fill = payment_mode))+
    geom_bar(stat = "identity", width = 0.5, alpha = 0.65) +
    facet_grid(region_name~.) +
    labs(x = "Payment mode",
         y = "Number",
         title = "Payment of fines & penalties (Jan-Dec, 2019)") +
    scale_fill_manual(values =c("#87CEFA", "#CFCFCF", "#6CA6CD")) +
    theme_minimal() +
    coord_flip() +
    theme(legend.position = "none") +
    my_theme
ggsave("image_output/10_payment_of_fines.jpg", width = 25, height = 15, units = "cm")

# 11. RC CANCELLATION -----------------------------------------------------
RC_cancellation <- readxl::read_xlsx("VehicleDetails_Cancellation_Jan-Dec2019.xlsx")

name_list_created <-  RC_cancellation %>% 
    filter(is.na(region_name)) %>% 
    select(Created_By) %>% 
    unique() %>% 
    pull()

tphu <- c("SYSTEM", "karmaleki", "npem", "szangmo", "utshomo", "dawapenjor", "UTSHOMO",
          "tphuentsho", "rinzinp", "NPEM", "ywangmo", "pthinley", "stshering", "slhamo", "kphuntsho")
pling <- c("stobgay", "nzangmo", "bsharma", "uwangdi", "unwangmo", "Npkarki", "swangdi"  )
gphu <- c("dthapa", "rtgurung", "jdorji", "njamtsho", "PSYONJEN", "psyonjen", "ddhaurali", "dkhandu")
sj <- c("tashidorji", "pdorji", "uwangchuk")
mgr <- c("sdorji", "ldorji" )
          
RC_cancellation <- RC_cancellation %>%
    mutate(region_name = ifelse(Created_By %in% tphu, "Thimphu", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% pling, "Phuentsholing", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% gphu, "Gelephu", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% sj, "Samdrupjongkhar", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% mgr, "Monggar", region_name))
    

RC_cancellation_grouped <-  RC_cancellation %>% 
    group_by(region_name) %>% 
    summarise(number = n())

WriteXLS(RC_cancellation_grouped, "11_RC_cancellation.xlsx")

RC_cancellation_grouped %>% 
    ggplot(aes(x = reorder(region_name, - number), y = number, fill = region_name))+
    geom_bar(stat = "identity", width = 0.5) +
    ylim(0, 800) +
    labs(x = "Region",
         y = "Number",
         title = "Cancellation of registration certificate (Jan-Dec, 2019)") +
    scale_fill_brewer(palette = "Pastel1") +
    theme_minimal() +
    coord_flip() +
    theme(legend.position = "none") +
    my_theme 
ggsave("11_RC_cancellation.jpg", width = 25, height = 15, units = "cm")


# 12. RC CONVERSION -------------------------------------------------------
RC_conversion <- readxl::read_xlsx("VehicleDetails_Conversion_Jan-Dec2019.xlsx")

RC_conversion_grouped <- RC_conversion %>% 
    group_by(region) %>% 
    summarise(number = n())

WriteXLS(RC_conversion_grouped, "12_RC_conversion.xlsx")

RC_conversion_grouped %>% 
    ggplot(aes(x = reorder(region, -number), y = number, fill = region)) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_brewer(palette = "Pastel1") +
    ylim(0, 200) +
    labs(x = "Region",
         y = "Number",
         title = "Vehicle ownership conversion"
    ) +
    theme_minimal() +
    coord_flip() +
    theme(legend.position = "none") +
    my_theme 
ggsave("12_RC_conversion.jpg", width = 25, height = 15, units = "cm")           

# 13. RWC -----------------------------------------------------------------

rwc_data <- readxl::read_xlsx("VehicleDetails_FitnessTest_Jan-Dec2019.xlsx")

rwc_data <- rwc_data %>% 
    mutate(payment_mode = ifelse(str_length(Receipt_Number) == 12, "Online", "Cash"))

rwc_data_grouped <- rwc_data %>% 
    group_by(region_name, payment_mode) %>% 
    summarise(number = n())

WriteXLS::WriteXLS(rwc_data_grouped, "rwc_data_grouped.xlsx")

rwc_data_grouped %>% 
    ggplot(aes(x = reorder(payment_mode, -number), y = number, fill = payment_mode)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
    facet_grid(region_name ~.) +
    labs(x = "Payment mode",
         y = "Number",
         title = "Payment of Road Worthiness Certification fees (Jan-Dec, 2019)") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    my_theme

ggsave("13_RWC_data.jpg", width = 25, height = 15, units = "cm")

# 14. NEW VEHICLE REGISTRATION --------------------------------------------
path_new_rc_1 <- "VehicleDetails_New_Org_Jan-Dec2019.xlsx"

path_new_rc_2 <- "VehicleDetails_New_PVT_Jan-Dec2019.xlsx"

rc_1 <- readxl::read_xlsx(path_new_rc_1)               
rc_2 <- readxl::read_xlsx(path_new_rc_2)

colnames(rc_1) == colnames(rc_2)

rc_1 <- rc_1 %>% 
    mutate(owner_type = "Org.") %>% 
    select(owner_type, region_name)

rc_2 <- rc_2 %>% 
    mutate(owner_type = "Pvt.") %>% 
    select(owner_type, region_name)

new_RC_combined <- rbind(rc_1, rc_2)

new_RC_combined_grouped <- new_RC_combined %>% 
    group_by(region_name, owner_type) %>% 
    summarise(number = n())

WriteXLS(new_RC_combined_grouped, "14_new_RC.xlsx")

new_RC_combined_grouped %>% 
    ggplot(aes(x =reorder(owner_type, -number), y = number, fill = owner_type)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
    facet_grid(region_name~.) +
    labs(x = "Owner type",
         y = "Number",
         title = "New vehicel registration (Jan-Dec, 2019)"
    ) +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    my_theme
ggsave("14_new_vehicle_registration.jpg", width = 25, height = 15, units = "cm")

# 15. NOC -----------------------------------------------------------------

noc_data <- readxl::read_xlsx("VehicleDetails_NOC_Jan-Dec2019.xlsx")

noc_data_grouped <- noc_data %>% 
    group_by(region_name) %>% 
    summarise(number = n())

WriteXLS(noc_data_grouped, "15_NOC_grouped.xlsx")

noc_data_grouped %>% 
    ggplot(aes(x = reorder(region_name, -number), y = number, fill = region_name)) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_brewer(palette = "Pastel1") +
    labs(x = "Region name", 
        y = "Number",
        title = "No Objection Certificate for sale of vehicles (Jan-Dec, 2019)") +
    ylim(0, 100) +
    theme_minimal() +
    theme(legend.position = "none") +
    my_theme
ggsave("15_NOC.jpg", width = 25, height = 15, units = "cm")

# 16. VEHICLE OWNERSHIP TRANSFER ------------------------------------------
RC_transfer <- readxl::read_xlsx("VehicleDetails_OSTransfer_Jan-Dec2019.xlsx")

RC_transfer_grouped <- RC_transfer %>% 
    group_by(region) %>% 
    summarise(number = n())

RC_transfer_grouped %>% 
    ggplot(aes( reorder(region, -number), y = number, fill = region)) +
    geom_bar(stat = "identity", width = 0.5) +
    scale_fill_brewer(palette = "Pastel1") +
    labs(x = "Region name",
         y = "Number",
         title = "Vehicle ownership transfer") +
    theme_minimal() +
    theme(legend.position = "none") +
    my_theme
ggsave("16_RC_ownership_transfer.jpg", width = 25, height = 15, units = "cm")

# 17. RC RENEWAL ---------------------------------------------------------

RC_renew_org <- readxl::read_xlsx("VehicleDetails_Renewal_Org_Jan-Dec2019.xlsx")

RC_renew_pvt <- readxl::read_xlsx("VehicleDetails_Renewal_PVT_Jan-Dec2019.xlsx")

tphu <- c("dechenwangmo", "karmaleki", "ywangmo")
pling <- c("stobgay", "nzangmo", "kjamtsho")
gphu <- c("rtgurung", "ddhaurali", "njamtsho", "dkhandu", "sangaylhamo")
sj <- c("pdorji", "uwangchuk", "psyonjen")
mgr <- c("sonamdorji", "nbaraily", "ddorji") 
others <- c("167", "99")

RC_renew_org <- RC_renew_org %>% 
    mutate(region_name = ifelse(Created_By %in% tphu, "Thimphu", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% pling, "Phuntsholing", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% gphu, "Gelephu", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% sj, "Samdrupjongkhar", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% mgr, "Mongar", region_name))

RC_renew_pvt <- RC_renew_pvt %>% 
    mutate(region_name = ifelse(Created_By %in% tphu, "Thimphu", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% pling, "Phuntsholing", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% gphu, "Gelephu", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% sj, "Samdrupjongkhar", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% mgr, "Mongar", region_name)) %>% 
    mutate(region_name = ifelse(Created_By %in% others, "Thimphu", region_name))

RC_renew_pvt %>% 
    filter(is.na(region_name)) %>% 
    select(Created_By) %>%
    unique() %>% 
    pull()

RC_renew_org <- RC_renew_org %>% 
    mutate(owner_type = "Org.") %>% 
    mutate(payment_mode = ifelse(str_length(Receipt_No) == 12, "Online", "Cash")) %>% 
    select(region_name, Receipt_No, owner_type, payment_mode)

RC_renew_pvt <- RC_renew_pvt %>% 
    mutate(owner_type = "Pvt.") %>% 
    mutate(payment_mode = ifelse(str_length(Receipt_No) == 12, "Online", "Cash")) %>% 
    select(region_name, Receipt_No, owner_type, payment_mode)

RC_renew_total <- rbind(RC_renew_org, RC_renew_pvt)

RC_renew_total_grouped <- RC_renew_total %>% 
    group_by(region_name, owner_type, payment_mode) %>% 
    summarise(number = n())

WriteXLS(RC_renew_total_grouped, "17_RC_renewal_grouped.xlsx")

RC_renew_total_grouped %>% 
    ggplot(aes(x = payment_mode, y = number, fill = owner_type)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
    facet_grid(region_name ~.) +
    labs(x = "Payment mode",
         y = "Number",
         title = "Renewal of vehicle registration certificate (Jan-Dec, 2019)") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "top") +
    my_theme

ggsave("image_output/17_RC_renewal.jpg", width = 25, height = 15, units = "cm")

# 18. RC REPLACEMENT ------------------------------------------------------
RC_rep <- readxl::read_xlsx("VehicleDetails_Replacement_Jan-Dec2019.xlsx")

RC_rep <- RC_rep %>% 
    mutate(payment_mode = ifelse(str_length(Receipt_No) == 12, "Online", "Cash"))

RC_rep_grouped <- RC_rep %>% 
    group_by(region_name, payment_mode) %>% 
    summarise(number = n())

WriteXLS(RC_rep_grouped, "18_RC_rep_grouped.xlsx")

RC_rep_grouped %>% 
    ggplot(aes(x = payment_mode, y = number, fill = payment_mode)) +
    geom_bar(stat = "identity", width = 0.5, alpha = 0.7) +
    facet_grid(region_name~.) +
    labs(x = "Payment mode", 
         y = "Number", 
         title = "Vehicle registration certificate replacement (Jan-Dec, 2019)") +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") +
    my_theme 

ggsave("18_RC_replacement.jpg", width = 25, height = 15, units = "cm")

# 19. ROUTE PERMITS -------------------------------------------------------

route_permit <- readxl::read_xlsx("VehicleDetails_RoutePermit_Jan-Dec2019.xlsx")

pling_1 <- route_permit %>% 
    filter(grepl("Phue", Route_From, ignore.case = TRUE)) %>% 
    select(Route_From) %>% 
    unique() %>% 
    pull()
pling_2 <-  route_permit %>% 
    filter(grepl("Phun", Route_From, ignore.case = TRUE)) %>% 
    select(Route_From) %>% 
    unique() %>% 
    pull()
pling_3 <-  route_permit %>% 
    filter(grepl("Plin", Route_From, ignore.case = TRUE)) %>% 
    select(Route_From) %>% 
    unique() %>% 
    pull()
pling_4 <-  route_permit %>% 
    filter(grepl("P/l", Route_From, ignore.case = TRUE)) %>% 
    select(Route_From) %>% 
    unique() %>% 
    pull()

all_pling <- c(pling_1, pling_2, pling_3, pling_4)

alt_tphu <- route_permit %>% 
    filter(grepl("Thim", Route_From, ignore.case = TRUE)) %>% 
    select(Route_From) %>% 
    unique() %>% 
    pull()

alt_sj <- c("Samrupjongkhar", "Samdrupjongkhar", "samdrupjongkhar", "Samdrup Jongkhar")

alt_Samtse <- route_permit %>% 
    filter(grepl("Samtse", Route_From, ignore.case = TRUE)) %>% 
    select(Route_From) %>% 
    unique() %>% 
    pull()

alt_mgr <- route_permit %>% 
    filter(grepl("Mong", Route_From, ignore.case = TRUE)) %>% 
    select(Route_From) %>% 
    unique() %>% 
    pull()

alt_punakha <- route_permit %>% 
    filter(grepl("Gammon", Route_From, ignore.case = TRUE)) %>% 
    select(Route_From) %>% 
    unique() %>% 
    pull()

alt_bum <- route_permit %>% 
    filter(grepl("Bumt", Route_From, ignore.case = TRUE)) %>% 
    select(Route_From) %>% 
    unique() %>% 
    pull()

alt_lhu <- route_permit %>% 
    filter(grepl("Auts", Route_From, ignore.case = TRUE)) %>% 
    select(Route_From) %>% 
    unique() %>% 
    pull()

name_list <- route_permit_grouped %>%
    filter(number < 5) %>%
    select(Route_From) %>% 
    pull()

route_permit <- route_permit %>% 
    mutate(Route_From = ifelse(Route_From %in% all_pling, 
                               "Phuntsholing", Route_From)) %>% 
    mutate(Route_From = ifelse(Route_From %in% alt_tphu, "Thimphu", Route_From)) %>%
    mutate(Route_From = ifelse(Route_From %in% alt_sj, "Sjongkhar", Route_From)) %>%
    mutate(Route_From = ifelse(Route_From %in% alt_Samtse, "Samtse", Route_From)) %>% 
    mutate(Route_From = ifelse(Route_From %in% alt_mgr, "Mongar", Route_From)) %>% 
    mutate(Route_From = ifelse(Route_From %in% alt_bum, "Bumthang", Route_From)) %>% 
    mutate(Route_From = ifelse(Route_From %in% alt_lhu, "Lhuntse", Route_From)) %>% 
    mutate(Route_From = ifelse(Route_From %in% alt_punakha, "Punakha", Route_From)) %>% 
    mutate(Route_From = ifelse(Route_From %in% name_list, "Others", Route_From)) %>% 
    mutate(Route_From = ifelse(Route_From == "Lhuentse", "Lhuntse", Route_From)) %>% 
    mutate(Route_From = ifelse(Route_From == "Bajo", "Wangdue", Route_From)) %>% 
    mutate(Route_From = ifelse(Route_From == "Samthang", "Wangdue", Route_From)) %>% 
    mutate(Route_From = ifelse(Route_From == "Sipsu", "Samtse", Route_From)) %>% 
    mutate(Route_From = ifelse(Route_From == "Buli", "Zhemgang", Route_From)) %>% 
    mutate(Route_From = ifelse(Route_From == "Chemurchi khola", "Samtse", Route_From)) %>% 
    mutate(Route_From = ifelse(Route_From == "PugliKhola", "Samtse", Route_From)) %>% 
    mutate(Route_From = ifelse(Route_From == "Kanglung", "Trashigang", Route_From))

route_permit_grouped <- route_permit %>% 
    group_by(Route_From) %>% 
    summarise(number = n()) %>% 
    arrange(desc(number))

write_csv(route_permit_grouped, "route_permit_grouped.csv")

route_permit_grouped %>% 
    ggplot(aes(x = reorder(Route_From, -number), y = number)) +
    geom_bar(stat = "identity") +
    labs(x = "Dzongkhag",
         y = "Number of vehicles",
         title = "Issuance of route permit") +
    scale_y_log10() +
    theme_minimal() +
    my_theme +
    coord_flip()
ggsave("19_route_permit.jpg", width = 25, height = 15, units = "cm")
