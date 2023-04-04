library("readxl")
library("dplyr")
library("stringr")
library("ggplot2")
library("tidyr")

wwtp <- read_excel("2020-2021 Combined WWTP Data.xlsx", sheet=2)

#how many wwtp in each council?
wwtp.count <- wwtp %>% count(`Managing organisation`)
wwtp.count.sort <- arrange(wwtp.count, n)

#lowest
print(slice(wwtp.count.sort, 1))

#highest
print(slice_tail(wwtp.count.sort, n=2))

# number 
uniq <- unique(wwtp.count$n)
uniq[which.max(tabulate(match(wwtp.count$n, uniq)))]

#wellington
wwtp.count %>% filter(`Managing organisation` == "%ellington%")

#wwtp serve people
wwtp.pop <- wwtp %>% 
  group_by(`Managing organisation`) %>% 
  summarise(total_population = sum(Population, na.rm=TRUE))

#serve the least people
print(wwtp.pop %>% 
        arrange(total_population), n=63)

#serve the most people
print(wwtp.pop %>% 
        arrange(desc(total_population)))


#treatment level
print(wwtp_treatment_levels <- wwtp %>% 
        count(`Level of treatment`))

#effluent status
print(unique(wwtp$`Effluent consent status`))
print(table(wwtp$`Effluent consent status`))

#clean consent status
wwtp$`Effluent consent status` <- str_replace_all(wwtp$`Effluent consent status`, 
                                                  c("Current" = "Current", 
                                                    "Currrent" = "Current",
                                                    "current" = "Current",
                                                    "lodged" = "Lodged"))

wwtp$`Effluent consent status` <- factor(wwtp$`Effluent consent status`,
                                         levels=c("Current", "Lodged", "Complying", "Expired", "Significant Non-Compliance-NFU (RM16-0206-DC.02+)"))

effluent.count <- wwtp %>% count(`Effluent consent status`)
summary(effluent.count)
ggplot(effluent.count, aes(x = `Effluent consent status`, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Effluent Consent Status Across WWTPs", x = "Effluent Consent Status", y = "Count")
ggsave("effluentbar.jpg", width = 12, height = 8, dpi = 300)


wwtp.subset <- wwtp[, c("Managing organisation", "Effluent consent expiry date", "Effluent consent status")]
wwtp.subset$`Effluent consent expiry date` <- as.Date(wwtp_subset$`Effluent consent expiry date`, format = "%d/%m/%Y")

summary(wwtp.subset)

ggplot(na.omit(wwtp.subset), aes(x =`Effluent consent expiry date`, y = `Effluent consent status`, color =`Managing organisation`)) + 
  geom_point(size = 3) + 
  theme_bw() +
  scale_x_date(date_labels="%Y",date_breaks  ="2 year") +
  labs(x = "Effluent consent expiry date", y = "Effluent consent status", color = "Managing organisation")
ggsave("effluentscatter.jpg", width = 12, height = 8, dpi = 300)
