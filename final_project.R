
sink(file = "final_project.txt", append = FALSE, type = c("output"), split = FALSE)
name <- Sys.info()
name[7]

  ## Libraries and initialize

#rm(list = ls())
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate) 
library(purrr)
library(stringr)

dir <- setwd(getwd())




## Read in data
deathtable_expansion <- read_xlsx("deathtable_expansion.xlsx")
deathtable_expansion <- deathtable_expansion %>% mutate(Population = as.numeric(Population))

combined_death <- deathtable_expansion %>% group_by(State, Year) %>% mutate(all_deaths = sum(Deaths), all_pop = sum(Population, na.rm=TRUE), rate = all_deaths/all_pop) %>% distinct(State, Year, .keep_all = TRUE) %>% select(State, Year, all_deaths, all_pop, rate)


unemployment <- read_xlsx("Unemployment (1).xlsx")
unemployment<- unemployment[-c(1,2,3),]
unemployment<- unemployment %>% row_to_names(row_number = 1)
unemployment <- unemployment %>% select(State, Area_name, Civilian_labor_force_2010:Med_HH_Income_Percent_of_State_Total_2019)
unemployment <- unemployment %>% filter(!grepl(",", Area_name)) ## remove counties
unemployment <- unemployment[-10,] ##remove DC (its duplicated)
unemployment1 <- unemployment %>% pivot_longer(cols = c(Civilian_labor_force_2010:Med_HH_Income_Percent_of_State_Total_2019), names_to = "unemploy_variable") 
unemployment1 <- unemployment1 %>% mutate(Year = str_extract(unemploy_variable, "[^_]+$")) %>% mutate(Year = as.numeric(Year)) ## get year from each variable
unemployment1 <- unemployment1 %>% mutate(unemploy_variable = gsub('.{5}$', '', unemploy_variable)) ## remove year from variable name
unemployment1 <-unemployment1 %>% filter(!State == "US")
unemployment1 <- unemployment1 %>% dplyr::rename(State = Area_name, state_abrev = State)
unemployment2 <- unemployment1 %>% pivot_wider(id_cols = c(State:Year), names_from = unemploy_variable, values_from = value)
unemployment2 <- unemployment2 %>% mutate(State = as.factor(State)) %>%mutate_if(is.character,as.numeric)

workforce <- read_excel("physician_total (1).xlsx", sheet = "Sheet4")
workforce <- workforce %>% select(STATE:EMP_2019)
workforce <- workforce %>% dplyr::rename(State = STATE)
workforce <- workforce %>% pivot_longer(cols = c(EMP_2010:EMP_2019), names_to = "physician_variable")
workforce  <- workforce  %>% mutate(Year = str_extract(physician_variable, "[^_]+$")) %>% mutate(Year = as.numeric(Year))
workforce<- workforce %>% mutate(physician_variable = gsub('.{5}$', '', physician_variable)) ## remove year from variable name
workforce <- workforce %>% pivot_wider(names_from = physician_variable, values_from = value)
workforce <- workforce %>% dplyr::rename(physician_total = EMP)

injection <- read_excel("injection.xlsx",sheet = "Sheet2")
injection <- injection %>% select(STATE:LEGAL_2019)
injection <- injection %>% dplyr::rename(State = STATE)
injection <- injection %>% pivot_longer(cols = c(LEGAL_2010:LEGAL_2019), names_to = "variable")
injection  <- injection  %>% mutate(Year = str_extract(variable, "[^_]+$")) %>% mutate(Year = as.numeric(Year))
injection<- injection %>% mutate(variable = gsub('.{5}$', '', variable)) ## remove year from variable name
injection <- injection %>% pivot_wider(names_from = variable, values_from = value)
injection <- injection %>% dplyr::rename(legal_injection = LEGAL)

expansion_date <- read_excel("ExpansionData.xlsx")
expansion_date <- expansion_date %>% dplyr::rename(date = `Expansion date`)
expansion_date[ expansion_date == "NA" ] <- NA
expansion_date <- expansion_date %>% mutate(date = as.numeric(date))
expansion_date <- expansion_date %>% mutate_at(vars(date), ~as.Date(., origin = "1899-12-30"))

age_data_0 <- read.csv("Age data.csv")
age_data1 <- age_data_0[-c(1:4)]
age_data1 <- age_data1[-c(4)]
colnames(age_data1) <- c("name", "sex", "age", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019")
age_data1 <- age_data1[!(age_data1$age ==999),] 
age_data1["age"][age_data1["age"] == 0] <- 1.00000000000001 ## changes age 0 to 1 so that we can get the total people

age_data1 <- age_data1 %>% mutate(across(contains('20'), list(f = ~(.*age)), 
                                         .names = "{col}_{fn}")) 

age_data2 <- age_data1 %>% filter(name != "United States") 

age_data3 <- age_data2 %>% group_by(name) %>% mutate(across(contains('20'), 
                                                            .fns = list(s = ~(sum(.))), 
                                                            .names = "{col}_{fn}")) 
age_data4 <- age_data3 %>% distinct(name, .keep_all=TRUE) %>% select(-c(sex, age)) %>% select(name, c(`2010_f`:`2019_f_s`))

patterns <- unique(substr(names(age_data4), 1, 4))  # store patterns in a vector
patterns <- patterns[-1]
new <- data.frame(sapply(patterns, function(x) age_data4[,grep(x, names(age_data4))][3] / age_data4[,grep(x, names(age_data4))][2]))  # loop through
final <- new %>% cbind(age_data4$name)
colnames(final) <- c("2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "State")
final <- final %>% select(State, c(`2010`:`2019`))
final <- final %>% pivot_longer(cols = c(`2010`:`2019`), names_to = "Year")
final <- final %>% dplyr::rename("Mean_Age" = value)
final <- final %>% mutate(Year = as.numeric(Year))

spending <- read_xlsx("Annual_bydrug_2019Q3.xlsx", sheet = "Annual_bydrug_2019Q3")
state_names <- read_xlsx("Annual_bydrug_2019Q3.xlsx", sheet = "Sheet1")
spending <- spending %>% full_join(state_names)

spending <- spending %>% filter(drugtype == "all") %>% rename(Year = year) %>% select(State, Year, imprx:percap_unadjmedamt)

other_deaths <- read_xlsx("Underlying Cause of Death, 1999-2019.xlsx", sheet = "All Cause of Death")
other_deaths <- other_deaths %>% pivot_longer(cols = c(Deaths2010:Deaths2019), names_to = "variable")
other_deaths  <-other_deaths  %>% mutate(Year = str_extract(variable, "[^s]+$")) %>% mutate(Year = as.numeric(Year))
other_deaths <- other_deaths  %>% mutate(variable = gsub('.{4}$', '', variable)) ## remove year from variable name
other_deaths <- other_deaths  %>% pivot_wider(names_from = variable, values_from = value)
other_deaths  <- other_deaths  %>% rename(other_deaths = Deaths)
other_deaths <- other_deaths  %>% mutate(Year = as.numeric(Year))

filenames <- list.files(paste0(dir, "/Education_Attainment_final_data"), pattern="*.csv", full.names=TRUE)

ldf <- lapply(filenames, read.csv, header=TRUE)
ldf <- ldf %>% reduce(rbind)
ldf <- ldf %>% filter(!Geographic.Area.Name == "")
ldf <- ldf %>% arrange(Geographic.Area.Name) %>% mutate(Year = rep_len(2010:2019, length.out = length(Geographic.Area.Name)))
ldf <- ldf %>% dplyr::rename(State = Geographic.Area.Name)




## Concat data


##unemployment
final_data <- combined_death %>% left_join(unemployment2, by.x = c(State, Year), by.y=c(State, Year))
## expansion date
final_data <- final_data %>% full_join(expansion_date, by="State")
final_data <- final_data %>% mutate(Year = year(as.Date(as.character(Year), format = "%Y")))

## Add physician data

final_data <- final_data %>% full_join(workforce)
final_data <- final_data %>% mutate(physician_per_pop = physician_total/all_pop)

#Add SEP data
final_data <- final_data %>% full_join(injection)

## add age data
final_data <- final_data %>% left_join(final, by = c("State", "Year"))

## spending data
final_data <- final_data %>% left_join(spending, by= c("State", "Year") )

## other deaths
final_data <- final_data %>% left_join(other_deaths, by= c("State", "Year") )
final_data <- final_data %>% mutate(other_deaths_perpop = other_deaths/all_pop)

final_data <- final_data %>% select(-c(Median_Household_Income, Med_HH_Income_Percent_of_State_Total))

## education data 
final_data <- final_data %>% left_join(ldf, by= c("State", "Year"))

## group data based on expansion date
# final_data <- final_data %>% mutate(expan_group = case_when(
#   is.na(date) == TRUE ~ "no_expansion",
#   date < as.Date("2014-02-01") ~ "early_expansion",
#   date > as.Date("2014-02-01") & date < as.Date("2018-01-01") ~"middle_expansion",
#   date > as.Date("2018-01-01") ~ "late_expansion"
# ))
final_data <- final_data %>% mutate(expan_group = case_when(
  is.na(date) == TRUE ~ "no_expansion",
  date < as.Date("2018-01-01") ~ "expansion",
  date > as.Date("2018-01-01") ~ "no_expansion"
))
final_data <- final_data %>% filter(!State %in% c("California", "Connecticut", "District of Columbia", "Minnesota", "Washington", "Colorado"))
final_data <- final_data %>% mutate(expan_group = as.factor(expan_group))
final_data <- final_data %>% filter(!is.na(Year))
```

## standardize data

final_data2<-subset(final_data, expan_group == "expansion") %>% mutate(difference = as.numeric(format(as.Date(as.character(Year), format = "%Y"), format = "%Y")) - as.numeric(format(date, format = "%Y")))
final_data2 <- final_data2 %>% filter(!(difference >3 | difference < -3) | is.na(difference))
#final_data <- final_data %>% mutate(difference = difference*-1)

## for no expansion --> y=0 is the mean of the expansion group at 2014. 
test <- final_data %>% group_by(expan_group) %>% summarize(mean_date = mean(date, na.rm=TRUE))

final_data1 <- subset(final_data, expan_group == "no_expansion") %>% mutate(difference =as.numeric(format(as.Date(as.character(Year), format = "%Y"), format = "%Y")) - 2014)
final_data1 <- final_data1 %>% filter(!(difference >3 | difference < -3) | is.na(difference))
#final_data1 <- final_data1 %>% mutate(difference = difference*-1)

final_data <- final_data2 %>% filter(!is.na(date)) %>% rbind(final_data1)


final_data <- final_data %>% mutate(higher_hs = (sum(Some.college.or.associate.s.degree+Bachelor.s.degree.or.higher))/all_pop)

final_data <- final_data %>% mutate(Unemployment_rate = Unemployed / Civilian_labor_force)




## group by expansion date

final_data_grouped <- final_data %>% group_by(expan_group, difference) %>% select_if(is.numeric) %>% summarise_at(vars(-group_cols(), c(all_deaths, all_pop, Employed, Unemployed, Civilian_labor_force)), sum, na.rm=TRUE)
count <- final_data %>% group_by(expan_group, difference) %>% summarize(count = n()) %>% pull(count)
final_data_grouped <- final_data_grouped %>% cbind(count) %>% rename(count = ...21)

final_data_grouped <- final_data_grouped %>% mutate(Unemployment_rate  = Unemployed / Civilian_labor_force,
                                                    rate = all_deaths / all_pop,
                                                    other_deaths_perpop = other_deaths / all_pop,
                                                    phyisican_perpop = physician_total/all_pop,
                                                    Mean_Age = Mean_Age /count,
                                                    spending_ave_pergroup = unadjmedamt / count,
                                                    injection_pergroup = legal_injection / count) %>% rename(death_rate= rate)



## Let's look at plots

final_data%>% ggplot(aes(difference, rate, color=expan_group))+stat_summary(geom = 'line') +
  #geom_vline(xintercept = 1994) +
  scale_x_continuous(breaks =seq(-3, 3,1))+
  ggtitle("Opioid Death Rate Before and After Expansion")+
  xlab("Time")+
  ylab("Death Rate (By population)")+
  labs(color = "Expansion Group")+
  geom_vline(xintercept = 0, color = "black", linetype = "dashed")+
  theme_bw()
ggsave("initial_plot.png", height = 8, width = 8)


## organize data for d-d 

# dummy for time
# final_data_grouped <- final_data_grouped %>% mutate(time = ifelse(difference >=0, 1, 0 ))
# final_data_grouped <- final_data_grouped %>% mutate(treated = ifelse(expan_group == "expansion", 1, 0))
# final_data_grouped <- final_data_grouped %>% mutate(did = time*treated)

final_data <- final_data %>%  mutate(time = ifelse(difference >=0, 1, 0 ))
final_data <- final_data %>%  mutate(treated = ifelse(expan_group == "expansion", 1, 0))





## modeling


library(plm)
fixed <- plm(rate ~ time*treated, index = c("State", "Year"), model = "within", data = final_data)
summary(fixed)

random <- plm(rate ~ time*treated, index = c("State", "Year"), model = "random", data = final_data)
summary(random)

phtest(random, fixed)

random1 <- plm(rate ~ time*treated + physician_per_pop + higher_hs+Unemployment_rate +factor(legal_injection), index = c("State", "Year"), model = "random", data = final_data)
summary(random1)

fixed1 <- plm(rate ~ time*treated + physician_per_pop + higher_hs +Unemployment_rate +factor(legal_injection), index = c("State", "Year"), model = "within", data = final_data)
summary(fixed1)

no_effects <- lm(rate ~ time*treated + physician_per_pop + higher_hs +Unemployment_rate, data = final_data)

library(stargazer)
stargazer(no_effects, fixed1, random1,
          title="Fixed Effects Vs. Random Effects",
          header=FALSE, 
          type="html", # "html" or "latex" (in index.Rmd) 
          omit.table.layout="n",
          digits=6, 
          single.row=TRUE,
          intercept.bottom=FALSE, #moves the intercept coef to top
          column.labels=c("No effects", "Fixed","Random"),
          dep.var.labels.include = FALSE,
          model.numbers = TRUE,
          dep.var.caption="Dependent variable: Opioid Death Rate",
          model.names=FALSE,
          star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001),
          out = "did_eqns.htm")


fixed2 <- plm(rate ~ time*treated + physician_per_pop + higher_hs +factor(legal_injection), index = c("State", "Year"), model = "within", data = final_data)
summary(fixed1)


library(stargazer)
stargazer(fixed1, fixed2,
          title="Fixed Effects Vs. Random Effects",
          header=FALSE, 
          type="html", # "html" or "latex" (in index.Rmd) 
          # omit.table.layout="n",
          digits=6, 
          single.row=TRUE,
          intercept.bottom=FALSE, #moves the intercept coef to top
          column.labels=c("Unemployment", "No Unemployment"),                  
          model.numbers = TRUE,
          dep.var.caption="Dependent variable: Opioid Death Rate",
          model.names=FALSE,
          star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001),
          out = "did_eqns1.htm")

phtest(random1, fixed1)

resid_fixed <- as.data.frame(resid(fixed1)) 
resid_random <- as.data.frame(resid(random1)) 

plot1 <- resid_fixed %>% ggplot(aes(`resid(fixed1)`)) + geom_histogram() + ggtitle("Fixed Effects Model") +xlab('Residuals') +theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))
plot2 <- resid_random %>% ggplot(aes(`resid(random1)`)) + geom_histogram()+ ggtitle("Random Effects Model")+xlab('Residuals')+theme_bw()+theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))
library(cowplot)
plot3 <-plot_grid(plot1, plot2, labels = c('A', 'B'), align = "hv")
save_plot(filename = "resid_histograms.png", plot = plot3)



## hausman for endogeneity

final_data <- final_data %>% mutate(did = time*treated)
fixed_resid <- resid(fixed1)

library(ivreg)

hausman <- ivreg(rate ~ time*treated + physician_per_pop + higher_hs +factor(State) |time+treated+higher_hs +physician_per_pop+Unemployment_rate, data = final_data)


library(GGally)
final_data5 <- final_data %>% ungroup() %>% select(-State) %>% select(rate,physician_per_pop, higher_hs, Unemployment_rate)
final_data5 <- final_data5 %>% rename(`Death Rate` = rate, `Physician Per Pop` = physician_per_pop,
                                      `% Higher HS` = higher_hs, `Unemployment Rate` = Unemployment_rate)
cor(final_data5, use = "complete.obs")
final_data5 %>% ggpairs(title="Correlations between Variables in the Model") + theme_bw()+ theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))
ggsave("correlation plot.png")


library(gtsummary)

final_data_fortbl <- final_data %>% ungroup() %>% select(physician_per_pop, higher_hs, Unemployment_rate, legal_injection, expan_group, rate)

tbl_summary(final_data_fortbl, by = expan_group, missing = "no") %>% modify_header(label = "**Variable**")%>% as_flex_table() %>%
  flextable::save_as_docx(path ="summarytable.docx")

library(tseries)
jarque.bera.test(resid_fixed$`resid(fixed1)`)

library(lmtest)
bptest(fixed1)

fitted <- as.data.frame(predict(fixed1))

residvfitted <- cbind(fitted,resid_fixed)

residvfitted %>% ggplot(aes(x = `predict(fixed1)`, y = `resid(fixed1)`)) + geom_point() + ylab("Residuals") +xlab("Fitted")+  geom_hline(yintercept = 0, color = "red", linetype = "dashed")+ggtitle("Redsiduals vs. Fitted")
ggsave("residvfit.png")


confint(fixed1)
confint(fixed2)


sink()


