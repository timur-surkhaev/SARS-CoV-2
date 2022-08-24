library(openxlsx)
library(dplyr)
library(ggplot2)
library(reshape2)
library(scales)
library(tsibble)
library(imputeTS)
library(tsoutliers)
library(TTR)

getwd()
setwd("WORKING_DIRECTORY")

corona_cases = read.xlsx("Data/CoronaDF.xlsx", sheet=1)
corona_tests = read.xlsx("Data/CoronaDF.xlsx", sheet=2)

coronaDF = full_join(corona_cases,
                     corona_tests,
                     by="Date") %>%
  mutate(TestsPerformed = as.numeric(TestsPerformed),
         ConfirmedCases = as.numeric(ConfirmedCases),
         Date = as.Date(Date)-1) %>%
  arrange(desc(Date))


TestsPerDay = sapply(1:(length(coronaDF$TestsPerformed)-1),
                     function(i) {
                       coronaDF$TestsPerformed[[i]] - coronaDF$TestsPerformed[[i+1]]
                     })

coronaDF = coronaDF %>%
  filter(!Date == "2020-03-14") %>%
  mutate(TestsPerDay = TestsPerDay,
         Rating = ConfirmedCases/TestsPerDay) %>%
  filter(TestsPerDay > 0) 

coronaDF_filled = coronaDF %>%
  as_tsibble() %>%
  fill_gaps(.full = TRUE) %>%
  mutate(TestsPerDay = na_locf(TestsPerDay),
         ConfirmedCases = na_locf(ConfirmedCases)) 

# ??????? ?????????? ???????
coronaDF_filled[161,5] = coronaDF_filled[160,5]
coronaDF_filled[c(16:18), 5] = (coronaDF_filled[15,5]+coronaDF_filled[19,5])/2

coronaDF_filled %>%
  ggplot(aes(Date)) +
  geom_line(aes(y=TestsPerDay, color = "TestsPerDay")) +
  geom_line(aes(y=ConfirmedCases*10, color = "ConfirmedCases")) +
  scale_y_continuous(
    
    # Features of the first axis
    name = "TestsPerDay",
    labels=comma,
    # Add a second axis and specify its features
    sec.axis = sec_axis(~./10, name="ConfirmedCases", labels = comma) 

  ) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  ggtitle("????? ?? ???????????")

TestsPerDay_SMA = SMA(coronaDF_filled$TestsPerDay, n=1) 

coronaDF_filled$Rating = coronaDF_filled$ConfirmedCases/coronaDF_filled$TestsPerDay

plot.ts(coronaDF_filled$Rating)
plot.ts(SMA(coronaDF_filled$Rating, n=7))



ts = as.ts(TestsPerDay_SMA[c(50:150)])

plot.ts(ts)

TestsPerDay_components = decompose(ts)
