# Load the libraries for Likert scale conversion and statistical analysis
library(dplyr)
library(psych)
library(ggplot2)
library(scales)

# Read the survey results as data_junior frame (repeat the same for senior sample group)
data_junior <- readxl::read_excel("junior_results.xlsx", na = "NA")

# Modify the name of first column
names(data_junior)[1] <- "submit_date"

# Label the sample group (repeat the same for senior sample group)

data_junior$group <- "junior"

# Evaluation of reactor types
attributes(data_junior)$names[2:7] <- c("APWR","ABWR","HTGR","MSR","LFR","iPWR")

# Convert the answers to 5-level Likert scale
data_junior <- data_junior %>%
  mutate(
    APWR = recode(APWR, "Significantly disagree" = 1, "Disagree" = 2, 
                     "Neither disagree nor agree" = 3, "Agree" = 4, 
                     "Significantly agree" = 5, .default = NA_real_),
    ABWR = recode(ABWR, "Significantly disagree" = 1, "Disagree" = 2, 
                  "Neither disagree nor agree" = 3, "Agree" = 4, 
                  "Significantly agree" = 5, .default = NA_real_),
    HTGR = recode(HTGR, "Significantly disagree" = 1, "Disagree" = 2, 
                  "Neither disagree nor agree" = 3, "Agree" = 4, 
                  "Significantly agree" = 5, .default = NA_real_),
    MSR = recode(MSR, "Significantly disagree" = 1, "Disagree" = 2, 
                  "Neither disagree nor agree" = 3, "Agree" = 4, 
                  "Significantly agree" = 5, .default = NA_real_),
    LFR = recode(LFR, "Significantly disagree" = 1, "Disagree" = 2, 
                  "Neither disagree nor agree" = 3, "Agree" = 4, 
                  "Significantly agree" = 5, .default = NA_real_),
    iPWR = recode(iPWR, "Significantly disagree" = 1, "Disagree" = 2, 
                  "Neither disagree nor agree" = 3, "Agree" = 4, 
                  "Significantly agree" = 5, .default = NA_real_))

# Statistical analysis of results
reactor <- describe(data_junior[,2:7])

# sort(reactor$mean, decreasing = TRUE)

# barplot(sort(reactor$mean, decreasing = TRUE), names.arg = row.names(reactor), ylab = "mean",
#        ylim = c(0,5), main = "Mean values for reactor selection")

ggplot(reactor, aes(reorder(row.names(reactor),-mean), mean)) + 
  geom_bar(stat = "identity", width=0.8, alpha=0.8) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) + 
  labs(x="Reactor type", y="Perception (5-level Likert scale)")

# Evaluation of energy services
attributes(data_junior)$names[8:13] <- c("Industrial","Hydrogen",
                                 "Desalination","Synthetic",
                                 "Motive","Ammonia")

# Convert the answers to 5-level Likert scale
data_junior <- data_junior %>%
  mutate(
    Industrial = recode(Industrial, "Significantly disagree" = 1, "Disagree" = 2, 
                  "Neither disagree nor agree" = 3, "Agree" = 4, 
                  "Significantly agree" = 5, .default = NA_real_),
    Hydrogen = recode(Hydrogen, "Significantly disagree" = 1, "Disagree" = 2, 
                  "Neither disagree nor agree" = 3, "Agree" = 4, 
                  "Significantly agree" = 5, .default = NA_real_),
    Desalination = recode(Desalination, "Significantly disagree" = 1, "Disagree" = 2, 
                  "Neither disagree nor agree" = 3, "Agree" = 4, 
                  "Significantly agree" = 5, .default = NA_real_),
    Synthetic = recode(Synthetic, "Significantly disagree" = 1, "Disagree" = 2, 
                 "Neither disagree nor agree" = 3, "Agree" = 4, 
                 "Significantly agree" = 5, .default = NA_real_),
    Motive = recode(Motive, "Significantly disagree" = 1, "Disagree" = 2, 
                 "Neither disagree nor agree" = 3, "Agree" = 4, 
                 "Significantly agree" = 5, .default = NA_real_),
    Ammonia = recode(Ammonia, "Significantly disagree" = 1, "Disagree" = 2, 
                  "Neither disagree nor agree" = 3, "Agree" = 4, 
                  "Significantly agree" = 5, .default = NA_real_))

# Statistical analysis of results
reactor <- describe(data_junior[,8:13])

ggplot(reactor, aes(reorder(row.names(reactor),-mean), mean)) + 
  geom_bar(stat = "identity", width=0.8, alpha=0.8) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) + 
  labs(x="Energy services", y="Perception (5-level Likert scale)")

# SMR deployment horizon

names(data_junior)[14] <- "SMR_deployment_dates"

# Conversion to numerical values
df <- data_junior %>% 
  group_by(SMR_deployment_dates) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

# Statistical analysis of results
ggplot(df, aes(reorder(df$SMR_deployment_dates,-n), n)) + 
  geom_bar(stat = "identity", width=0.6, alpha=0.8) +
  scale_x_discrete(label=abbreviate(df$SMR_deployment_dates, minlength = 15)) +
  scale_y_continuous(breaks = breaks_pretty()) +
  labs(x="SMR deployment horizon", y="Counts") 

# Evaluation of SMR deployment indicators
attributes(data_junior)$names[15:22] <- c("Technology","Safety",
                                  "Burnup","Waste",
                                  "Clean","Cost","Cooling","Distributed")

# Convert the answers to 5-level Likert scale
data_junior <- data_junior %>%
  mutate(
    Technology = recode(Technology, "Significantly disagree" = 1, "Disagree" = 2, 
                        "Neither disagree nor agree" = 3, "Agree" = 4, 
                        "Significantly agree" = 5, .default = NA_real_),
    Safety = recode(Safety, "Significantly disagree" = 1, "Disagree" = 2, 
                      "Neither disagree nor agree" = 3, "Agree" = 4, 
                      "Significantly agree" = 5, .default = NA_real_),
    Burnup = recode(Burnup, "Significantly disagree" = 1, "Disagree" = 2, 
                          "Neither disagree nor agree" = 3, "Agree" = 4, 
                          "Significantly agree" = 5, .default = NA_real_),
    Waste = recode(Waste, "Significantly disagree" = 1, "Disagree" = 2, 
                       "Neither disagree nor agree" = 3, "Agree" = 4, 
                       "Significantly agree" = 5, .default = NA_real_),
    Clean = recode(Clean, "Significantly disagree" = 1, "Disagree" = 2, 
                    "Neither disagree nor agree" = 3, "Agree" = 4, 
                    "Significantly agree" = 5, .default = NA_real_),
    Cost = recode(Cost, "Significantly disagree" = 1, "Disagree" = 2, 
                     "Neither disagree nor agree" = 3, "Agree" = 4, 
                     "Significantly agree" = 5, .default = NA_real_),
    Cooling = recode(Cooling, "Significantly disagree" = 1, "Disagree" = 2, 
                  "Neither disagree nor agree" = 3, "Agree" = 4, 
                  "Significantly agree" = 5, .default = NA_real_),
    Distributed = recode(Distributed, "Significantly disagree" = 1, "Disagree" = 2, 
                     "Neither disagree nor agree" = 3, "Agree" = 4, 
                     "Significantly agree" = 5, .default = NA_real_))

# Statistical analysis of results
reactor <- describe(data_junior[,15:22])

ggplot(reactor, aes(reorder(row.names(reactor),-mean), mean)) + 
  geom_bar(stat = "identity", width=0.8, alpha=0.8) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) + 
  labs(x="Deployment indicators", y="Perception (5-level Likert scale)")

# Evaluation of Nuclear Fuel Cycle scenarios

# Technological Engagement and Innovation
attributes(data_junior)$names[23:28] <- c("Agenda","Network",
                                  "Applications","Research",
                                  "Science","Investment")

# Convert the answers to 3-point Likert scale
data_junior <- data_junior %>%
  mutate(
    Agenda = recode(Agenda, "Non-relevant" = 0, "Low importance" = 1, 
                        "Medium importance" = 2, "High importance" = 3, 
                        .default = NA_real_),
    Network = recode(Network, "Non-relevant" = 0, "Low importance" = 1, 
                    "Medium importance" = 2, "High importance" = 3, 
                    .default = NA_real_),
    Applications = recode(Applications, "Non-relevant" = 0, "Low importance" = 1, 
                    "Medium importance" = 2, "High importance" = 3, 
                    .default = NA_real_),
    Research = recode(Research, "Non-relevant" = 0, "Low importance" = 1, 
                    "Medium importance" = 2, "High importance" = 3, 
                    .default = NA_real_),
    Science = recode(Science, "Non-relevant" = 0, "Low importance" = 1, 
                    "Medium importance" = 2, "High importance" = 3, 
                    .default = NA_real_),
    Investment = recode(Investment, "Non-relevant" = 0, "Low importance" = 1, 
                    "Medium importance" = 2, "High importance" = 3, 
                    .default = NA_real_))

# Statistical analysis of results
reactor <- describe(data_junior[,23:28])

ggplot(reactor, aes(reorder(row.names(reactor),-mean), mean)) + 
  geom_bar(stat = "identity", width=0.8, alpha=0.8) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) + 
  labs(x="Technological Engagement and Innovation", y="Perception (3-point Likert scale)")

# Capacity Building and Knowledge Sharing
attributes(data_junior)$names[29:32] <- c("Capacity","Knowledge",
                                   "Organizations","Education")

# Convert the answers to 3-point Likert scale
data_junior <- data_junior %>%
  mutate(
    Capacity = recode(Capacity, "Non-relevant" = 0, "Low importance" = 1, 
                    "Medium importance" = 2, "High importance" = 3, 
                    .default = NA_real_),
    Knowledge = recode(Knowledge, "Non-relevant" = 0, "Low importance" = 1, 
                     "Medium importance" = 2, "High importance" = 3, 
                     .default = NA_real_),
    Organizations = recode(Organizations, "Non-relevant" = 0, "Low importance" = 1, 
                          "Medium importance" = 2, "High importance" = 3, 
                          .default = NA_real_),
    Education = recode(Education, "Non-relevant" = 0, "Low importance" = 1, 
                      "Medium importance" = 2, "High importance" = 3, 
                      .default = NA_real_))

# Statistical analysis of results
reactor <- describe(data_junior[,29:32])

ggplot(reactor, aes(reorder(row.names(reactor),-mean), mean)) + 
  geom_bar(stat = "identity", width=0.8, alpha=0.8) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) + 
  labs(x="Capacity Building and Knowledge Sharing", y="Perception (3-point Likert scale)")

# Safety, Regulation, and Standards
attributes(data_junior)$names[33:35] <- c("Compliance","Risks",
                                   "Impacts")

# Convert the answers to 3-point Likert scale
data_junior <- data_junior %>%
  mutate(
    Compliance = recode(Compliance, "Non-relevant" = 0, "Low importance" = 1, 
                      "Medium importance" = 2, "High importance" = 3, 
                      .default = NA_real_),
    Risks = recode(Risks, "Non-relevant" = 0, "Low importance" = 1, 
                       "Medium importance" = 2, "High importance" = 3, 
                       .default = NA_real_),
    Impacts = recode(Impacts, "Non-relevant" = 0, "Low importance" = 1, 
                           "Medium importance" = 2, "High importance" = 3, 
                           .default = NA_real_))

# Statistical analysis of results
reactor <- describe(data_junior[,33:35])

ggplot(reactor, aes(reorder(row.names(reactor),-mean), mean)) + 
  geom_bar(stat = "identity", width=0.8, alpha=0.8) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) + 
  labs(x="Safety, Regulation, and Standards", y="Perception (3-point Likert scale)")

# Economic Development and Sustainability
attributes(data_junior)$names[36:38] <- c("Reduction","Attraction",
                                   "Savings")

# Convert the answers to 3-point Likert scale
data_junior <- data_junior %>%
  mutate(
    Reduction = recode(Reduction, "Non-relevant" = 0, "Low importance" = 1, 
                        "Medium importance" = 2, "High importance" = 3, 
                        .default = NA_real_),
    Attraction = recode(Attraction, "Non-relevant" = 0, "Low importance" = 1, 
                   "Medium importance" = 2, "High importance" = 3, 
                   .default = NA_real_),
    Savings = recode(Savings, "Non-relevant" = 0, "Low importance" = 1, 
                     "Medium importance" = 2, "High importance" = 3, 
                     .default = NA_real_))

# Statistical analysis of results
reactor <- describe(data_junior[,36:38])

ggplot(reactor, aes(reorder(row.names(reactor),-mean), mean)) + 
  geom_bar(stat = "identity", width=0.8, alpha=0.8) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) + 
  labs(x="Economic Development and Sustainability", y="Perception (3-point Likert scale)")

# Energy Security and Waste Management
attributes(data_junior)$names[39:43] <- c("Assurance","Sufficient",
                                   "Development","Methods","Compactification")

# Convert the answers to 3-point Likert scale
data_junior <- data_junior %>%
  mutate(
    Assurance = recode(Assurance, "Non-relevant" = 0, "Low importance" = 1, 
                       "Medium importance" = 2, "High importance" = 3, 
                       .default = NA_real_),
    Sufficient = recode(Sufficient, "Non-relevant" = 0, "Low importance" = 1, 
                        "Medium importance" = 2, "High importance" = 3, 
                        .default = NA_real_),
    Development = recode(Development, "Non-relevant" = 0, "Low importance" = 1, 
                     "Medium importance" = 2, "High importance" = 3, 
                     .default = NA_real_),
    Methods = recode(Methods, "Non-relevant" = 0, "Low importance" = 1, 
                       "Medium importance" = 2, "High importance" = 3, 
                       .default = NA_real_),
    Compactification = recode(Compactification, "Non-relevant" = 0, "Low importance" = 1, 
                     "Medium importance" = 2, "High importance" = 3, 
                     .default = NA_real_))

# Statistical analysis of results
reactor <- describe(data_junior[,39:43])

ggplot(reactor, aes(reorder(row.names(reactor),-mean), mean)) + 
  geom_bar(stat = "identity", width=0.8, alpha=0.8) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) + 
  labs(x="Energy Security and Waste Management", y="Perception (3-point Likert scale)")

# Policy Framework and Decision Support
attributes(data_junior)$names[44:47] <- c("Planning","Nonproliferation",
                                   "Frameworks","Leadership")

# Convert the answers to 3-point Likert scale
data_junior <- data_junior %>%
  mutate(
    Planning = recode(Planning, "Non-relevant" = 0, "Low importance" = 1, 
                       "Medium importance" = 2, "High importance" = 3, 
                       .default = NA_real_),
    Nonproliferation = recode(Nonproliferation, "Non-relevant" = 0, "Low importance" = 1, 
                        "Medium importance" = 2, "High importance" = 3, 
                        .default = NA_real_),
    Frameworks = recode(Frameworks, "Non-relevant" = 0, "Low importance" = 1, 
                         "Medium importance" = 2, "High importance" = 3, 
                         .default = NA_real_),
    Leadership = recode(Leadership, "Non-relevant" = 0, "Low importance" = 1, 
                     "Medium importance" = 2, "High importance" = 3, 
                     .default = NA_real_))

# Statistical analysis of results
reactor <- describe(data_junior[,44:47])

ggplot(reactor, aes(reorder(row.names(reactor),-mean), mean)) + 
  geom_bar(stat = "identity", width=0.8, alpha=0.8) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) + 
  labs(x="Policy Framework and Decision Support", y="Perception (3-point Likert scale)")

# Interaction and Partnerships
attributes(data_junior)$names[48:50] <- c("Interaction","Relations","Ecosystem")

# Convert the answers to 3-point Likert scale
data_junior <- data_junior %>%
  mutate(
    Interaction = recode(Interaction, "Non-relevant" = 0, "Low importance" = 1, 
                      "Medium importance" = 2, "High importance" = 3, 
                      .default = NA_real_),
    Relations = recode(Relations, "Non-relevant" = 0, "Low importance" = 1, 
                              "Medium importance" = 2, "High importance" = 3, 
                              .default = NA_real_),
    Ecosystem = recode(Ecosystem, "Non-relevant" = 0, "Low importance" = 1, 
                        "Medium importance" = 2, "High importance" = 3, 
                        .default = NA_real_))

# Statistical analysis of results
reactor <- describe(data_junior[,48:50])

ggplot(reactor, aes(reorder(row.names(reactor),-mean), mean)) + 
  geom_bar(stat = "identity", width=0.8, alpha=0.8) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) + 
  labs(x="Interaction and Partnerships", y="Perception (3-point Likert scale)")

# Prospects for the Future
attributes(data_junior)$names[51:56] <- c("Technetium","Catalysts","Commercial",
                                   "Sources","Devices","Actinides")

# Convert the answers to 3-point Likert scale
data_junior <- data_junior %>%
  mutate(
    Technetium = recode(Technetium, "Non-relevant" = 0,"Very low confidence" = 1, 
                       "Low confidence" = 2, "Medium confidence" = 3, 
                       "High confidence" = 4, "Very high confidence" = 5, 
                       .default = NA_real_),
    Catalysts = recode(Catalysts, "Non-relevant" = 0,"Very low confidence" = 1, 
                        "Low confidence" = 2, "Medium confidence" = 3, 
                        "High confidence" = 4, "Very high confidence" = 5, 
                        .default = NA_real_),
    Commercial = recode(Commercial, "Non-relevant" = 0,"Very low confidence" = 1, 
                        "Low confidence" = 2, "Medium confidence" = 3, 
                        "High confidence" = 4, "Very high confidence" = 5, 
                        .default = NA_real_),
    Sources = recode(Sources, "Non-relevant" = 0,"Very low confidence" = 1, 
                        "Low confidence" = 2, "Medium confidence" = 3, 
                        "High confidence" = 4, "Very high confidence" = 5, 
                        .default = NA_real_),
    Devices = recode(Devices, "Non-relevant" = 0,"Very low confidence" = 1, 
                        "Low confidence" = 2, "Medium confidence" = 3, 
                        "High confidence" = 4, "Very high confidence" = 5, 
                        .default = NA_real_),
    Actinides = recode(Actinides, "Non-relevant" = 0,"Very low confidence" = 1, 
                        "Low confidence" = 2, "Medium confidence" = 3, 
                        "High confidence" = 4, "Very high confidence" = 5, 
                        .default = NA_real_))

# Statistical analysis of results
reactor <- describe(data_junior[,51:56])

ggplot(reactor, aes(reorder(row.names(reactor),-mean), mean)) + 
  geom_bar(stat = "identity", width=0.8, alpha=0.8) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=0.2) + 
  labs(x="Prospects for the Future", y="Perception (5-level Likert scale)")

write.csv(data_junior,"junior.csv", na = "NA")

# Concatenate the survey results in one file
data <- rbind(data_junior, data_senior)

# One-group Anova method

ABWR<-aov(ABWR~group,data=data)

summary(ABWR)