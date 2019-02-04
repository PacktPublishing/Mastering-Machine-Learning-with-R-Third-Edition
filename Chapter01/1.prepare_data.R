
# install.packages("caret")
# install.packages("janitor")
# install.packages("readr")
# install.packages("sjmisc")
# install.packages("skimr")
# install.packages("tidyverse")
# install.packages("vtreat")

library(magrittr)

gettysburg <- readr::read_csv("C:/Users/cory/Desktop/data/gettysburg.csv") # tilde
gettysburg <- readr::read_csv('gettysburg.csv')

colnames(gettysburg)

dim(gettysburg)

# str(gettysburg)

View(gettysburg)


# duplicates --------------------------------------------------------------

dupes <- duplicated(gettysburg)
table(dupes)
which(dupes == "TRUE")

# janitor::get_dupes(gettysburg)

gettysburg <- dplyr::distinct(gettysburg, .keep_all = TRUE)

gettysburg %>%
  dplyr::filter(army == "Confederate" & type == "Infantry") %>%
  sjmisc::descr() -> descr_stats
readr::write_csv(descr_stats, 'descr_stats.csv')

# skimr::skim(gettysburg) # group_by and dataframe

# distinct values of categories -------------------------------------------

# unique_count <- 
#   sapply(gettysburg, function(y)
#     unique(y))

#unique_count$type

dplyr::count(gettysburg, dplyr::n_distinct(type))

gettysburg_cat <-
  gettysburg[, sapply(gettysburg, class) == 'character']

gettysburg_cat %>%
  dplyr::summarize_all(dplyr::funs(dplyr::n_distinct(.))) # ad dataframe

gettysburg_cat %>% 
  dplyr::group_by(Cdr_casualty) %>%
  dplyr::summarise(num_rows = n()) # as dataframe

gettysburg_cat %>%
  janitor::tabyl(army, Cdr_casualty) # as dataframe

# missing value exploration ----------------------------------------------------------

na_count <-
  sapply(gettysburg, function(y)
    sum(length(which(is.na(
      y
    )))))

na_df <- data.frame(na_count)
View(na_df)

which(is.na(gettysburg[, 'killed']))

# Missing -----------------------------------------------------------------

gettysburg$missing_isNA <- 
  ifelse(is.na(gettysburg$missing), 1, 0)

gettysburg$missing[is.na(gettysburg$missing)] <- 0

# low or no variance ------------------------------------------------------

feature_variance <- caret::nearZeroVar(gettysburg, saveMetrics = TRUE)

head(feature_variance)

which(feature_variance$zeroVar == 'TRUE')

row.names(feature_variance[17, ])

gettysburg_fltrd <- gettysburg[, feature_variance$zeroVar == 'FALSE']

# other tools -------------------------------------------------------------

# gettysburg_fltrd <- janitor::clean_names(gettysburg_cat, case = "lower_camel")

# Treatment ---------------------------------------------------------------

my_treatment <- vtreat::designTreatmentsZ(
  dframe = gettysburg_fltrd,
  varlist = colnames(gettysburg_fltrd),
  minFraction = 0.05
)

gettysburg_treated <- vtreat::prepare(my_treatment, gettysburg_fltrd)

dim(gettysburg_treated)

colnames(gettysburg_treated)

table(gettysburg_treated$type_catP)

gettysburg_treated <- 
  gettysburg_treated %>%
  dplyr::select(-dplyr::contains('_catP'))

# table(gettysburg_treated$wounded_isBAD)


# clean up names ----------------------------------------------------------

colnames(gettysburg_treated) <-
  sub('_clean', "", colnames(gettysburg_treated))

colnames(gettysburg_treated) <-
  sub('_isBAD', "_isNA", colnames(gettysburg_treated))


# tidy correlation --------------------------------------------------------

df_corr <- cor(gettysburg_treated, method = "spearman")

high_corr <- caret::findCorrelation(df_corr, cutoff = 0.9)

high_corr
colnames(gettysburg_treated)[c(9, 4, 22, 43, 3, 5)]

gettysburg_noHighCorr <- gettysburg_treated[, -high_corr]

df_corr <- data.frame(df_corr)

df_corr$feature1 <- row.names(df_corr)

gettysburg_corr <-
  tidyr::gather(data = df_corr,
                key = "feature2",
                value = "correlation",
                -feature1)

gettysburg_corr <- 
  gettysburg_corr %>%
  dplyr::filter(feature1 != feature2)


# linear combination ------------------------------------------------------

linear_combos <- caret::findLinearCombos(gettysburg_noHighCorr)

linear_combos

colnames(gettysburg_noHighCorr)[c(16, 7, 8, 9, 10, 11, 12, 13, 14, 15)]

linear_remove <- colnames(gettysburg_noHighCorr[16])

df <- gettysburg_noHighCorr[, !(colnames(gettysburg_noHighCorr) %in% linear_remove)]

dim(df)
