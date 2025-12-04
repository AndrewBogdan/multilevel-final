
# Tidyverse
library(tidyverse)
# Haven; data interoperability, especially labelled data.
library(haven)
# Lme4
library(lme4)

source("util.R")

see <- function(model) {
  broom.mixed::tidy(model) %>%
    left_join(
      confint(model, level=0.99, method="Wald") %>% as_tibble(rownames = "term"),
      by = "term"
    )
}

data <- readRDS("data/clean_data.rds") %>%
  drop_na(job_satis) %>%
  filter(add_count(., person_id)$n > 1) %>%
  mutate(
    job_satis = 4 - haven::zap_labels(job_satis),
    # I like the idea of using a monotone coding or something, but with
    #  only three elements on the scale, I don't think it matters
    mismatch = mismatch %>% as_factor() %>% fct_recode(
      ":C" = "Closely related", ":S" = "Somewhat related", ":N" = "Not related"
    ),
    reason = reason_1_for_mismatch %>% as_factor() %>%
      fct_relevel("Logical Skip") %>%
      fct_recode(
        ":Pay" = "Pay, promotion opportunities",
        ":Conds" = "Working conditions",
        ":Loc" = "Job location",
        ":Change" = "Change in career or professional interests",
        ":Fam" = "Family-related reasons",
        ":Job" = "Job in highest degree field not available",
        ":Other" = "Other reason for not working",
        ":NA" = "Logical Skip"
      ),
    job_type = as_factor(job_type) %>% fct_recode(
      ":CS/M Prof" = "Postsecondary teachers-Computer and math sciences",
      ":CS/M" = "Computer scientists and mathematicians",
      ":BioMed Sci" = "Biological and medical scientists",
      ":Bio Prof" = "Postsecondary teachers-Life related sciences",
      ":Other Bio" = "Other life and related scientists",
      ":Chem" = "Chemists, except biochemists",
      ":Phys" = "Physicists and astronomers",
      ":Sci Prof" = "Postsecondary teachers-Physical and related sciences",
      ":Other Sci" = "Other physical and related scientists",
      ":Econ" = "Economists",
      ":Psych" = "Psychologists",
      ":Soc Prof" = "Postsecondary teachers-Social and related sciences",
      ":SocSci" = "Other social scientists",
      ":Other Eng" = "Other engineers",
      ":ChemE" = "Chemical engineers",
      ":CivE" = "Civil engineers",
      ":EE" = "Electrical or computer hardware engineers",
      ":MechE" = "Mechanical engineers",
      ":Eng Prof" = "Postsecondary teachers - engineering",
      ":Health" = "Health-related occupations",
      ":SciE Man" = "science and engineering managers",
      ":SciE Teach" = "Science and engineering pre-college teachers",
      ":SciE Teach2" = "Science and engineering pre-college teachers",
      ":Exec" = "Top and mid-level managers, executives, administrators",
      ":OtherMan" = "Other management related occupations",
      ":LibTeach" = "Non-science and engineering pre-college and post-secondary teachers",
      ":Other" = "Other Non-science and engineering occupations"
    ),
    job_type_group = as_factor(job_type_group) %>% 
      fct_relevel("Non-science and engineering occupations") %>%
      fct_recode(
        ":CS/M" = "Computer and mathematical scientists",
        ":LifeSci" = "Biological, agricultural and other life scientists",
        ":PhysSci" = "Physical and related scientists",
        ":SocSci" = "Social and related scientists",
        ":Eng" = "Engineers",
        ":STEM-adj" = "Science and engineering related occupations",
        ":Non-STEM" = "Non-science and engineering occupations",
        ":X" = "Logical Skip"
      ),
    major = as_factor(major) %>% fct_recode(
      ":CS/M" = "Computer and mathematical sciences",
      ":Bio" = "Biological sciences",
      ":LifeS" = "Other biological, agricultural, environmental life sciences",
      ":Chem" = "Chemistry, except biochemistry",
      ":Phys" = "Physics and astronomy",
      ":OtherSci" = "Other physical and related sciences",
      ":Econ" = "Economics",
      ":Poli" = "Political and related sciences",
      ":Psych" = "Psychology",
      ":Soc" = "Sociology and anthropology",
      ":OtherSoc" = "Other social sciences",
      ":ChemE" = "Chemical engineering",
      ":CivE" = "Civil engineering",
      ":EE" = "Electrical, electronics and communications engineering",
      ":MechE" = "Mechanical engineering",
      ":OtherE" = "Other engineering",
      ":Health" = "Health-related fields",
      ":OtherSci" = "Other science and engineering-related",
      ":Man" = "Management and administration",
      ":Non-STEM" = "Other non-science and engineering",
      ":X" = "Missing"
    ),
    major_group = as_factor(major_group) %>% 
      fct_relevel("Non-science and engineering fields") %>%
      fct_recode(
        ":CS/M" = "Computer and mathematical sciences",
        ":LifeSci" = "Biological, agricultural and environmental life sciences",
        ":PhysSci" = "Physical and related sciences",
        ":SocSci" = "Social and related sciences",
        ":Eng" = "Engineering",
        ":STEM-adj" = "Science and engineering-related fields",
        ":Non-STEM" = "Non-science and engineering fields",
        ":X" = "Missing"
      ),
    highest_degree = highest_degree %>% as_factor() %>% fct_recode(
      ":B" = "Bachelor's", ":M" = "Master's", ":Dr" = "Doctorate",
      ":Pr" = "Professional"
    ),
    # Salary: we treat the top and bottom 7.5% of the scale (for reasons)
    #  and then take a log2 centered around $60k, which is a round number close
    #  to the median. We could do ln around $66k but that would be harder to read.
    # We have to do the CPI conversion _after_ the cutoffs, as one cutoff is 
    #  the saturation of the scale. I don't _like_ this, but I can't think of
    #  much better.
    salary_adj = ifelse(
      test = salary < 150000 & salary > 20000, 
      yes = log2(salary * cpi_2009_conversion / 60000),
      no = 0  # We have to code this as zero or else a matrix is singular.
    ),
    rich = salary == 150000, # Upper end of the scale, 7.5% are saturated
    poor = salary <= 20000, # Bottom 7.5%; these data are anomalous
    # I'm centering this around 36 to 40 hours per week, as that's full time.
    weekly_hours = weekly_hours - 3,
    # We adjusted salaries to 2009, so let's adjust year to that too.
    # Nevermind, we don't have 2009 in the data, let's do the most recent one.
    survey_year = survey_year - 2013,
    # Our sample is mostly men (about 60%) so I'm setting them as the default
    gender = gender %>% as_factor() %>% fct_relevel("Male") %>%
      fct_recode(":M" = "Male", ":F" = "Female"),
    # Center around 25 (my age) and divide by 10 (decades, for scaling)
    age = (age - 25) / 10,
    # Our data is 65% white people
    race = race %>% as_factor() %>% fct_relevel("White") %>%
      fct_recode(":W" = "White", ":A" = "Asian", ":M" = "Under-represented minorities")
  ) %>%
  cache("data-lm-read.rds", use_cache = FALSE)


cl_data <- data %>%
  mutate(job_satis = as_factor(job_satis)) %>%
  select(where(~ !any(is.na(.))))


print("Running clmm...")

model5_clmm <- ordinal::clmm(
  job_satis ~ 1 +
    # Main covariate
    mismatch +
    # Education covariates
    highest_degree +
    major_group +
    # Job covariates
    job_type_group +
    weekly_hours +  # Centered around 36-40 hrs/wk
    # Pay covariates
    poor +  # Bottom 7.5%
    salary_adj +  # Log2, centered @ $60k, 2009 dollars
    rich +  # Top 7.5% / $150k salary
    # Demographic
    gender +
    age +  # Decades around 25 years
    race +
    survey_year +  # Centered around 2013
    (1 | person_id), # Random intercept for each person
  data = cl_data,
  link = "logit",
) %>% cache("cache/clmm_model.rds", use_cache = FALSE)


print("Ran!")

see(model5_clmm)
