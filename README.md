Package contains functions that allow production of a table via function statsd1 with each variable summarized as % (number) for categorical 
variables and mean or median (SD or IQR) for continuous variables for each group (supports up to 5 groups). If there are 
more than 2 groups, will also produce a table of p-values produced in pairwise testing. Categorical variables can be coded 
as 0 or 1, or using character names like 'one' or 'two'. All categorical variables with more than two possible values should 
be coded as character names. Run the line of code containing statsd1 individually and a prompt will appear to enter desired column names 
from the outputed table of descriptive statistics. There will be a column name prompt for each group. 

Can produce descriptive statistics that compare up to four groups (five coming soon) simultaneously. A single variable must be
created in your spreadsheet that differentiates the groups. In the below example the first group is given a value of 0 in the 
'Group' variable and the second group a value of 1. Either numbers or words can be supplied in the grouping variable like 
'one' or 'zero.'

Categorical variables can be compared using the chi-squared test or Fisher's exact test. Chi-squared is not appropriate for small
sample sizes thus requiring the use of Fisher's exact test. The exact test is technically always appropriate to perform, though 
commonly it is used when 20% of groups being compared have < 5 instances of the observed variable. This means that if we are comparing
four groups, if one of those groups had < 5 observations, that is 25% of groups, necessitating the exact test. The chosen cutoff value
in the app means that if any single group has less than that number of observations, the exact test will be used instead of chi-squared.

Continuous variables are compared using either a t-test or mann-whitney U test in comparing two groups and ANOVA or Kruskal-Wallis if 
comparing more than two groups. The parametric tests (t-test and ANOVA) are used to compare group means when variables are normally distributed. 
If variables are not normally distrubted, the mean is significantly affected by outliers and is thus a poor representation of the 'middle' 
of the distrubtion, which is why we compare medians in this case as they more accurately signify the middle of the distribution. Mann-whitney U
and Kruskal-wallis are the non-parametric tests used to compare group medians.

Variables are assessed for normality using the Shapiro-wilk test. However, the Shapiro-wilk test is not accurate when using small sample sizes
so if a group has fewer than 5 observations, it is automatically considered non-normal.

Categorical variables with more than two possible values should be used when these values are mutually exclusive, meaning that if a patient
takes on a certain value of the variable it is not possible that they could take on a different value simultaneously. A good example of this
is if patients were broken into age ranges as an individual cannot be different ages at the same time. In this case it is most appropriate 
to treat this variable as a true multi-level categorical variable that will produce a single p-value in a table 1 denoting differences between
groups in how many patients occupy each value of the multi-level categorical variable. Alternatively, for multi-level categorical variables 
that an individual could have different values for simultaneously are also possible. For example, a single patient can have multiple medical
diagnoses simultaneously. In this case, it is appropriate to transform each value of the categorical variable into an independent variable
(new column in a spreedsheet) where all patients will have either a '0' or '1' for this variable, denoting its presence of absence. All
non-mutally exclusive multi-level categorical variables should be transformed prior to uploading the spreedsheet into this program

When the number of groups is greater than 2, significant results of tests for categorical and continuous variables described above, 
simply demonstrate that at least one group median, mean, or number of observations significantly differs from one or more of the 
other groups. However, these significant results tells us nothing about which groups differ and at times we require this specific 
information. If a comparative test produces a p-value less than 0.05, pairwise testing will automatically be conducted between every
possible pair of groups. The results (p-values) of these tests will be put into a separate table that can be downloaded. The image
below shows how the table column numbers correspond to the pairs of groups tested.

