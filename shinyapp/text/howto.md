1. Upload your data file in CSV format. R default options for `read.csv` will apply except for missing values where both (NA) and dot (.) are treated as missing. If your data has columns with other non-numeric missing value codes then they be treated as factors.

2. The UI is dynamic and changes depending on your choices of options, x ,y, filter, group and so on.


3. It is assumed that your data is tidy and ready for plotting (long format).

4. x and y variable(s) input allow numeric and factor variables.

5. You can select more than one y variable. The data will always be automatically stacked using `tidyr::gather` and result in yvars and yvalues variables. If you select a mix of factor and continuous variables, all selected variables will be transformed to factor. The internal variable yvalues is used for y variable mapping and you can select yvars for facetting. The app automatically select additional row split as yvars and set Facet Scales to free_y. To change Facet Scales and many other options go to Graph Options tab.

6. Inputs, Categorize/Rename, Recode into Binned Categories: Include the numeric variable to change to categorical in the list and then choose a number of cuts (default is 3). This helps when you want to group or color by cuts of a continuous variable.

7. Inputs, Categorize/Rename, Treat as Categories: This changes a numeric variable to factor without binning. This helps when you want to group or color by numerical variable that has few unique values e.g. 1,2,3.

8. Inputs, Categorize/Rename, Custom cuts: You can cut a numeric variable to factor using specified cutoffs, by default a text input is created and  comma separated min, median, max are used. You can override with any values of your liking. An optional Treat as Numeric checkbox can be selected to recode the created custom cuts variable to numeric values that start with 0. This can be useful to overlay a logistic smooth or to use a Kaplan-Meier curve. Numeric Codes/values correspondence are shown in text below the checkbox.

9. Inputs, Categorize/Rename, Change labels of this variable: this field allow you to select a factor/character variable in order to input in a comma separated list of new labels. By default, value1, value2,...valuen will be populated in the field. Make sure to edit/type keeping the correct number of new labels otherwise the plot will not be generated as the recoding will fail. You can combine levels by repeating a value with special attention to spaces e.g. 1,1,2.

10. Inputs, Categorize, Combine Variables: This allows to paste together two variables other than those used for y axis (e.g. Sex with values: Male and Female and Treatment with values: TRT1, TRT2 and TRT3) to construct a new one called combinedvariable with values: Male TRT1, Male TRT2, Male TRT3, Female TRT1, Female TRT2, Female TRT3. The combinedvariable is available to color, group and any other mappings and by default a dummy placeholder is made available.

11. Inputs, Filters: There is six slots for Filter variables. Filter variables 1, 2,3,4,5 and 6 are applied sequentially. Values shown for filter variable 2 will depend on your selected data exclusions using filter variable 1 and so on. The first three filters accept numeric and non numeric columns while the last three are sliders and only work with numeric variables. For performance improvement the first three filters only show variables with a default maximum number of levels of 500 but you can increase it to your needs.

12. Inputs, Simple Rounding: allows you to round a numeric variable using the n digits you specify and defaults to 0. This convert values like 1.11, 1.12, 2.1 to 1, 1, 2 which provide an easy way to bin continuous values for better viewing in the plot.

13. New ! Inputs, Reorder Variables: This group of options allow you to Reorder a categorical variable using a function of another numerical variable and default to using the median of yvalues. Maximum and Minimum are possible as well as choosing the variable of your liking. You can also check the Reverse Order box to have the order inverted. The Custom Reorder this variable will create an input field with unique values of this variable and then you can drag and drop to the order you like. Finally the change labels of this variable is another place where you can change the names of the levels of a character variable like the one holding the names of the selected y variables yvars which is handy if you want to change the names in the facet labels. 

14. You can use smoothing functions including, loess, linear and logistic fits. Please make sure that data is compatible with the smoothing used i.e. for logistic a numerical 0/1 variable is expected.

15. You can also use quantile regression, median/percentiles, mean/CI. The median menu has options to label the plot with the median or the number of data points used for the median computations.

16. Limited boxplots support. Carefully choose grouping expecially when the x axis variable is continuous, you can change the Group By: variable in the Color/Group/Split/Size/Fill Mappings (?) to better reflect your needs. It is also possible to join the boxplots with a line connecting medians and percentiles but for this to work color and group should be both set to none or to the same variable. In addition, the ignore grouping variable in the median menu should be unchecked.

17. Initial support to enable Kaplan-Meier Plots with Confidence Intervals, censoring ticks and commonly used transformations. When K-M curves are added nothing else is show on the plot.

18. Download the plot using the options on the 'Download' tab. This section is based on code from Mason DeCamillis ggplotLive app.

19. Look at the data table in the 'Data' tab. You can reorder the columns, filter and much more.


Contact me @ samermouksassi@gmail.com for feedback or use the link below to file bugs/features/pull requests!

<a href="https://github.com/smouksassi/ggplotwithyourdata/issues" target="_blank">Click Here to submit an issue on github</a>

*Samer Mouksassi 2016*
