library(readxl)
food_correlations = read_excel("C:/Users/Aleksandar/Downloads/Mortality/output/Mortality and Food correlations.xlsx")

causes <- unique( food_correlations['Cause'] )
causes <- unlist( causes )
causes <- causes[! causes %in% 
c('diabetes', 
'metabolic_syndrome', 
'malig_neoplasm_of_breast',
'ischaemic_heart_diseases',
'diseases_of_liver',
'all_causes',
'malignant_neoplasms_of_digestive_organs') ]

library(plm)

output_file_path <- 'C:/Users/Aleksandar/Downloads/Mortality/r scripts/preferred_effects.txt'
file.create(output_file_path)

for(cause in causes) {
	df <- read.csv(file.path(paste("C:/Users/Aleksandar/Downloads/Mortality/output/datasets/icd9 summary, scaled data, combined columns, lag/", cause, ".csv", sep = "")))
	df <- pdata.frame(df, index = c("Country", "Year"), drop.index = FALSE, row.names = FALSE)
	print(cause)
	n <- names(df)
	f <- as.formula(paste('Total.Deaths ~ ', paste(n[!n %in% c('Country', 'Year', 'Total.Deaths')], collapse = " + ")))

	wi <- plm(f, data = df, model = 'within')
	re <- plm(f, data = df, model = 'random')
	
	# We prefer Random Effects under the null hypothesis
	preferred_model <- 'RE'
	if(phtest(wi, re)$p.value < 0.05) {
		# Reject null hypothesis
		preferred_model <- 'FE'
	}

	cat(paste0(cause, '=', preferred_model), file=output_file_path, append=TRUE, sep='\n')
}


