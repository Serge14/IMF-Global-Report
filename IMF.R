setwd("/home/sergiy/Documents/Work/Nutricia/Global/IMF-DRY/IMF")

library(data.table)

# Read all necessary files
dictBrands = fread("dictBrands.csv", stringsAsFactors = FALSE)
dictCompany = fread("dictCompany.csv", stringsAsFactors = FALSE)
dictSegments = fread("dictSegments.csv", stringsAsFactors = FALSE)
dictGR = fread("GRIMFDictionary.csv", stringsAsFactors = FALSE, data.table = TRUE)
selection = read.csv("Selection.csv", stringsAsFactors = FALSE)
data = fread("/home/sergiy/Documents/Work/Nutricia/Rework/201712/BFprocessed.csv", 
             header = TRUE, stringsAsFactors = FALSE, data.table = TRUE)
houses = fread("houses.csv", stringsAsFactors = FALSE, data.table = TRUE)
#exclude = read.csv("exclude.csv", stringsAsFactors = FALSE)
#flavoured = read.csv("IMFFlavoured.csv", stringsAsFactors = FALSE)

# Change the name of the Mlysh Istrinskiy to bring in line with short name
selection$Brand[selection$Brand == "MALYSH ISTRINSKII"] = "MALYSH ISTR"

# IF PLUS must be moved to FO
data = data[PS3 == "PLUS" & PS2 == "IF", PS2 := "FO"]

# Rename company in order to align names with global report names
setkey(dictCompany, RTRICompanyName)
setkey(data, Company)
data[dictCompany, Company := GRCompanyName]

# Rename brands in order to align names with global report names
setkey(dictBrands, BrandName)
setkey(data, Brand)
data[dictBrands, Brand := GRBrandName]

# Create subset which consists only segments we'll work with
df = data[, .(Items = sum(ITEMSC), Value = sum(VALUEC), Volume = sum(VOLUMEC)),
          by = .(Brand, PS0, PS2, PS3, PS, Company, Ynb, Mnb, Additives, PriceSegment)]

#df = data[, .(Items = sum(ItemsC), Value = sum(ValueC), Volume = sum(VolumeC)),
#          by = .(Brand, PS0, PS2, PS3, PS, Company, Ynb, Mnb, Additives, `Price Segment`)]

# Add houses to the dataset
setkey(houses, Brand)
setkey(df, Brand)
df[houses, House := House]

# Create master dataset
df1 = df[PS0 == "IMF", .(PS0, Company, Brand, House, Ynb, Mnb, Volume, Value)]
names(df1)[1] = "Segment"

df2 = df[PS2 == "IF" | PS2 == "FO", .(PS2, Company, Brand, House, Ynb, Mnb, Volume, Value)]
names(df2)[1] = "Segment"
df2[, Segment := "IFFO"]
df1 = rbind(df1, df2)

df2 = df[PS == "BIF" | PS == "BFO" | PS == "BPIF" | PS == "BPFO", 
         .(PS2, Company, Brand, House, Ynb, Mnb, Volume, Value)]
names(df2)[1] = "Segment"
df2[, Segment := "IFFO BASE&PLUS"]
df1 = rbind(df1, df2)

df2 = df[PS == "BPIF" | PS == "BFO" | PS == "BPFO", 
         .(PS, Company, Brand, House, Ynb, Mnb, Volume, Value)]
names(df2)[1] = "Segment"
df2[, Segment := "FO BASE&PLUS"]
df1 = rbind(df1, df2)

df2 = df[(PS2 == "IF" | PS2 == "FO") & PS3 == "SPECIALS", .(PS, Company, Brand, House, Ynb, Mnb, Volume, Value)]
names(df2)[1] = "Segment"
df2[, Segment := "IFFO SPECIALS"]
df1 = rbind(df1, df2)

df2 = df[PS0 == "IMF", .(PS3, Company, Brand, House, Ynb, Mnb, Volume, Value)]
names(df2)[1] = "Segment"
df1 = rbind(df1, df2)

df2 = df[PS0 == "IMF", .(PS2, Company, Brand, House, Ynb, Mnb, Volume, Value)]
names(df2)[1] = "Segment"
df1 = rbind(df1, df2)

df2 = df[PS3 == "BASE", .(PS2, Company, Brand, House, Ynb, Mnb, Volume, Value)]
names(df2)[1] = "Segment"
df2[, Segment := paste(Segment, "BASE")]
df1 = rbind(df1, df2)

df2 = df[PS3 == "SPECIALS", .(PS2, Company, Brand, House, Ynb, Mnb, Volume, Value)]
names(df2)[1] = "Segment"
df2[, Segment := paste("SPECIALS", Segment)]
df1 = rbind(df1, df2)

df2 = df[, .(PS, Company, Brand, House, Ynb, Mnb, Volume, Value)]
names(df2)[1] = "Segment"
df1 = rbind(df1, df2)

df2 = df[PS2 == "GUM", .(Additives, Company, Brand, House, Ynb, Mnb, Volume, Value)]
names(df2)[1] = "Segment"
df2[, Segment := paste("GUM", Segment)]
df1 = rbind(df1, df2)
#!!!!!!!
df2 = df[PS3 == "SPECIALS", .(PS, Company, Brand, House, Ynb, Mnb, Volume, Value)]
names(df2)[1] = "Segment"
df2[, Segment := paste("SPECIALS", Segment)]
#df2[, Segment := paste("GUM", Segment)]
df1 = rbind(df1, df2)

df2 = df[PS2 == "GUM" & PS3 == "SPECIALS", .(PS, Company, Brand, House, Ynb, Mnb, Volume, Value)]
names(df2)[1] = "Segment"
df2[, Segment := paste("GUM SPECIALS", Segment)]
df1 = rbind(df1, df2)

df2 = df[PS0 == "IMF", .(PriceSegment, Company, Brand, House, Ynb, Mnb, Volume, Value)]
names(df2)[1] = "Segment"
df1 = rbind(df1, df2)

# Select brands and companies to report
sel = unique(selection$Company)
df1 = df1[!(Company %in% sel), Company := "ALL OTHER MANUFACTURERS"]
sel = unique(selection[c("Company", "Brand")])
#df1 = df1[!(Brand %in% sel), Brand := "ALL OTHER BRANDS"]
df1 = df1[!(paste0(df1$Company, df1$Brand) %in% paste0(sel$Company, sel$Brand)), Brand := "ALL OTHER BRANDS"]

collect = data.table::dcast(df1, Segment ~ Ynb + Mnb, value.var = c("Volume", "Value"), fun.aggregate = sum)
names(collect)[1] = "Join"

dftemp = data.table::dcast(df1, Segment + Company ~ Ynb + Mnb, value.var = c("Volume", "Value"), fun.aggregate = sum)
dftemp$Segment = paste0(dftemp$Segment, "_", dftemp$Company)
names(dftemp)[1] = "Join"
dftemp = dftemp[,-2]

collect = rbind(collect, dftemp)

dftemp = data.table::dcast(df1, Segment + Company + Brand ~ Ynb + Mnb, value.var = c("Volume", "Value"), 
                           fun.aggregate = sum)
dftemp$Segment = paste0(dftemp$Segment, "_", dftemp$Company, "_", dftemp$Brand)
names(dftemp)[1] = "Join"
dftemp = dftemp[,c(-2, -3)]

collect = rbind(collect, dftemp)

#Add Houses to export file

df1 = df1[Company == "DANONE"]

dftemp = data.table::dcast(df1, Segment + Company + House ~ Ynb + Mnb, value.var = c("Volume", "Value"), 
                           fun.aggregate = sum)
dftemp$Segment = paste0(dftemp$Segment, "_", dftemp$Company, "_", dftemp$House)
names(dftemp)[1] = "Join"
dftemp = dftemp[,c(-2, -3)]

collect = rbind(collect, dftemp)

dftemp = data.table::dcast(df1, Segment + Company + Brand + House ~ Ynb + Mnb, value.var = c("Volume", "Value"), 
                           fun.aggregate = sum)
dftemp$Segment = paste0(dftemp$Segment, "_", dftemp$Company, "_", dftemp$Brand, "_", dftemp$House)
names(dftemp)[1] = "Join"
dftemp = dftemp[,c(-2, -3, -4)]

# Collect file is needed to check, final is final. Data are identical
collect = rbind(collect, dftemp)

# Calculate number of columns
nc = length(names(collect)) -1

#Calculate additionalaggregated columns
collect$Volume_FY2015=collect[, Reduce(`+`, .SD), .SDcols = grep("Volume_2015", names(collect), value = T)]
collect$Volume_FY2016=collect[, Reduce(`+`, .SD), .SDcols = grep("Volume_2016", names(collect), value = T)]
collect$Volume_MAT2YA = collect[, Reduce(`+`, .SD), .SDcols = (nc/2 - 34):(nc/2 - 23)]
collect$Volume_MATLY = collect[, Reduce(`+`, .SD), .SDcols = (nc/2 - 22):(nc/2 - 11)]
collect$Volume_MAT = collect[, Reduce(`+`, .SD), .SDcols = (nc/2 - 10):(nc/2 + 1)]

collect$Value_FY2015=collect[, Reduce(`+`, .SD), .SDcols = grep("Value_2015", names(collect), value = T)]
collect$Value_FY2016=collect[, Reduce(`+`, .SD), .SDcols = grep("Value_2016", names(collect), value = T)]
collect$Value_MAT2YA = collect[, Reduce(`+`, .SD), .SDcols = (nc - 34):(nc - 23)]
collect$Value_MATLY = collect[, Reduce(`+`, .SD), .SDcols = (nc - 22):(nc - 11)]
collect$Value_MAT = collect[, Reduce(`+`, .SD), .SDcols = (nc - 10):(nc + 1)]

# Reorder columns
setcolorder(collect, c(1, (nc+2):(nc+6), 2:(nc/2+1), (nc+7):(nc+11), (nc/2+2):(nc+1)))

write.csv(collect, "collect.csv")

# Join with GR file
final = collect[dictGR, on = "Join"]

# Delete temporary columns
final = final[, -(dim(final)[2]-6):-dim(final)[2]]
final[is.na(final)] = 0
write.csv(final, "final.csv")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
