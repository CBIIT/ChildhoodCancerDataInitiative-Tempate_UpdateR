#!/usr/bin/env Rscript

#Childhood Cancer Data Initiative - Template_UpdateR.R

#This script will take a CCDI metadata manifest file and transfer the contents to a different version of the CCDI metadata manifest.

##################
#
# USAGE
#
##################

#Run the following command in a terminal where R is installed for help.

#Rscript --vanilla CCDI-CatchERR.R --help


##################
#
# Env. Setup
#
##################

#List of needed packages
list_of_packages=c("readr","dplyr","tidyr","knitr","openxlsx","stringi","readxl","janitor","optparse","tools")

#Based on the packages that are present, install ones that are required.
new.packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
suppressMessages(if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org"))

#Load libraries.
suppressMessages(library(readr,verbose = F))
suppressMessages(library(dplyr,verbose = F))
suppressMessages(library(tidyr,verbose = F))
suppressMessages(library(knitr,verbose = F))
suppressMessages(library(readxl,verbose = F))
suppressMessages(library(openxlsx, verbose = F))
suppressMessages(library(stringi,verbose = F))
suppressMessages(library(janitor,verbose = F))
suppressMessages(library(optparse,verbose = F))
suppressMessages(library(tools,verbose = F))


#remove objects that are no longer used.
rm(list_of_packages)
rm(new.packages)


##################
#
# Arg parse
#
##################

#Option list for arg parse
option_list = list(
  make_option(c("-f", "--file"), type="character", default=NULL, 
              help="dataset file, CCDI_submission_metadata_template.xlsx", metavar="character"),
  make_option(c("-t", "--template"), type="character", default=NULL, 
              help="dataset template file, CCDI_submission_metadata_template.xlsx (likey a newer version)", metavar="character")
)

#create list of options and values for file input
opt_parser = OptionParser(option_list=option_list, description = "\nCCDI-Template_UpdateR v1.0.0")
opt = parse_args(opt_parser)

#If no options are presented, return --help, stop and print the following message.
if (is.null(opt$file)&is.null(opt$template)){
  print_help(opt_parser)
  cat("Please supply both the input file (-f) and template file (-t), CCDI_submission_metadata_template.xlsx.\n\n")
  suppressMessages(stop(call.=FALSE))
}


#Data file pathway
file_path=file_path_as_absolute(opt$file)

#Template file pathway
template_path=file_path_as_absolute(opt$template)

cat("\nThe CCDI data template is being updated at this time.\n")


###########
#
# File name rework
#
###########

#Rework the file path to obtain a file extension.
file_name=stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][2])
ext=tolower(stri_reverse(stri_split_fixed(stri_reverse(basename(file_path)),pattern = ".", n=2)[[1]][1]))
path=paste(dirname(file_path),"/",sep = "")

#Output file name based on input file name and date/time stamped.
output_file=paste(file_name,
                  "_Updater",
                  stri_replace_all_fixed(
                    str = Sys.Date(),
                    pattern = "-",
                    replacement = ""),
                  sep="")


##############
#
# Read in each tab and apply to a data frame list
#
##############

#determine sheets in submission template
sheet_names_file=readxl::excel_sheets(path = file_path)
sheet_names_temp=readxl::excel_sheets(path = template_path)
non_template_sheets=c("README and INSTRUCTIONS","Dictionary","Terms and Value Sets")
sheet_names_file=sheet_names_file[!sheet_names_file %in% non_template_sheets]
sheet_names_temp=sheet_names_temp[!sheet_names_temp %in% non_template_sheets]

# A bank of NA terms to make sure NAs are brought in correctly
NA_bank=c("NA","na","N/A","n/a")

#Establish the list for incoming file
workbook_list=list()
incomplete_node=c()

#create a list of all node pages with data
for (node in sheet_names_file){
  #read the sheet
  df=suppressMessages(read_xlsx(path = file_path, trim_ws = TRUE, na=NA_bank, sheet = node, guess_max = 1000000, col_types = "text"))
  #df=readWorkbook(xlsxFile = file_path,sheet = node, na.strings = NA_bank)
  #create an emptier version that removes the type and makes everything a character
  df_empty_test=df%>%
    select(-type)%>%
    mutate(across(everything(), as.character))
  #remove empty rows and columns
  df_empty_test=remove_empty(df_empty_test,c("rows","cols"))
  
  #if there are at least one row in the resulting data frame, add it
  if (dim(df_empty_test)[1]>0){
    #if the only columns in the resulting data frame are only linking properties (node.node_id), do not add it.
    if (any(!grepl(pattern = "\\.",x = colnames(df_empty_test)))){
      #add the data frame to the workbook
      workbook_list=append(x = workbook_list,values = list(df))
      names(workbook_list)[length(workbook_list)]<-node
    }else{
      incomplete_node=c(incomplete_node,node)
    }
  }
}

nodes_present=names(workbook_list)


#Establish the list for the template file
workbook_list_temp=list()
incomplete_node=c()

#create a list of all node pages with data
for (node in sheet_names_temp){
  #read the sheet
  df=suppressMessages(read_xlsx(path = template_path, trim_ws = TRUE, na=NA_bank, sheet = node, guess_max = 1000000, col_types = "text"))
  #create an emptier version that removes the type and makes everything a character
  df_empty_test=df%>%
    mutate(across(everything(), as.character))
  
  #if there are at least one row in the resulting data frame, add it
  if (dim(df)[1]>0){
      #add the data frame to the workbook
      workbook_list_temp=append(x = workbook_list_temp,values = list(df))
      names(workbook_list_temp)[length(workbook_list_temp)]<-node
  }
}

nodes_present_temp=names(workbook_list_temp)

temp_node_prop=names(unlist(x = workbook_list_temp,recursive = TRUE))
temp_node_prop_df=tibble(temp_node_prop=temp_node_prop)

temp_node_prop_df=temp_node_prop_df%>%
  separate(temp_node_prop,c("temp_node","temp_prop"),"\\.",extra = "merge")

workbook_list_empty=workbook_list

for (node in nodes_present){
  workbook_list_empty[node][[1]]=workbook_list_empty[node][[1]][1,]
}

wb_node_prop=names(unlist(x = workbook_list_empty,recursive = TRUE))
wb_node_prop_df=tibble(wb_node_prop=wb_node_prop)

wb_node_prop_df=wb_node_prop_df%>%
  separate(wb_node_prop,c("wb_node","wb_prop"),"\\.",extra = "merge")


#create empty df that notes placements that are not 1:1 between models.
wb_node_prop_not_placements_df=wb_node_prop_df
wb_node_prop_not_placements_df$wb_change<-NA
wb_node_prop_not_placements_df$temp_node_new<-NA
wb_node_prop_not_placements_df=wb_node_prop_not_placements_df[0,]

sink(file = paste(path,output_file,"_log.txt",sep = ""))

cat("The following output will note 'WARNING' and 'ERRORS' based on whether the property was relocated to a different node or not placed within the new template.\n-----------\n")

for (x in 1:dim(wb_node_prop_df)[1]){
  wb_node_prop_add=wb_node_prop_df[x,]
  wb_node=wb_node_prop_add$wb_node
  wb_prop=wb_node_prop_add$wb_prop
  
  #if the property does not have any values, skip
  if (any(!is.na(workbook_list[wb_node][[1]][wb_prop][[1]]))){
    
    temp_nodes= unique(temp_node_prop_df$temp_node)
    
    #if the node is found
    if (wb_node %in% temp_nodes){
      temp_props= filter(temp_node_prop_df, temp_node==wb_node)$temp_prop
      
      #if the property is found
      if (wb_prop %in% temp_props){
        
        #check to make sure the data frames are the same length
        workbook_length=dim(workbook_list[wb_node][[1]])[1]
        template_length=dim(workbook_list_temp[wb_node][[1]])[1]
        if(workbook_length!=template_length){
          #create a stand in matrix to replace with the correct amount if the lengths are different, usually happens for the initial data frame creation
          replace_df=as.data.frame(matrix(nrow=workbook_length,ncol = dim(workbook_list_temp[wb_node][[1]])[2]))
          colnames(replace_df)<-names(workbook_list_temp[wb_node][[1]])
          workbook_list_temp[wb_node][[1]]=replace_df
        }
        
        #write out the values
        workbook_list_temp[wb_node][[1]][wb_prop]=workbook_list[wb_node][[1]][wb_prop]
        
        #if the property is not within the same node
      }else{
        #if the property is found elsewhere, but not in a file node or diagnosis node (since those are two nodes that share a decent subset of properties)
        if(wb_prop %in% temp_node_prop_df$temp_prop &
           (!grepl(pattern = "file", x = wb_node) &
            !grepl(pattern = "diagnosis", x = wb_node))){
          temp_node_pos=grep(pattern = TRUE, x = temp_node_prop_df$temp_prop %in% wb_prop)
          temp_node_new=temp_node_prop_df$temp_node[temp_node_pos]
          workbook_list_temp[temp_node_new][[1]][wb_prop]=workbook_list[wb_node][[1]][wb_prop]
          
          cat("\nWARNING: The following property, ",wb_prop,", for the following node, ",wb_node, ", was instead placed in the following node, ", temp_node_new,", for the same property in the new template.", sep="")
          
          wb_node_prop_add$wb_change="relocated"
          wb_node_prop_add$temp_node_new=temp_node_new
          wb_node_prop_not_placements_df=rbind(wb_node_prop_not_placements_df,wb_node_prop_add)
        }else{
          cat("\nERROR: The following property, ",wb_prop,", for the following node, ",wb_node, ", did not find a placement in the new template.", sep="")
          
          wb_node_prop_add$wb_change="not_placed"
          wb_node_prop_add$temp_node_new=NA
          wb_node_prop_not_placements_df=rbind(wb_node_prop_not_placements_df,wb_node_prop_add)
        }
      }
    }else{
      #if the property is found elsewhere, but not in a file node or diagnosis node (since those are two nodes that share a decent subset of properties)
      if(wb_prop %in% temp_node_prop_df$temp_prop &
         (!grepl(pattern = "file", x = wb_node) &
          !grepl(pattern = "diagnosis", x = wb_node))){
        temp_node_pos=grep(pattern = TRUE, x = temp_node_prop_df$temp_prop %in% wb_prop)
        temp_node_new=temp_node_prop_df$temp_node[temp_node_pos]
        workbook_list_temp[temp_node_new][[1]][wb_prop]=workbook_list[wb_node][[1]][wb_prop]
        
        cat("\nWARNING: The following property, ",wb_prop,", for the following node, ",wb_node, ", was instead placed in the following node, ", temp_node_new,", for the same property in the new template.", sep="")
        
        wb_node_prop_add$wb_change="relocated"
        wb_node_prop_add$temp_node_new=temp_node_new
        wb_node_prop_not_placements_df=rbind(wb_node_prop_not_placements_df,wb_node_prop_add)
      }else{
        cat("\nERROR: The following property, ",wb_prop,", for the following node, ",wb_node, ", did not find a placement in the new template.", sep="")
        
        wb_node_prop_add$wb_change="not_placed"
        wb_node_prop_add$temp_node_new=NA
        wb_node_prop_not_placements_df=rbind(wb_node_prop_not_placements_df,wb_node_prop_add)
      }
    }
  }
}



#Write out of data frame to explain what was placed 
log_table_out=kable(wb_node_prop_not_placements_df)

cat("\n\nThe following table is an output of workbook (wb) nodes and properties that were moved to a different node in the template (temp) or did not find a place in the new template.",
    "-----------\n",
    log_table_out,
    sep = "\n")


sink()

###############
#
# Write out
#
###############

#Write out file

wb=openxlsx::loadWorkbook(file = template_path)

cat("\n\nWriting out the UpdateR file.\n")

#progress bar
pb=txtProgressBar(min=0,max=length(nodes_present),style = 3)
x=0

for (node in nodes_present_temp){
  x=x+1
  setTxtProgressBar(pb,x)
  df=workbook_list_temp[node][[1]]
  openxlsx::deleteData(wb, sheet = node,rows = 1:(dim(df)[1]+1),cols=1:(dim(df)[2]+1),gridExpand = TRUE)
  openxlsx::writeData(wb=wb, sheet=node, df)
  openxlsx::saveWorkbook(wb = wb,file = paste(path,output_file,".xlsx",sep = ""), overwrite = T)
}



cat(paste("\n\nProcess Complete.\n\nThe output file can be found here: ",path,"\n\n",sep = "")) 
