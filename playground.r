# Given a particulary locus_id plot that over the given circle


foo <- readRDS("dehalogenators.rds")
head(foo)
tail(foo)
foo <- foo[1:500,]
example_tags = c("DET_RS00015","DET_RS00005")
foo[['locus_tag']] %in% example_tags

foostring = "DET_RS00015"
unlist(strsplit(foostring , ","))


generate_locus_index <- function(df_in, column_value = "locus_tag", tag_list = NULL){
  if (!is.null(tag_list)){
    index <- df_in[[column_value]] %in% tag_list
    return(index)
  }
}

index = generate_locus_index(foo, tag_list = example_tags)
subset_df_by_row_ind(foo, index)  
}
# subset_df_by_row_ind <- function(df_in, ind){
#   if (length(ind) != dim(df_in)[1]){
#     stop("ind must be same length as df_in")
#   }
#   df_out <- df_in[ind,]
#   return(df_out)
# }

locus_tags <- unlist(strsplit("DET_RS00015", ","))

df_locus <- open_dataframe() #!!!! MAKE GENERAL
print(dim(df_locus))
print(head(df_locus))
print(length(df_locus$locus_tag))
print(df_locus$locus_tag %in% locus_tags)
print(length(df_locus$locus_tag %in% locus_tags))
df_locus_indexed <- df_locus[df_locus$locus_tag %in% locus_tags ,]
df_locus_indexed
