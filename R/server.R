
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggplot2)
library(shiny)
library(ggbio)
library(GenomicRanges)

# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#' open dataframe
#' 
#' reads and rds file to a dataframe
#' 
#' @param rds_file 
#' @return df a data.frame 
open_dataframe <- function(rds_file = "dehalogenators.rds" ){
  df <-readRDS(rds_file)
  return(df)
}

#' generate keyword index
#'
#' index a dataframe by searching for a keyword in a column of a dataframe
#' 
#' @param df_in is the input data.frame
#' @param column_value is the name of the column to select
#' @param is a key word to search in the column
#'
generate_keyword_index <- function(df_in, column_value = "name", keyword = "dehalogenase"){
  index <- sapply(df_in[[column_value]], function(x) grepl(keyword,x,ignore.case = TRUE))  
  return(unname(index))
}

generate_locus_index <- function(df_locus, column_value = "locus_tag", tag_list = NULL){
  if (!is.null(tag_list)){
    index <- df_locus[[column_value]] %in% tag_list
    return(index)
  }
}

#' subset df by row ind 
#'
#' passes a logical index to subset df by row
#' 
#' @param df_in
#' @param ind
#' @return df_out
subset_df_by_row_ind <- function(df_in, ind){
  if (length(ind) != dim(df_in)[1]){
    stop("ind must be same length as df_in")
  }
  df_out <- df_in[ind,]
  return(df_out)
}

#' keyword df
#' 
#' adds a keyword to the dataframe as a new column
#' 
#' @param df_in
#' @return df_out
keyword_df <- function(df_in, keyword = NULL){
  if (!is.null(keyword)){
    df_in$keyword <- keyword
    df_out <- df_in
    return(df_out)
  }
  return(df_in)
}

keyword_by_locus <- function(df_in){
  df_in$keyword <- df_in$locus_tag
  return(df_in)
}

subset_and_keyword_df <- function(df, keyword = "dehalogenase", column_value = "name", append_keyword = FALSE){
  keyword_index <- generate_keyword_index(df, column_value = column_value, keyword = keyword)
  df1 <- subset_df_by_row_ind(df_in = df, ind=keyword_index)
  if (append_keyword){
    df1 <- keyword_df(df1, keyword)
  }
  return(df1)
}

consolidate_selection <-function(df, keywords = c("dehalogenase","^tRNA"),column_value = "name", append_keyword = FALSE){
  foo <- lapply(keywords, function(x) subset_and_keyword_df(df,keyword=x,column_value = column_value, append_keyword = append_keyword))
  bar <- Reduce(rbind, foo)
  return(bar)
}

make_ideogram <- function(size_bp = 1500000){
  require(GenomicRanges)
  GRanges(seqnames = c("chr1"), ranges= IRanges(start = 0, end = size_bp))
}

make_gene_positions <- function(df, size_bp = 1500000){
  require(GenomicRanges)
  my_IR <- IRanges(start = df$start, end =  df$end) 
  N = dim(df)[1]
  my_pos <- GRanges(seqnames = rep("chr1", N),
                    ranges= my_IR, 
                    strand = df$strand, 
                    genome = as.factor(df$genome), 
                    bacteria = unclass(as.factor(df$genome)),
                    keyword = df$keyword
                
  )
  seqlengths(my_pos) <- c(size_bp)
  return(my_pos)
}

make_locus_gene_positions <- function(df, size_bp = 1500000){
  require(GenomicRanges)
  my_IR <- IRanges(start = df$start, end =  df$end) 
  print(my_IR)
  N = dim(df)[1]
  print(N)
  my_pos <- GRanges(seqnames = rep("chr1", N),
                    ranges= my_IR, 
                    strand = df$strand, 
                    genome = as.factor(df$genome), 
                    bacteria = unclass(as.factor(df$genome)))
  seqlengths(my_pos) <- c(size_bp)
  return(my_pos)
}

genome_ggbio <- function(my_ideogram, my_pos, geom_call = "rect", alpha_call = 0.5, track_width_call = 14){
  library(ggbio)
  p <- ggplot() + layout_circle(my_ideogram, geom = "ideo", fill = "gray70", radius = 30, trackWidth = 4)
  p <- p + layout_circle(my_ideogram, geom = "scale", size = 2, radius = 35, trackWidth = 2)
  p <- p + layout_circle(my_pos, geom = geom_call, 
                         aes(y = bacteria, colour = keyword, fill = keyword, group = bacteria), 
                         alpha = alpha_call ,linetype = "blank" , radius = 16 , trackWidth = track_width_call) + 
    scale_size(range = c(1, 5))
  return(p)
} 



produce_genome_ggbio <- function(genomes, keywords, size_bp = 1500000, track_width_call = 14, add_locus = FALSE, df_locus=NULL){
  df <- open_dataframe()
  cs <- consolidate_selection(df, keywords = genomes, column_value = "genome", append_keyword = FALSE)
  cs <- consolidate_selection(cs, keywords = keywords, column_value = "name", append_keyword = TRUE)
  if (add_locus){
    cs <- rbind(cs,df_locus)
  }
  my_ideogram <- make_ideogram(size_bp = size_bp)
  my_pos      <- make_gene_positions(cs, size_bp = size_bp)
  gg <- genome_ggbio(my_ideogram = my_ideogram, my_pos = my_pos, geom_call = "point", alpha_call = 0.5, track_width_call = track_width_call)
  return(list(gg, cs))
}


shinyServer(function(input, output) {
  
  output$announce <- renderText({
    " 2015(c) made by Koshlan in California"
    })
  output$distPlot <- renderPlot({
    #print(input$Input1)
    genomes <-  input$genomes
    keywords <- input$keywords
    do_locus_tag <- FALSE
    if (input$locus_tags != ""){
      do_locus_tag = TRUE # SWITCH LOGIC DOWNBELOW
      locus_tags <- unlist(strsplit(input$locus_tags, ","))
      df_locus <- open_dataframe() #!!!! MAKE GENERAL
      df_locus <- df_locus[df_locus$locus_tag %in% locus_tags,]
      df_locus <- keyword_by_locus(df_locus) # Add locus_tags as there own keywords
    } else {
      df_locus <- open_dataframe() 
    }
  
    
    if (length(keywords) < 1 ){
      keywords = c("dehalogenase","^tRNA","^hydrogenase") # "^tRNA","^hydrogenase","vinyl chloride")
    }
    
    if (input$ad1 != ""){
      keywords <- c(keywords, input$ad1)
    }
    if (input$ad2 != ""){
      keywords <- c(keywords, input$ad2)
    }
    if (length(genomes) < 1){
      genomes <- c("NC_002936.3", "NC_007356.1", "NC_009455.1", "NC_013552.1", 
                   "NC_013890.1", "NC_020386.1", "NC_020387.1", "NC_022964.1", 
                   "NZ_CP006949.1", "NZ_CP006950.1", "NZ_CP006951.1")
    } 
    print(keywords)
    #??additional_keywords <- input$additional_keywords
    print(input$track_width)
    if (do_locus_tag){
      p_list <- produce_genome_ggbio(genomes, keywords, track_width_call = input$track_width, add_locus = TRUE, df_locus = df_locus )
    }else{
      p_list <- produce_genome_ggbio(genomes, keywords, track_width_call = input$track_width, add_locus = FALSE, df_locus = NULL)
    }
    p <- p_list[[1]]
    cs <- p_list[[2]]
    output$mytable <- renderDataTable({df_locus})
    output$mytable2 <- renderDataTable({cs})
    p
    #df = data.frame(a=c(1,3),b=c(2,5))
    #ggplot(df, aes(a,b)) + geom_point()
    # generate bins based on input$bins from ui.R
    #x    <- faithful[, 2]
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

})
