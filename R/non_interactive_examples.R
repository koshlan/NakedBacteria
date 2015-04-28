# # non_interactive_examples.r
# # Plot showing the genomic organization of reductive dehalogenase genes in the
# # Dehalococcoides, Dehalobacter, Dehaligenimonas genera.
# 
# #cookie_monster <- open_dataframe()
# head(cookie_monster)
# dput(levels(as.factor(cookie_monster$genome)))
# # COMPLETE FINISHED DEHALOBACTER GENOMES
#   # Dehalobacter sp. DCA RefSeq Genome NC_018866.1
#   # Dehalobacter sp. CF RefSeq Genome NC_018867.1
#   # Dehalobacter sp. Restrictus NZ_CP007033.1
# # DRAFT GENOMES: IN CONTIG FORM
#   # Dehalobacter sp. UNSWDHB NZ_AUUR00000000.1
#   # Dehalobacter sp. E1 RefSeq Metagenome and RefSeq Genome NZ_CANE00000000.1
# finished_dehal0bacter_genomes <- c("NC_018866.1", "NC_018867.1", "NZ_CP007033.1")
# # Dehaligenimonas
#   # Dehalogenimonas lykanthroporepellens BL-DC-9, complete genome NC_014314 
# finished_dehalogenimonas_genomes <- c("NC_014314.1") 
# 
# finished_dehaloccoides_genomes <- c("NC_002936.3", "NC_007356.1", "NC_009455.1", "NC_013552.1", 
#              "NC_013890.1", "NC_020386.1", "NC_020387.1", "NC_022964.1", 
#              "NZ_CP006949.1", "NZ_CP006950.1", "NZ_CP006951.1")
# 
# # QUICK VERSION
catchment <- produce_genome_ggbio(genomes = finished_dehalobacter_genomes, 
                     keywords = c("dehalogenase", "hydrogenase", "integrase", "transposase"), 
                     size_bp = 3000000, 
                     track_width_call = 14, 
                     add_locus = FALSE, 
                     df_locus=NULL)
catchment[[1]]
catchment[[2]]

# # QUICK VERSION
catchment <- produce_genome_ggbio(genomes = finished_dehalogenimonas_genomes, 
                                  keywords = c("dehalogenase", "Ni/Fe hydrogenase", "integrase", "transposase"),
                                  size_bp = 1800000, 
                                  track_width_call = 14, 
                                  add_locus = FALSE, 
                                  df_locus=NULL)
catchment[[1]]
catchment[[2]]
# 
catchment <- produce_genome_ggbio(genomes = finished_dehaloccoides_genomes, 
                                  keywords = c("dehalogenase", "Ni/Fe hydrogenase", "integrase", "transposase"),
                                  size_bp = 1400000, 
                                  track_width_call = 14, 
                                  add_locus = FALSE, 
                                  df_locus=NULL)
catchment[[1]]
catchment[[2]]
# 
# multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
#   library(grid)
#   
#   # Make a list from the ... arguments and plotlist
#   plots <- c(list(...), plotlist)
#   
#   numPlots = length(plots)
#   
#   # If layout is NULL, then use 'cols' to determine layout
#   if (is.null(layout)) {
#     # Make the panel
#     # ncol: Number of columns of plots
#     # nrow: Number of rows needed, calculated from # of cols
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   }
#   
#   if (numPlots==1) {
#     print(plots[[1]])
#     
#   } else {
#     # Set up the page
#     grid.newpage()
#     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
#     
#     # Make each plot, in the correct location
#     for (i in 1:numPlots) {
#       # Get the i,j matrix positions of the regions that contain this subplot
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }
# multiplot(p3, p1, p2, cols=3)
# 
# ?multiplot# Here we carefully construct 
# 
# ### CAREFUL CONSTRUCTION OF EACH GRAPH TO CUSTOM SPECIFICATIONS
# 
bacteria <- open_dataframe()
finished_dehaloccoides_genomes <- c("NC_002936.3", "NC_007356.1", "NC_009455.1", "NC_013552.1", 
                                    "NC_013890.1", "NC_020386.1", "NC_020387.1", "NC_022964.1", 
                                    "NZ_CP006949.1", "NZ_CP006950.1", "NZ_CP006951.1")

# SUBSET TO ONLY DHC
dhc <- bacteria[bacteria$genome %in%  finished_dehaloccoides_genomes , ]
input_dataframe <- dhc
# GENERATE AND INDEX
dehalogenase <- generate_keyword_index(input_dataframe , column_value = "name", keyword = "dehalogenase", rejection_words = c("anchor", "haloacid"))
integrase <- generate_keyword_index(input_dataframe , column_value = "name", keyword = "integrase")
# SUBSET ONLY "dehalogenase" excluding unwanted words
d <- input_dataframe[dehalogenase, ]
d <- keyword_df(d, keyword = "1_rdhA") # ADD A KEYWORD 
i <- input_dataframe [integrase, ]
i <- keyword_df(i, keyword = "2_integrase")
D <- rbind(d,i)
size_bp = 1500000
my_ideogram <- make_ideogram(size_bp = size_bp)
my_pos <- make_gene_positions(D)
gg1 <- genome_ggbio(my_ideogram, my_pos, geom_call= "rect" , alpha = 1)


# SUBSET TO ONLY DHC
dhc <- bacteria[bacteria$genome %in%  finished_dehaloccoides_genomes , ]
input_dataframe <- dhc
# GENERATE AND INDEX
dehalogenase <- generate_keyword_index(input_dataframe , column_value = "name", keyword = "dehalogenase", rejection_words = c("anchor", "haloacid"))
integrase <- generate_keyword_index(input_dataframe , column_value = "name", keyword = "integrase")
# SUBSET ONLY "dehalogenase" excluding unwanted words
d <- input_dataframe[dehalogenase, ]
d <- keyword_df(d, keyword = "1_rdhA") # ADD A KEYWORD 
i <- input_dataframe [integrase, ]
i <- keyword_df(i, keyword = "2_integrase")
D <- rbind(d,i)
size_bp = 1500000
my_ideogram <- make_ideogram(size_bp = size_bp)
my_pos <- make_gene_positions(D)
p <- ggplot() + layout_circle(my_ideogram, geom = "ideo", fill = "gray70", radius = 30, trackWidth = 4)
p <- p + layout_circle(my_ideogram, geom = "scale", size = 5, radius = 35, trackWidth = 2)
p <- p + layout_circle(my_pos, geom = "rect", 
                       aes(y = bacteria, colour = keyword, fill = keyword, group = bacteria), 
                       alpha = 1 ,linetype = "blank" , radius = 14 , trackWidth = 16) + scale_size(range = c(1, 5))
gg1 <- p#genome_ggbio(my_ideogram, my_pos, geom_call= "rect" , alpha = 1)
gg1

# SUBSET TO ONLY Dehalobacter
finished_dehalobacter_genomes <- c("NC_018866.1", "NC_018867.1", "NZ_CP007033.1")
dhb <- bacteria[bacteria$genome %in%  finished_dehalobacter_genomes , ]
dhb
input_dataframe <- dhb
# GENERATE AND INDEX
dehalogenase <- generate_keyword_index(input_dataframe , column_value = "name", keyword = "dehalogenase", rejection_words = c("anchor", "haloacid"))
integrase <- generate_keyword_index(input_dataframe , column_value = "name", keyword = "integrase")
# SUBSET ONLY "dehalogenase" excluding unwanted words
d <- input_dataframe[dehalogenase, ]
d <- keyword_df(d, keyword = "1_rdhA") # ADD A KEYWORD 
i <- input_dataframe [integrase, ]
i <- keyword_df(i, keyword = "2_integrase")
D <- rbind(d,i)
D
size_bp = 3000000
my_ideogram <- make_ideogram(size_bp = size_bp)
my_pos <- make_gene_positions(D)
p <- ggplot() + layout_circle(my_ideogram, geom = "ideo", fill = "gray70", radius = 30, trackWidth = 4)
p <- p + layout_circle(my_ideogram, geom = "scale", size = 5, radius = 35, trackWidth = 2)
p <- p + layout_circle(my_pos, geom = "rect", 
                       aes(y = bacteria, colour = keyword, fill = keyword, group = bacteria), 
                       alpha = 1 ,linetype = "blank" , radius = 20 , trackWidth = 10) + scale_size(range = c(1, 5))


gg2 <- p # genome_ggbio(my_ideogram, my_pos, geom_call= "rect" , alpha = 1)
gg2
max(D$start)
gg2$mapping



# SUBSET TO ONLY Dehalogenimonas
finished_dehalogenimonas_genomes <- c("NC_014314.1") 
dhb <- bacteria[bacteria$genome %in%  finished_dehalogenimonas_genomes , ]
dhb
input_dataframe <- dhb
# GENERATE AND INDEX
dehalogenase <- generate_keyword_index(input_dataframe , column_value = "name", keyword = "dehalogenase", rejection_words = c("anchor", "haloacid"))
integrase <- generate_keyword_index(input_dataframe , column_value = "name", keyword = "integrase")
transposase <- generate_keyword_index(input_dataframe , column_value = "name", keyword = "transposase")
# SUBSET ONLY "dehalogenase" excluding unwanted words
d <- input_dataframe[dehalogenase, ]
d <- keyword_df(d, keyword = "1_rdhA") # ADD A KEYWORD 
i <- input_dataframe [integrase, ]
i <- keyword_df(i, keyword = "2_integrase")
t <- input_dataframe[transposase , ]
t
t <- keyword_df(t, keyword = "3_transposase")
D <- rbind(d,i,t)
D
size_bp = 1900000
my_ideogram <- make_ideogram(size_bp = size_bp)
my_pos <- make_gene_positions(D)


library(ggbio)
p <- ggplot() + layout_circle(my_ideogram, geom = "ideo", fill = "gray70", radius = 30, trackWidth = 4)
p <- p + layout_circle(my_ideogram, geom = "scale", size = 5, radius = 35, trackWidth = 2)
p <- p + layout_circle(my_pos, geom = "rect", 
                       aes(y = bacteria, colour = keyword, fill = keyword, group = bacteria), 
                       alpha = 1 ,linetype = "blank" , radius = 24 , trackWidth = 6) + 
  scale_size(range = c(1, 5))


gg3 <- p #genome_ggbio(my_ideogram, my_pos, geom_call= "rect" , alpha = 1, track_width_call = 10)
gg3
