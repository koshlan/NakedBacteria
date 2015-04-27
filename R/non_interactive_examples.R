# non_interactive_examples.r
# Plot showing the genomic organization of reductive dehalogenase genes in the
# Dehalococcoides, Dehalobacter, Dehaligenimonas genera.

cookie_monster <- open_dataframe()
head(cookie_monster)
dput(levels(as.factor(cookie_monster$genome)))


c("NC_002936.3", "NC_007356.1", "NC_009455.1", "NC_013552.1", 
  "NC_013890.1", "NC_014314.1", "NC_018866.1", "NC_018867.1", "NC_020386.1", 
  "NC_020387.1", "NC_022964.1", "NZ_CP006949.1", "NZ_CP006950.1", 
  "NZ_CP006951.1", "NZ_CP007033.1")


# Dehalobacter sp. DCA RefSeq Genome NC_018866.1
# Dehalobacter sp. CF RefSeq Genome NC_018867.1
# Dehalobacter

Dehalbacter_genomes <- c("NC_018866.1", "NC_018867.1", 
  "NC_002936.3", "NC_007356.1", "NC_009455.1", "NC_013552.1", 
                         "NC_013890.1", "NC_014314.1", "NC_018866.1", "NC_018867.1", "NC_020386.1", 
                         "NC_020387.1", "NC_022964.1", "NZ_CP006949.1", "NZ_CP006950.1", 
                         "NZ_CP006951.1", "NZ_CP007033.1")
