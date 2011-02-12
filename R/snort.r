# SNORT package v0.1, 11 February 2011
# There are internal functions, utility functions and
#   external (callable) functions
#
# External functions.....
# e1: snort.generate_all
# e2: snort.generate_collapsed
# e3: snort.3dgraphics
#
# Internal functions.....
#  i1: snort.triple_store_input
#  i2: snort.triple_store_entity_register
#  i3: snort.triple_store_create_heterogeneous_links
#  i4: snort.triple_store_create_homogeneous_links_between_two_columns
#  i5: snort.triple_store_create_all_homogeneous_self_match_links
#  i6: snort.triple_store_create_homogeneous_cross_links
#  i7: snort.build_triple_store
#  i8: snort.entities to pajek
#  i9: snort.triples_to_pajek
# i10: snort.triplestore_to_pajek
# i11: snort.simple collapse
#
# Utility functions.....
# u1: augment_column_names.snort
# u2: value_counts.snort
# u3: fastcluster.snort
# u4: clustertables.snort
# u5: write_cluster_tables.snort

# Code begins.....

# External functions.....

# e1: snort.generate_all
# generate all function
snort.generate_all <- function(homedir, input1, input2, 
    logfile, entityfile, triplefile) {
    
    # set working directory.....
    homedir2 <- gsub("/", "//", c(homedir))
    setwd(homedir2)
    
    sink(logfile)
    
    # triplestore
    triplestore <- snort.build_triple_store(input1, input2)
    
    # entities to pajek
    snort.entities_to_pajek(triplestore[[1]], entityfile)
    
    # triplestore to pajek
    snort.triplestore_to_pajek(triplestore[[1]], triplestore[[2]], 
        triplefile)
    
    sink()
}

# e2: snort.generate_collapsed
# generate collapsed function
snort.generate_collapsed <- function(homedir, input1, 
    input2, logfile, entityfile, triplefile) {
    
    # set working directory.....
    homedir2 <- gsub("/", "//", c(homedir))
    setwd(homedir2)
    
    sink(logfile)
    
    # triplestore
    triplestore <- snort.build_triple_store(input1, input2)

    # collapsed
    TLL <- snort.triple_store_input(input1, input2)

    TLF <- fastcluster.snort(triplestore[[1]], triplestore[[2]])
    aaf <- TLF[[1]]
    clustab <- clustertables.snort(aaf, TLL)
    aaa <- clustab[[1]][[1]]
    write_cluster_tables.snort(clustab[[1]], clustab[[2]])
    SC <- snort.simple_collapse(triplestore[[1]], triplestore[[2]])
    
    # entities to pajek
    snort.entities_to_pajek(SC[[1]], entityfile)
    
    # triplestore to pajek
    snort.triplestore_to_pajek(SC[[1]], SC[[2]], triplefile)
    
    sink()
}

# e3: snort.3dgraphics
# three dimensional graphics function
snort.3dgraphics <- function(homedir, triplefile, 
    displabels, labelcolor, edgecolor, vertexcolor, backgrcolor) {
    
    # set working directory.....
    homedir2 <- gsub("/", "//", c(homedir))
    setwd(homedir2)
    
    # Attach sna and network libraries
    library(network)
    library(sna)
    
    # define concatenation operator
    `%+%` <- function(a, b) paste(a, b, sep = "/")
    
    # Pajek data source, fully specified
    pajekdata <- homedir2 %+% triplefile
    
    # Read in Pajek data
    test.net.1 <- read.paj(pajekdata)
    
    # Generate 3D plot
    gplot3d(test.net.1, displaylabels = displabels, label.col = labelcolor, 
        edge.col = edgecolor, vertex.col = vertexcolor, bg.col = backgrcolor)
}

# Internal functions.....

# i1: snort.triple_store_input
# triple_store_input function
snort.triple_store_input <- function(BasicInputTable, 
    TwoColumnTable = NULL) {
    
    # basic input routine, reads names, other column info from
    #   a basic input table.
    # to be extended to include multiple column, table
    #   homogeneous links
    
    # print(BasicInputTable)
    # print(TwoColumnTable)
    
    BasInTab <- read.csv(BasicInputTable, stringsAsFactors = F)
    
    if (!is.null(TwoColumnTable)) {
        TwoColTab <- read.csv(TwoColumnTable, stringsAsFactors = F)
        print("TwoColumn Input Table!")
    }
    
    Tablist <- list(NULL)
    Varlist <- list(NULL)
    TabNamelist <- list(NULL)
    j <- 1
    
    for (i in 1:nrow(BasInTab)) {
        if (BasInTab[i, 1] != "") {
            Tablist[[j]] <- read.csv(BasInTab[i, 1])
            TabNamelist[[j]] <- BasInTab[i, 1]
            j <- j + 1
            k <- 1
        }
        
        if (k == 1) 
            Varlist[[j - 1]] <- BasInTab[i, 2]
        else {
            
            Varlist[[j - 1]] <- c(Varlist[[j - 1]], BasInTab[i, 
                2])
            
        }
        k <- k + 1
    }
    
    print("first loop done")
    print("length(Tablist)")
    print(length(Tablist))
    print(dim(Tablist[[1]]))
    print(dim(Tablist[[2]]))
    # dim(Tablist[[3]])
    print(Varlist)
    print(TabNamelist)
    
    ColTabList <- list(NULL)
    
    for (i in 1:length(Tablist)) {
        print(i)
        print(Varlist[[i]])
        print(dimnames(Tablist[[i]])[[2]])
        print(dim(Tablist[[i]][, Varlist[[i]]]))
        ColTabList[[i]] <- Tablist[[i]][, Varlist[[i]]]
        print("done")
        
    }
    
    print("input complete")
    return(list(ColTabList, TabNamelist, Tablist, Varlist, TwoColTab))
    
}

# i2: snort.triple_store_entity_register
# triple_store_entity_register function
snort.triple_store_entity_register <- function(TableList, 
    TableNameList) {
    
    # returns full entity register in two modes: as list with
    #   one element per column (entity_register), and as single
    #   data frame (er)
    # creates a comprehensive list of all entities in all
    #   tables.
    # each entity has a unique name, a value, along with table,
    #   column, row identifiers and a type.
    # N.B. future additions: remove nulls
    
    regoffset <- 0
    k <- 1
    entity_register <- list(NULL)
    er <- NULL
    
    namevec <- c("Entity_Index", "Value", "Row_Number", "Column_Name", 
        "Table_Name", "Mode")
    for (i in 1:length(TableList)) {
        
        aaa <- TableList[[i]]
        print("TableList[[i]]")
        print(summary(TableList[[i]]))
        
        aaa <- TableNameList[[i]]
        print("TableNameList[[i]]")
        
        for (j in 1:ncol(TableList[[i]])) {
            indvec <- 1:nrow(TableList[[i]]) + regoffset
            
            entity_register[[k]] <- cbind(data.frame(indvec), 
                factor(TableList[[i]][, j]), 1:nrow(TableList[[i]]), 
                dimnames(TableList[[i]])[[2]][j], TableNameList[[i]], 
                is.factor(TableList[[i]][, j]))
            dimnames(entity_register[[k]]) <- list(indvec, namevec)
            if (k == 1) 
                er <- entity_register[[k]]
            else er <- rbind(er, entity_register[[k]])
            print("er")
            
            print("entity_register[[k]]")
            aaa <- entity_register[[k]]
            
            k <- k + 1
            regoffset <- regoffset + nrow(TableList[[i]])
        }
    }
    dimnames(er)[[2]] <- namevec
    
    return(list(entity_register, er))
    
}

# i3: snort.triple_store_create_heterogeneous_links
# triple_store_create_heterogeneous_links function
snort.triple_store_create_heterogeneous_links <- function(TableList, 
    TableNameList, EntityRegister) {
    
    # takes entire table list, list of table names and entity
    #   register. returns triple store component for
    #   heterogeneous links
    
    roffset <- 0
    tsmat <- NULL
    tslist <- list(NULL)
    
    # for all tables, for each pair of columns (excluding
    #   selfsame pairs)
    
    # iterate over all tables
    tablenames <- unique(EntityRegister[, 5])
    print(tablenames)
    l <- 1
    for (i in 1:length(tablenames)) {
        TabEntities <- EntityRegister[which(EntityRegister[, 
            5] == tablenames[i]), ]
        print("TabEntities")
        print(dim(TabEntities))
        
        # iterate over all pairs of columns in the table
        colnames <- unique(TabEntities[, 4])
        print(colnames)
        for (j in 1:length(colnames)) {
            for (k in j:length(colnames)) {
                # make sure this is not the same column repeated twice
                if (j != k) {
                  
                  Column1 <- augment_column_names.snort(TabEntities[which(TabEntities[, 
                    4] == colnames[j]), ], "1")
                  Column2 <- augment_column_names.snort(TabEntities[which(TabEntities[, 
                    4] == colnames[k]), ], "2")
                  
                  addcol <- "Heterogeneous_link"
                  names(addcol) <- "Link_type"
                  tsel <- cbind(Column1, Column2, addcol)
                  
                  print(paste(tablenames[i], colnames[j], colnames[k]))
                  tslist[[l]] <- tsel
                  if (is.null(tsmat)) 
                    tsmat <- tsel
                  else tsmat <- rbind(tsmat, tsel)
                  l <- l + 1
                }
            }
        }
    }
    
    return(list(tsmat, tslist))
    
}

# i4:
#   snort.triple_store_create_homogeneous_links_between_two_columns
# triple_store_create_homogeneous_links_between_two_columns
#   function
snort.triple_store_create_homogeneous_links_between_two_columns <- function(Column1, 
    Column2 = NULL, nonull = TRUE) {
    
    # takes entity register subsets for each column. Returns
    #   triple store matches on those columns.
    # single column match different from multiple columns:
    #   self-matching is excluded
    # if Column2 is NULL then single column match is assumed,
    #   and self-matches are handled appropriately.
    print("dim Column1")
    print(dim(Column1))
    if (is.null(Column2)) {
        self_match <- TRUE
        C1 <- value_counts.snort(Column1)
        print("dim C1")
        print(dim(C1))
        
        mode(C1[, "Count"])
        print("max C1")
        print(max(C1[, "Count"]))
        if (max(C1[, "Count"]) == 1) 
            return(NULL)
        Col1 <- C1[which(C1[, "Count"] > 1), 1:ncol(Column1)]
        print("with nulls")
        print(dim(Col1))
        
        print("dim Col1")
        print(dim(Col1))
        
        Col2 <- Col1
        Column2 <- Column1
    }
    else {
        self_match <- FALSE
        Col1 <- Column1
        Col2 <- Column2
    }
    
    if (nonull) {
        Col1 <- Col1[which(Col1[, 2] != "" & Col1[, 2] != " "), 
            ]
        Col2 <- Col2[which(Col2[, 2] != "" & Col2[, 2] != " "), 
            ]
        
        
        print("nonull")
        print(dim(Col1))
        print(dim(Col2))
    }
    
    # print('going into merge')
    mergemat <- merge(Col1, Col2, by = c("Value", "Value"))
    # print('merged')
    
    if (self_match == TRUE) 
        mergemat <- mergemat[which(mergemat[, "Row_Number.x"] < 
            mergemat[, "Row_Number.y"]), ]
    
    if (nrow(mergemat) == 0) 
        return(NULL)
    
    print(dim(mergemat))
    
    finalcol1 <- augment_column_names.snort(Column1[mergemat[, 
        "Row_Number.x"], ], "1")
    finalcol2 <- augment_column_names.snort(Column2[mergemat[, 
        "Row_Number.y"], ], "2")
    
    if (self_match == TRUE) 
        
    addcol <- "Homogeneous_link_self_match"
    
    if (self_match == FALSE) 
        addcol <- "Homogeneous_link_general"
    
    names(addcol) <- "Link_type"
    tsel <- cbind(finalcol1, finalcol2, addcol)
    
    return(tsel)
}

# i5:
#   snort.triple_store_create_all_homogeneous_self_match_links
# triple_store_create_all_homogeneous_self_match_links
#   function
snort.triple_store_create_all_homogeneous_self_match_links <- function(EntityRegister, 
    nonull = TRUE) {
    
    tablenames <- unique(EntityRegister[, 5])
    print(tablenames)
    l <- 1
    tsmat <- NULL
    for (i in 1:length(tablenames)) {
        TabEntities <- EntityRegister[which(EntityRegister[, 
            5] == tablenames[i]), ]
        print("TabEntities")
        print(dim(TabEntities))
        
        # iterate over all pairs of columns in the table
        colnames <- unique(TabEntities[, 4])
        print("colnames")
        print(colnames)
        for (j in 1:length(colnames)) {
            
            print(colnames[j])
            print(dim(tsmat))
            
            ColEntities <- TabEntities[which(TabEntities[, 4] == 
                colnames[j]), ]
            print(paste(i, j, colnames[j]))
            print(dim(ColEntities))
            tsel <- snort.triple_store_create_homogeneous_links_between_two_columns(ColEntities, 
                NULL, nonull)
            if (is.null(tsmat)) 
                tsmat <- tsel
            else tsmat <- rbind(tsmat, tsel)
            print(colnames[j])
            print(dim(tsmat))
            
        }
        
    }
    return(tsmat)
}

# i6: snort.triple_store_create_homogeneous_cross_links
# triple_store_create_homogeneous_cross_links function
snort.triple_store_create_homogeneous_cross_links <- function(EntityRegister, 
    CrossColMat = NULL, nonull = TRUE) {
    
    tsmat <- NULL
    for (i in 1:nrow(CrossColMat)) {
        TabEntities1 <- EntityRegister[which(EntityRegister[, 
            5] == CrossColMat[i, 1] & EntityRegister[, 4] == 
            CrossColMat[i, 2]), ]
        
        TabEntities2 <- EntityRegister[which(EntityRegister[, 
            5] == CrossColMat[i, 3] & EntityRegister[, 4] == 
            CrossColMat[i, 4]), ]
        
        tsel <- snort.triple_store_create_homogeneous_links_between_two_columns(TabEntities1, 
            TabEntities2, nonull)
        print("tsc run done. dim tsel:")
        print(dim(tsel))
        if (is.null(tsmat)) 
            tsmat <- tsel
        else tsmat <- rbind(tsmat, tsel)
        
    }
    print("dim tsmat on return")
    print(dim(tsmat))
    return(tsmat)
}

# i7: snort.build_triple_store
# build_triple_store function
snort.build_triple_store <- function(BasicInputTable, 
    TwoColumnTable, nonull) {
    TL <- snort.triple_store_input(BasicInputTable, TwoColumnTable)
    
    print("input complete")
    
    ER <- snort.triple_store_entity_register(TL[[1]], TL[[2]])
    
    print("ER complete")
    
    TS <- snort.triple_store_create_heterogeneous_links(TL[[1]], TL[[2]], 
        ER[[2]])
    
    print("hetr complete")
    
    tshomall <- snort.triple_store_create_all_homogeneous_self_match_links(ER[[2]])
    
    print("hom complete")
    
    tscross <- snort.triple_store_create_homogeneous_cross_links(ER[[2]], 
        TL[[5]])
    
    print("cross complete")
    
    entreg <- ER[[2]]
    tstore <- rbind(TS[[1]], tshomall, tscross)
    arcindex <- 1:(nrow(tstore))
    tstore <- cbind(arcindex, tstore)
    
    return(list(entreg, tstore))
    
}

# i8: snort.entities to pajek
# entities to pajek function
snort.entities_to_pajek <- function(entreg, filename, 
    colourscheme = 1) {
    
    # identify link types
    
    colours <- c("Yellow", "Red", "Blue", "Green", "Red", "White", 
        "Black")
    
    colvec <- unique(entreg[, c("Column_Name", "Table_Name")])
    clv <- nrow(colvec)
    noclv <- ceiling(clv/length(colours))
    repcol <- rep(colours, noclv)
    colourep <- repcol[1:clv]
    
    colvec <- cbind(colvec, colourep)
    
    exentreg <- merge(entreg, colvec, by.x = c("Column_Name", 
        "Table_Name"), by.y = c("Column_Name", "Table_Name"))
    
    entmat <- cbind(exentreg[, c("Entity_Index", "Value")], paste("ic", 
        exentreg[, "colourep"]))
    write(paste("*Vertices ", nrow(entmat)), filename, append = FALSE)
    write.table(entmat, file = filename, append = TRUE, sep = " ", 
        col.names = FALSE, row.names = FALSE, quote = 2)
}

# i9: snort.triples_to_pajek
# triples to pajek function
snort.triples_to_pajek <- function(triplestore, filename, 
    colourscheme = 1, app = TRUE) {
    
    colours <- c("c Yellow", "c Red", "c Blue", "c Green", "c Red", 
        "c White", "c Black")
    
    colvec <- unique(triplestore[, c("Column_Name1", "Table_Name1", 
        "Column_Name2", "Table_Name2")])
    clv <- nrow(colvec)
    noclv <- ceiling(clv/length(colours))
    repcol <- rep(colours, noclv)
    colourep <- repcol[1:clv]
    colvec <- cbind(colvec, colourep)
    
    extriples <- merge(triplestore, colvec, by.x = c("Column_Name1", 
        "Table_Name1", "Column_Name2", "Table_Name2"), by.y = c("Column_Name1", 
        "Table_Name1", "Column_Name2", "Table_Name2"))
    
    tripmat <- cbind(data.frame(as.integer(extriples[, "Entity_Index1"])), 
        as.integer(extriples[, "Entity_Index2"]), 1, extriples[, 
            "colourep"])
    # mode(tripmat[,1:3]) <- integer
    # factor(tripmat[,4])
    
    print(summary(tripmat))
    print(mode(tripmat[, 1]))
    print(mode(tripmat[, 2]))
    print(mode(tripmat[, 3]))
    print(mode(tripmat[, 4]))
    
    print(mode(tripmat))
    
    write(paste("*Arcs ", 1), filename, append = app)
    write.table(tripmat, file = filename, append = TRUE, sep = " ", 
        col.names = FALSE, row.names = FALSE, quote = 2)
}

# i10: snort.triplestore_to_pajek
# triplestore to pajek function
snort.triplestore_to_pajek <- function(entreg, triplestore, 
    filename, colourscheme = 1) {
    snort.entities_to_pajek(entreg, filename)
    snort.triples_to_pajek(triplestore, filename)
}

# i11: snort.simple collapse
# simple collapse function
snort.simple_collapse <- function(entreg, tripstor) {
    
    tripstornames <- dimnames(tripstor)[[2]]
    entregnames <- dimnames(entreg)[[2]]
    #maxentreg : unique list of values, with maximum entity
    #   index for each value
    maxentreg <- aggregate(entreg[, "Entity_Index"], list(entreg[, 
        "Value"]), max)
    dimnames(maxentreg)[[2]] <- c("Val", "entno")
    
    # copy the value column
    maxentreg <- cbind(maxentreg, maxentreg[, "Val"])
    
    # megereg: merge entity list with maxentreg on value
    mergereg <- merge(entreg, maxentreg, by.x = "Value", by.y = "Val")
    mergereg[, 2] <- mergereg[, 7]
    
    # newentreg: same as entreg but with entity list maximised
    newentreg <- mergereg[, entregnames]
    
    print(length(unique(mergereg[, "entno"])))
    print(dim(mergereg))
    print(dim(newentreg))
    print(dimnames(newentreg)[[2]])
    
    # merge triple store and list of values with maximal entity
    #   indexes
    
    mergetripstore <- merge(tripstor, newentreg, by.x = c("Value1", 
        "Row_Number1", "Column_Name1", "Table_Name1"), by.y = c("Value", 
        "Row_Number", "Column_Name", "Table_Name"))
    
    print(dimnames(mergetripstore)[[2]])
    # reassign maximised entity numbers to entity numbers for
    #   Val 1
    mergetripstore[, "Entity_Index1"] <- mergetripstore[, "Entity_Index"]
    
    # reduce mergetripstore to original columns of the
    #   triplestore in original order
    mergetripstore <- mergetripstore[, tripstornames]
    
    mergetripstore <- merge(mergetripstore, newentreg, by.x = c("Value2", 
        "Row_Number2", "Column_Name2", "Table_Name2"), by.y = c("Value", 
        "Row_Number", "Column_Name", "Table_Name"))
    print(dimnames(mergetripstore)[[2]])
    # reassign maximised entity numbers to entity numbers for
    #   Val 2
    mergetripstore[, "Entity_Index2"] <- mergetripstore[, "Entity_Index"]
    
    # reduce mergetripstore to original columns of the
    #   triplestore in original order
    mergetripstore <- mergetripstore[, tripstornames]
    
    newtripstore <- mergetripstore
    
    return(list(newentreg[order(newentreg[, "Entity_Index"]), 
        ], newtripstore))
}

# Utility functions.....

# u1: utility function - augment_column_names.snort
# augment_column_names function
augment_column_names.snort <- function(X, a) {
    colnam <- dimnames(X)[[2]]
    for (i in 1:length(colnam)) {
        colnam[i] <- paste(colnam[i], a, sep = "")
    }
    dimnames(X)[[2]] <- colnam
    return(X)
}

# u2: utility function - value_counts.snort
# value_counts function
value_counts.snort <- function(col1) {
    
    tab <- table(col1[, "Value"])
    #print('tab')
    print(paste(length(tab), max(tab)))
    #fix(col1)
    nametab <- data.frame(tab)
    dimnames(nametab)[[2]] <- c("Value", "Count")
    mergecol1 <- merge(nametab, col1, by = c("Value", "Value"))
    #print('mergecol')
    #print(dim(mergecol1))
    retcol <- mergecol1[, c(3, 1, 4:(ncol(col1) + 1), 2)]
    return(retcol)
    
}

# u3: utility function - fastcluster.snort
# fastcluster function
fastcluster.snort <- function(entreg, triplestore) {
    entreg2 <- cbind(entreg, 1:nrow(entreg))
    triplestore2 <- cbind(triplestore, 0)
    nce <- ncol(entreg2)
    nts <- ncol(triplestore2)
    
    repeat {
        E1ind <- triplestore2[, "Entity_Index1"]
        E2ind <- triplestore2[, "Entity_Index2"]
        E1 <- entreg2[E1ind, ]
        E2 <- entreg2[E2ind, ]
        
        compmax <- cbind(entreg2[E1ind, nce], entreg2[E2ind, 
            nce])
        
        cp <- cbind(compmax, compmax[, 1] != compmax[, 2])
        
        maxvec <- apply(compmax, 1, max)
        
        triplestore2[, nts] <- maxvec
        
        maxvecu <- aggregate(c(maxvec, maxvec), list(c(E1ind, 
            E2ind)), max)
        
        entreg2[maxvecu[, 1], nce] <- maxvecu[, 2]
        
        print(sum(cp[, 3]))
        if (sum(cp[, 3]) == 0) 
            break
    }
    return(list(entreg2, triplestore2))
}

# u4: utility function - clustertables.snort
# clustertables function
clustertables.snort <- function(entreg2, tablist) {
    retlist <- list(NULL)
    for (i in 1:length(tablist[[2]])) {
        entreg2i <- entreg2[which(entreg2[, "Table_Name"] == 
            tablist[[2]][i]), c("Row_Number", "1:nrow(entreg)")]
        maxvecu <- aggregate(entreg2i, list(index_row_number = entreg2i[, 
            "Row_Number"]), max)
        minvecu <- aggregate(entreg2i, list(entreg2i[, "Row_Number"]), 
            min)
        
        clusters <- cbind(tablist[[1]][[i]], 0)
        
        ncs <- ncol(clusters)
        
        clusters[maxvecu[, 2], ncs] <- maxvecu[, 3]
        clusters[, ncs + 1] <- clusters[, ncs]
        
        dimnames(clusters)[[2]][ncs + 1] <- "clusters"
        print(length(which(clusters[, ncs] == 0)))
        number_of_rows_in_cluster <- sort(table(clusters[, ncs]))
        print(max(number_of_rows_in_cluster))
        stl <- cbind(as.numeric(names(number_of_rows_in_cluster)), 
            number_of_rows_in_cluster)
        
        mclusters <- merge(clusters, stl, by.x = ncs, by.y = 1)
        
        omclusters <- mclusters[order(mclusters[, ncs + 2]), 
            ]
        
        retlist[[i]] <- omclusters
        
    }
    return(list(retlist, tablist[[2]]))
}

# u5: utility function - write_cluster_tables.snort
# write_cluster_tables function
write_cluster_tables.snort <- function(retlist, tablist) {
    
    for (i in 1:length(tablist)) {
        write.csv(retlist[[i]], paste("clust_", tablist[[i]], 
            sep = ""))
    }
    
}

# Code ends.....
