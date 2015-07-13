#' Merge cover values
#' 
#' Merge cover values within vegetation plots under the assumption of independent overlap or no overlap among plant taxa. Function can be used to merge cover values for a specific taxon and/or specific layers.
#
#' 
#' @param dataframe   a data frame in long format, with plot IDs and taxa names stored in two columns.
#' @param cover   name of column with cover values (numeric proportions in the range of 0-1).
#' @param plot   name of column with plot IDs. Column can be of type character, integer, numeric or factor. 
#' @param taxacol   name of column with taxa names. Column must be of type character or factor.
#' @param taxonname  name of taxon which cover values should be merged in a plot. 
#'        Default is "NULL": function merges cover values for identical taxa in a plot, across all layers they occur in.
#' @param layercol  name of column with layer information. Column can be of type character, integer, numeric or factor.
#' @param layernames  name(s) of layer(s) which cover values should be merged. Vector can be numeric, integer, factor or character.
#'        Default is "NULL": function merges cover values for all layers in which a species occurs.   
#' @param method  method for merging cover values. Current options are "independent" (default) and "sum". See details below.
#' 
#' 
#' @return Returns a new data frame in long format in which cover values for species within plots are merged and stored in a new column ("new.cover"). 
#'          The original columns 'cover' and 'layercol' will be replaced by new columns 'new.cov' and 'layer.new', the latter designating merged layers with "new_layer_veggie".
#' 
#' @details For method="independent", this function will merge cover values under the independence assumption (Tichy & Holt 2006, Fischer 2014),
#'          with the degree of overlap between plants following a normal distribution. By contrast, the 'sum' method does not assume any overlap among plants and simply sums up the merged cover values.
#'          
#'          Unless values are supplied for layernames or taxonname, the function will merge cover values for across all layers or across identical taxa, respectively. 
#'          By supplying values to these two arguments, the function can merge values at different levels of complexity. 
#'
#'          (1) In the simplest case (default options layernames=NULL, taxonname=NULL), the function will merge all cover values for identical taxa in a plot, across all layers they occur in.
#'
#'          (2a) If a character value is supplied for the taxonname argument but layernames==NULL, the function will merge all cover values for the specified taxon, irrespective of the layer it occurs in. 
#'          
#'          (2b) If values are supplied for the layersnames argument but taxonname=NULL, the function will merge all cover values of the same species for the respective layers. 
#'          
#'
#'          (3) The most complex scenario is when the users supply values for the layers and taxonname argument. In this case, the function will merge cover values only for the specified taxon and layers, in a plot.
#' @keywords vegetation data, data manipulation.
#' @examples veg.df <- data.frame(plot.ID=paste("P",c(1,1,1,1,1, 2,2,2,2,2,2,2),sep=""),
#'                    species.name=c("Spec1", "Spec1", "Spec1","Spec2", "Spec3","Spec1", "Spec1", "Spec5", "Spec4", "Spec6","Spec6", "Spec6"),
#'                    layer=c(6,5,4,6,6, 4,5,2,3,4,5,6),
#'                    abund=c(0.4,0.2,0.2,0.1,0.8,0.6,0.4,0.4,0.7,0.1,0.6,0.2),
#'                    region=c(rep("Region1",5), rep("Region2",7)))
#'
#' # Merge cover values for identical species within a plot, irrespective of the layer they occur in, 
#' # under the assumption of independent overlap
#'merge_cov(veg.df, cover="abund", plot="plot.ID", taxacol="species.name", layercol="layer")
#'
#' #...and under the assumption of no overlap
#' merge_cov(veg.df, cover="abund", plot="plot.ID", taxacol="species.name",layercol="layer",  method="sum")
#'
#'# Merge cover values for identical species within a plot, for layers "4" and "5"
#'# (assumption of independent overlap)
#' merge_cov(veg.df, plot="plot.ID", taxacol="species.name", 
#'          layercol="layer", layernames=c("4","5"), cover="abund")
#'
#'# Merge cover values for taxon "Spec1" and layer "4" and "5" within a plot
#'# (assumption of independent overlap)
#'merge_cov(veg.df, plot="plot.ID", taxacol="species.name",taxonname="Spec1", 
#'          layercol="layer", layernames=c("4", "5"), cover="abund")

#' @references  Fischer, H.S. 2014. "On the combination of species cover values from different vegetation layers". Applied Vegetation Science 18: 169-170.
#' 
#'              Tichý, L. & Holt, J. 2006. "JUICE program for management, analysis and classification of ecological data." Program manual. Masaryk University Brno, Czech Republic.
#' 
#' 
#' 
#' 


merge_cov <-  function(dataframe, cover, 
                       plot, 
                       taxacol, taxonname=NULL, 
                       layercol, layernames=NULL,
                       method="independent") 
{ 
  # Check whether dplyr and magrittr are installed
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("magrittr", quietly = TRUE)
  requireNamespace("lazyeval", quietly = TRUE)
  
  # Check whether data.frame name is supplied 
  if(is.null(dataframe)){
    stop("'data frame' not supplied")
  }
  
  if(is.null(cover)){
    stop("'cover' not supplied")
  }
  
  if(is.null(plot)){
    stop("'plot' not supplied")
  }
  
  if(is.null(dataframe)){
    stop("'taxacol' not supplied")
  }
  
  if(is.null(layercol)){
    stop("'layercol' not supplied")
  }
  
  # Check column names
  
  if(cover %in% colnames(dataframe)==FALSE){
    stop("'cover' must be a column in the specified dataframe")
  }
  
  if(plot %in% colnames(dataframe)==FALSE){
    stop("'plot' must be a column in the specified dataframe")
  }
  
  if(taxacol %in% colnames(dataframe)==FALSE){
    stop("'taxacol' must be a column in the specified dataframe")
  }
  
  if(layercol %in% colnames(dataframe)==FALSE){
    stop("'layercol' must be a column in the specified dataframe")
  }
  
  # Check whether data frame is a tbl_df() object
  # Coerce into data.frame for easier handling
  if("tbl_df"%in%class(dataframe)){
    dataframe <- as.data.frame(dataframe)
  }
  

  # Check for duplicate column x species x layer entries
  check.vec <- dataframe%>%
    dplyr::count_(list(plot,taxacol,layercol))%>%
    magrittr::use_series(n)%>%
    dplyr::n_distinct()
  check.vec
  
  if(any(check.vec > 1)){
    stop("Duplicate plot x taxacol x layercol entry in dataframe")
  }
  
  
# Check object types
   if(is.numeric(dataframe[[cover]]) ==FALSE){
     stop("'cover' must be numeric")
   }
  
  if((is.numeric(dataframe[[plot]])|is.character(dataframe[[plot]])|is.factor(dataframe[[plot]])|is.integer(dataframe[[plot]]))==FALSE){
    stop("'plot' must be of type character, integer, numeric or factor")
  }
  
  if((is.character(dataframe[[taxacol]])|is.factor(dataframe[[taxacol]]))==FALSE){
    stop("'taxacol' must be of type numeric, character or factor")
  }
  

  if(is.character(dataframe[[layercol]])==FALSE){
    message("Coercing 'layercol' to type character")
  }
  
  dataframe[,layercol] <- as.character(dataframe[,layercol])
  
  
  
  
  # Checking for vector type in taxonname and layernames
    
  if(is.numeric(taxonname)|is.integer(taxonname)){
      stop("'taxonname' must be of type character, factor or NULL")
    }

  if(length(taxonname)>1){
        stop("'taxonname' must be a single name")
  }
  

# Define dots for lazyevaluation
  
dots <- list(lazyeval::interp(~f(x,y),
                                f=as.name("%in%"),
                                x=as.name(layercol),
                                y=layernames))
  
dots1 <- list(lazyeval::interp(~f(x,y),
                                 f=as.name("%in%"),
                                 x=as.name(taxacol),
                                 y=taxonname))
  
dots2 <- setNames(list(lazyeval::interp(~ y,
                                          y="new_layer_veggie")),
                    "layer.new")
  
dots3 <- setNames(list(lazyeval::interp(~ y,
                                          y="new_taxon_veggie")),
                    "taxon.new")
  
dots4 <- setNames(list(lazyeval::interp(~ y,
                                          y=as.name(layercol))),
                    "layer.new")
  
  
dots5 <- setNames(list(lazyeval::interp(~ y,
                                           y=as.name(taxacol))),
                     "taxon.new")
                     
                     
dots6 <- setNames(list(lazyeval::interp(~1-dplyr::last(cumprod(1-x)), x=as.name(cover))),
                     "new.cov")
                     
dots7 <- setNames(list(lazyeval::interp(~sum(x), x=as.name(cover))),
                     "new.cov")  
  
  
  # 1. method="independent"   ----------------------------------------------------------------------------

  if(method=="independent"){
    # setNames function specifies the column to be calculated in summarize, as well as column names
    
    # 1.1 method="independent" & is.null(layernames) & is.null(taxonname)=TRUE ---- 
    #     DONE!
    if(is.null(layernames) & is.null(taxonname)){
   

    # Target rows
    tempi.1.1.0 <- dataframe %>% 
      dplyr::group_by_(plot, taxacol)%>%
      dplyr::filter_(~n()>1) 
    
    # new layer names
    tempi.1.1.1 <- tempi.1.1.0 %>%
      dplyr::mutate_(.dots=dots2)
    
    # original rows with new layer column
    tempi.1.1.2 <- dataframe %>%
      dplyr::anti_join(tempi.1.1.0)%>%
      dplyr::mutate_(.dots=dots4) %>%
      dplyr::bind_rows(tempi.1.1.1)

    # Operation
    tempi.1.1.3 <- tempi.1.1.1 %>%
      dplyr::group_by_(plot, taxacol) %>%
      dplyr::summarize_(.dots=dots6)%>%
      dplyr::ungroup()%>%
      dplyr::mutate_(.dots=dots2)
    

    tempi.1.1.4 <- tempi.1.1.2 %>%  
      dplyr::left_join(tempi.1.1.3, by=c(plot, taxacol, "layer.new")) %>%
      dplyr::mutate_(new.cov =lazyeval::interp(~ifelse(layer.new=="new_layer_veggie", new.cov, cover),
                         cover=as.name(cover))) %>%
      dplyr::group_by_(plot, taxacol,"layer.new", "new.cov") %>%
      dplyr::distinct()%>%
      dplyr::select_(lazyeval::interp(~-layercol, layercol=as.name(layercol)))%>%
      dplyr::select_(lazyeval::interp(~-cover, cover=as.name(cover)))%>%
      #dplyr::ungroup()%>%
      dplyr::arrange_(lazyeval::interp(~plot, plot=as.name(plot)))
    
    return(as.data.frame(tempi.1.1.4))
    
    }
    
    
    
    
    # 1.2 method="independent" & is.null(layernames)==FALSE & is.null(taxonname)=TRUE ---- 
    #     DONE!
    if(is.null(layernames) ==FALSE & is.null(taxonname)){
      
      # Filter out rows to be manipulated
      # original layer names
      tempi.1.2.0 <- dataframe %>% 
        dplyr::group_by_(plot, taxacol)%>%
        dplyr::filter_(.dots=dots)%>%
        dplyr::filter_(~n()>1) %>%
        dplyr::ungroup()
      
      # new layer names
      tempi.1.2.1 <- tempi.1.2.0 %>%
        dplyr::mutate_(.dots=dots2)
      
      # original rows with new layer column
      tempi.1.2.2 <- dataframe %>%
        dplyr::anti_join(tempi.1.2.0)%>%
        dplyr::mutate_(.dots=dots4) %>%
        dplyr::bind_rows(tempi.1.2.1)
      
 
      # Operation
      tempi.1.2.3 <- tempi.1.2.1 %>%
        dplyr::group_by_(plot, taxacol) %>%
        dplyr::summarize_(.dots=dots6)%>%
        dplyr::ungroup()%>%
        dplyr::mutate_(.dots=dots2)
      
      
      # Stitch dataframes together
      tempi.1.2.4 <-  tempi.1.2.2 %>%
        dplyr::left_join(tempi.1.2.3, 
                  by=c(plot, taxacol, "layer.new")) %>%
        dplyr::mutate_(new.cov =lazyeval::interp(~ifelse(layer.new=="new_layer_veggie", new.cov, cover),
                       layercol=as.name(layercol), cover=as.name(cover))) %>%
        dplyr::group_by_(plot, taxacol, "layer.new", "new.cov")%>%
        dplyr::distinct()%>%
        dplyr::select_(lazyeval::interp(~-layercol, layercol=as.name(layercol)))%>%
        dplyr::select_(lazyeval::interp(~-cover, cover=as.name(cover)))%>%
        dplyr::arrange_(lazyeval::interp(~plot, plot=as.name(plot)))
      
      return(as.data.frame(tempi.1.2.4))
      
    }

    
# 1.3 method="independent" & is.null(layernames) ==TRUE & is.null(taxonname)=FALSE ---- 
    if(is.null(layernames) & is.null(taxonname)==FALSE){

      
      # Filter out rows to be manipulated
      # original layer names

      tempi.1.3.0 <- dataframe %>% 
        dplyr::group_by_(plot, taxacol)%>%
        dplyr::filter_(.dots=dots1)%>%
        dplyr::filter_(~n()>1) %>%
        dplyr::ungroup()
      
      # new layer names
      tempi.1.3.1 <- tempi.1.3.0 %>%
        dplyr::mutate_(.dots=dots2)%>%
        dplyr::mutate_(.dots=dots3)
      
      # original rows with new layer column
      tempi.1.3.2 <- dataframe %>%
        # Remove target rows
        dplyr::anti_join(tempi.1.3.0) %>%
        dplyr::mutate_(.dots=dots4) %>%
        dplyr::mutate_(.dots=dots5) %>%
        # Add target rows back
        dplyr::bind_rows(tempi.1.3.1)
      
      
      # Operation
      tempi.1.3.3 <- tempi.1.3.1 %>%
        dplyr::group_by_(plot, taxacol) %>%
        #dplyr::filter_(~taxon.new=="new_taxon_veggie")%>%
        dplyr::summarize_(.dots=dots6)%>%
        dplyr::ungroup()%>%
        dplyr::mutate_(.dots=dots2)
        
        
        # Stitch dataframes together
        tempi.1.3.4 <-  tempi.1.3.2 %>%
        dplyr::left_join(tempi.1.3.3, 
                  by=c(plot, taxacol, "layer.new")) %>%
        dplyr::mutate_(new.cov =lazyeval::interp(~ifelse(layer.new=="new_layer_veggie", new.cov, cover),
                                cover=as.name(cover))) %>%
        dplyr::group_by_(plot, taxacol, "taxon.new", "new.cov")%>%
        dplyr::distinct()%>%
        dplyr::ungroup()%>%
        dplyr::select_(lazyeval::interp(~-layercol, layercol=as.name(layercol)))%>%
        dplyr::select_(lazyeval::interp(~-cover, cover=as.name(cover)))%>%
        dplyr::select_(lazyeval::interp(~-taxon.new))%>%
        dplyr::arrange_(lazyeval::interp(~plot, plot=as.name(plot)))

      
      return(as.data.frame(tempi.1.3.4))
      
    }   
    
    
    
    
# 1.4 method="independent" & is.null(layernames) ==FALSE & is.null(taxonname)=FALSE ---- 
 
    
    if(is.null(layernames) ==FALSE & is.null(taxonname)==FALSE){
      
      
      # Filter out rows to be manipulated
      # original layer names
      tempi.1.4.0 <- dataframe %>% 
        dplyr::group_by_(plot, taxacol)%>%
        dplyr::filter_(.dots=dots)%>%
        dplyr::filter_(.dots=dots1)%>%
        dplyr::filter_(~n()>1) %>%
        dplyr::ungroup()
      
      # new layer names
      tempi.1.4.1 <- tempi.1.4.0 %>%
        dplyr::mutate_(.dots=dots2)%>%
        dplyr::mutate_(.dots=dots3)
      
      # original rows with new layer column
      tempi.1.4.2 <- dataframe %>%
        # Remove target rows
        dplyr::anti_join(tempi.1.4.0) %>%
        dplyr::mutate_(.dots=dots4) %>%
        dplyr::mutate_(.dots=dots5) %>%
        # Add target rows back
        dplyr::bind_rows(tempi.1.4.1)
      
      
      # Operation
      tempi.1.4.3 <- tempi.1.4.1 %>%
        dplyr::group_by_(plot, taxacol) %>%
        #dplyr::filter_(~taxon.new=="new_taxon_veggie")%>%
        dplyr::summarize_(.dots=dots6)%>%
        dplyr::ungroup()%>%
        dplyr::mutate_(.dots=dots2)
      
      
      # Stitch dataframes together
      tempi.1.4.4 <-  tempi.1.4.2 %>%
        dplyr::left_join(tempi.1.4.3, 
                         by=c(plot, taxacol, "layer.new")) %>%
        dplyr::mutate_(new.cov =lazyeval::interp(~ifelse(layer.new=="new_layer_veggie", new.cov, cover),
                                                 cover=as.name(cover))) %>%
        dplyr::group_by_(plot, taxacol, "taxon.new", "new.cov")%>%
        dplyr::distinct()%>%
        dplyr::ungroup()%>%
        dplyr::select_(lazyeval::interp(~-layercol, layercol=as.name(layercol)))%>%
        dplyr::select_(lazyeval::interp(~-cover, cover=as.name(cover)))%>%
        dplyr::select_(lazyeval::interp(~-taxon.new))%>%
        dplyr::arrange_(lazyeval::interp(~plot, plot=as.name(plot)))
      
    return(as.data.frame(tempi.1.4.4))
      
      }
      
  }
  

 # 2. method="sum"   -------------------------------------------------------------------
  
  # method="sum" & is.null(layer)=TRUE
  if(method=="sum"){
    
    if(is.null(layernames) & is.null(taxonname)){ 
    # setNames function specifies the column to be calculated in summarize, as well as column names
    
      
      temps.1.1.0 <- dataframe %>% 
        dplyr::group_by_(plot, taxacol)%>%
        dplyr::filter_(~n()>1) 

      
      # new layer names
      temps.1.1.1 <- temps.1.1.0 %>%
        dplyr::mutate_(.dots=dots2)
      
      # original rows with new layer column
      temps.1.1.2 <- dataframe %>%
        dplyr::anti_join(temps.1.1.0)%>%
        dplyr::mutate_(.dots=dots4) %>%
        dplyr::bind_rows(temps.1.1.1)
      
      # Operation
      temps.1.1.3 <- temps.1.1.1 %>%
        dplyr::group_by_(plot, taxacol) %>%
        dplyr::summarize_(.dots=dots7)%>%
        dplyr::ungroup()%>%
        dplyr::mutate_(.dots=dots2)
      
      
      
      # Stitch dataframes together
      temps.1.1.4 <-  temps.1.1.2 %>%
        dplyr::left_join(temps.1.1.3, 
                         by=c(plot, taxacol, "layer.new")) %>%
        dplyr::mutate_(new.cov =lazyeval::interp(~ifelse(layer.new=="new_layer_veggie", new.cov, cover),
                                                 layercol=as.name(layercol), cover=as.name(cover))) %>%
        dplyr::group_by_(plot, taxacol, "layer.new", "new.cov")%>%
        dplyr::distinct()%>%
        dplyr::select_(lazyeval::interp(~-layercol, layercol=as.name(layercol)))%>%
        dplyr::select_(lazyeval::interp(~-cover, cover=as.name(cover)))%>%
        dplyr::arrange_(lazyeval::interp(~plot, plot=as.name(plot)))
      
      return(as.data.frame(temps.1.1.4))
      
      
      }
      
    
# 2.2 method="sum" & is.null(layernames) ==FALSE & is.null(taxonname)==TRUE ---- 
    
    
      if(is.null(layernames) ==FALSE & is.null(taxonname)){
        
        temps.1.2.0 <- dataframe %>% 
          dplyr::group_by_(plot, taxacol)%>%
          dplyr::filter_(.dots=dots)%>%
          dplyr::filter_(~n()>1) %>%
          dplyr::ungroup()
        
        # new layer names
        temps.1.2.1 <- temps.1.2.0 %>%
          dplyr::mutate_(.dots=dots2)
        
        # original rows with new layer column
        temps.1.2.2 <- dataframe %>%
          dplyr::anti_join(temps.1.2.0)%>%
          dplyr::mutate_(.dots=dots4) %>%
          dplyr::bind_rows(temps.1.2.1)
        
        
        
        # Operation
        temps.1.2.3 <- temps.1.2.1 %>%
          dplyr::group_by_(plot, taxacol) %>%
          dplyr::summarize_(.dots=dots7)%>%
          dplyr::ungroup()%>%
          dplyr::mutate_(.dots=dots2)
        
        
        
        # Stitch dataframes together
        temps.1.2.4 <-  temps.1.2.2 %>%
          dplyr::left_join(temps.1.2.3, 
                           by=c(plot, taxacol, "layer.new")) %>%
          dplyr::mutate_(new.cov =lazyeval::interp(~ifelse(layer.new=="new_layer_veggie", new.cov, cover),
                                                   layercol=as.name(layercol), cover=as.name(cover))) %>%
          dplyr::group_by_(plot, taxacol, "layer.new", "new.cov")%>%
          dplyr::distinct()%>%
          dplyr::select_(lazyeval::interp(~-layercol, layercol=as.name(layercol)))%>%
          dplyr::select_(lazyeval::interp(~-cover, cover=as.name(cover)))%>%
          dplyr::arrange_(lazyeval::interp(~plot, plot=as.name(plot)))
        
        return(as.data.frame(temps.1.2.4))
        
      }
    
    
# 2.3 method="sum" & is.null(layernames) ==TRUE & is.null(taxonname)=FALSE ---- 
    
    if(is.null(layernames) & is.null(taxonname)==FALSE){

        # Filter out rows to be manipulated
        # original layer names
        
        temps.1.3.0 <- dataframe %>% 
          dplyr::group_by_(plot, taxacol)%>%
          dplyr::filter_(.dots=dots1)%>%
          dplyr::filter_(~n()>1) %>%
          dplyr::ungroup()
        
        # new layer names
        temps.1.3.1 <- temps.1.3.0 %>%
          dplyr::mutate_(.dots=dots2)%>%
          dplyr::mutate_(.dots=dots3)
        
        # original rows with new layer column
        temps.1.3.2 <- dataframe %>%
          # Remove target rows
          dplyr::anti_join(temps.1.3.0) %>%
          dplyr::mutate_(.dots=dots4) %>%
          dplyr::mutate_(.dots=dots5) %>%
          # Add target rows back
          dplyr::bind_rows(temps.1.3.1)
        
        
        # Operation
        temps.1.3.3 <- temps.1.3.1 %>%
          dplyr::group_by_(plot, taxacol) %>%
          #dplyr::filter_(~taxon.new=="new_taxon_veggie")%>%
          dplyr::summarize_(.dots=dots7)%>%
          dplyr::ungroup()%>%
          dplyr::mutate_(.dots=dots2)
        
        
        # Stitch dataframes together
        temps.1.3.4 <-  temps.1.3.2 %>%
          dplyr::left_join(temps.1.3.3, 
                           by=c(plot, taxacol, "layer.new")) %>%
          dplyr::mutate_(new.cov =lazyeval::interp(~ifelse(layer.new=="new_layer_veggie", new.cov, cover),
                                                   cover=as.name(cover))) %>%
          dplyr::group_by_(plot, taxacol, "taxon.new", "new.cov")%>%
          dplyr::distinct()%>%
          dplyr::ungroup()%>%
          dplyr::select_(lazyeval::interp(~-layercol, layercol=as.name(layercol)))%>%
          dplyr::select_(lazyeval::interp(~-cover, cover=as.name(cover)))%>%
          dplyr::select_(lazyeval::interp(~-taxon.new))%>%
          dplyr::arrange_(lazyeval::interp(~plot, plot=as.name(plot)))
        
        
        return(as.data.frame(temps.1.3.4))
 
      } 
      
      
      
      
# 2.4 method="independent" & is.null(layernames) ==FALSE & is.null(taxonname)=FALSE ---- 
      
      if(is.null(layernames) ==FALSE & is.null(taxonname)==FALSE){
        
        
        # Filter out rows to be manipulated
        # original layer names
        temps.1.4.0 <- dataframe %>% 
          dplyr::group_by_(plot, taxacol)%>%
          dplyr::filter_(.dots=dots)%>%
          dplyr::filter_(.dots=dots1)%>%
          dplyr::filter_(~n()>1) %>%
          dplyr::ungroup()
        
        # new layer names
        temps.1.4.1 <- temps.1.4.0 %>%
          dplyr::mutate_(.dots=dots2)%>%
          dplyr::mutate_(.dots=dots3)
        
        # original rows with new layer column
        temps.1.4.2 <- dataframe %>%
          # Remove target rows
          dplyr::anti_join(temps.1.4.0) %>%
          dplyr::mutate_(.dots=dots4) %>%
          dplyr::mutate_(.dots=dots5) %>%
          # Add target rows back
          dplyr::bind_rows(temps.1.4.1)
        
        
        # Operation
        temps.1.4.3 <- temps.1.4.1 %>%
          dplyr::group_by_(plot, taxacol) %>%
          dplyr::summarize_(.dots=dots7)%>%
          dplyr::ungroup()%>%
          dplyr::mutate_(.dots=dots2)
        

        # Stitch dataframes together
        temps.1.4.4 <-  temps.1.4.2 %>%
          dplyr::left_join(temps.1.4.3, 
                           by=c(plot, taxacol, "layer.new")) %>%
          dplyr::mutate_(new.cov =lazyeval::interp(~ifelse(layer.new=="new_layer_veggie", new.cov, cover),
                                                   cover=as.name(cover))) %>%
          dplyr::group_by_(plot, taxacol, "taxon.new", "new.cov")%>%
          dplyr::distinct()%>%
          dplyr::ungroup()%>%
          dplyr::select_(lazyeval::interp(~-layercol, layercol=as.name(layercol)))%>%
          dplyr::select_(lazyeval::interp(~-cover, cover=as.name(cover)))%>%
          dplyr::select_(lazyeval::interp(~-taxon.new))%>%
          dplyr::arrange_(lazyeval::interp(~plot, plot=as.name(plot)))
        
        return(as.data.frame(temps.1.4.4))
      }  
  }

}


