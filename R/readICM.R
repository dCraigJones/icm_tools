get_model_export <- function(filename) {
  data <- read.csv(filename
                   , header=FALSE
                   , stringsAsFactors=FALSE
                   , skip=3
  )
  
  fields <- read.csv(filename
                     , header=FALSE
                     , stringsAsFactors=FALSE
                     , nrows=1
  )
  
  export <- as.data.frame(data)
  colnames(export) <- fields
  
  return(export)
}


# get model info ----------------------------------------------------------
All.Links <- get_model_export("t:/dcj/mops/link_conduit.csv")
All.Pumps <- get_model_export("t:/dcj/mops/link_pump.csv")
#All.Links <- get_model_export("./data-ignore/mops/link_conduit.csv")
tbl.Links <- All.Links %>% group_by(ObjectTable) %>% nest()
#All.Pumps <- get_model_export("./data-ignore/mops/link_pump.csv")
tbl.Pumps <- All.Pumps %>% group_by(ObjectTable) %>% nest()
export <- bind_rows(tbl.Links, tbl.Pumps)

All.ARVs <- get_model_export("t:/dcj/mops/link_user_control.csv")
All.Nodes <- get_model_export("t:/dcj/mops/link_node.csv")
All.Subcatchments <- get_model_export("t:/dcj/mops/link_subcatchment.csv")

All.Outfalls <- All.Nodes %>% filter(toupper(node_type)=="OUTFALL")
