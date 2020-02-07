library(tidyverse)


# util --------------------------------------------------------------------

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
All.ARVs <- get_model_export("t:/dcj/mops/link_user_control.csv")
All.Nodes <- get_model_export("t:/dcj/mops/link_node.csv")
All.Outfalls <- All.Nodes %>% filter(toupper(node_type)=="OUTFALL")
All.Subcatchments <- get_model_export("t:/dcj/mops/link_subcatchment.csv")


# Build Link Table --------------------------------------------------------
DS <- NULL

Outfall <- All.Outfalls$node_id

Pump.US <- All.Pumps$us_node_id
Pump.DS <- All.Pumps$ds_node_id

Link.US <- All.Links$us_node_id
Link.DS <- All.Links$ds_node_id

Link.IsPump <- !(is.na(match(Link.US,Pump.DS)))
Link.IsOutfall <- !(is.na(match(Link.DS,Outfall)))

Link <- cbind.data.frame(Link.US,Link.DS,Link.IsPump,Link.IsOutfall, rep(NA, length(Link.DS)), stringsAsFactors = FALSE)
colnames(Link) <- c("US", "DS", "IsPump", "IsOutfall", "DSPump")

# Add ARV to Link Table
ARV.US <- All.ARVs$us_node_id
ARV.DS <- All.ARVs$ds_node_id

ARV <- cbind.data.frame(ARV.US,ARV.DS,rep(FALSE, length(ARV.DS)),rep(FALSE, length(ARV.DS)), rep(NA, length(ARV.DS)), stringsAsFactors = FALSE)
colnames(ARV) <- c("US", "DS", "IsPump", "IsOutfall", "DSPump")

Link <- rbind.data.frame(Link, ARV, stringsAsFactors = FALSE)

# Build Pump Trace --------------------------------------------------------

GetDS <- function(root, DS.Node) {
  #return information if recursion is greater than n iterations
  n <- 500
  e <- Cstack_info()
  if (as.numeric(e[4])>n) {print(c(root, DS.Node, e[4]))}
  
  index <- match(DS.Node, Link[,1])
  if (is.na(index)){
    Link[root,5] <<- DS
  }
  else {
    if (!((Link[index,3]) | (Link[index,4]))) {
      DS <<- as.character(Link[index,2])
      GetDS(root, DS)
    }
    else {
      Link[root,5] <<- DS
    }  
  }
  
}


for (i in 1:length(Link[,1])) {
  GetDS(i,Link[i,2])
}

#write.csv(Link, "export.csv")


# link to subcatchment ----------------------------------------------------


# Subcatchment connected to liftstations
# node_at_ls <- All.Subcatchments %>% 
#   inner_join(All.Nodes, by="node_id") %>% 
#   filter(str_detect(asset_id, "LS-*")) %>%
#   select(subcatchment_id, node_id, DSPump=node_id)
# 
# remain <- All.Subcatchments %>%
#   filter(!(subcatchment_id %in% node_at_ls$subcatchment_id))
# 
# link_at_us <- remain %>%
#   inner_join(Link, by=c("node_id"="US")) %>%
#   select(subcatchment_id, node_id, DSPump, asset_id)
# 
# remain <- remain %>% 
#   filter(!(subcatchment_id %in% link_at_us$subcatchment_id))
# 
# link_at_ds <- remain %>%
#   inner_join(Link, by=c("node_id"="DS"))
# 
# remain <- remain %>% 
#   filter(!(subcatchment_id %in% link_at_ds$subcatchment_id))
# 
# tmp <- bind_rows(node_at_ls, link_at_us, link_at_ds) %>%
#   left_join(All.Nodes, by=c("DSPump"="node_id")) %>%
#   filter(str_detect(asset_id, "LS-*"))
# 
# tmp %>% 
#   filter(wastewater_profile==1 & population>0) %>%
#   left_join(unit_flow_by_ls, by=c("asset_id"="ps")) %>%
#   filter(!is.na(profile)) %>%
#   select(subcatchment_id, profile) %>% count()
#   


try <- All.Subcatchments %>% 
  filter(wastewater_profile==1 & population > 0)

count(try)

node_at_ls <- try %>%
inner_join(All.Nodes, by="node_id") %>% 
  filter(str_detect(asset_id, "LS-*")) %>%
  mutate(DSPump=node_id)

remain <- try %>%
  filter(!(subcatchment_id %in% node_at_ls$subcatchment_id))

link_at_us <- remain %>%
  left_join(Link, by=c("node_id"="US"))

remain <- remain %>% 
  filter(!(subcatchment_id %in% link_at_us$subcatchment_id))

tmp <- bind_rows(node_at_ls, link_at_us)

tmp %>% 
  select(subcatchment_id, DSPump) %>%
  filter(!is.na(DSPump)) %>%
  distinct() %>%
  left_join(All.Nodes, by=c("DSPump"="node_id")) %>%
  filter(str_detect(asset_id, "LS-*")) %>%
  select(subcatchment_id, asset_id) %>%
  left_join(unit_flow_by_ls, by=c("asset_id"="ps")) %>%
  select(subcatchment_id, profile) %>%
  filter(!is.na(profile))


# 
# 
# 
# 
# 
# 
# # Subcatchment connected to outfalls
# All.Subcatchments %>% inner_join(All.Nodes, by="node_id") %>% filter(node_type=="Outfall") %>% count()
# 
# tmp <- left_join(All.Subcatchments, Link, by=c("node_id"="US"))
# 
# a <- Link %>% 
#   filter(IsPump==TRUE) %>% 
#   inner_join(
#       tmp %>% 
#       filter(is.na(DSPump))
#       ,by=c("DSPump"="node_id")
#   ) %>% 
#   distinct(subcatchment_id, DSPump) %>%
#   select(subcatchment_id, node_id=DSPump, DSPump=DSPump) %>%
#   right_join(tmp, by="subcatchment_id")
# #Union DSPump.x with DSPump.y
# 
# #save(tmp, file="./temp/tmp.RData")
# #load(file="./temp/tmp.RData")
#  
# inner_join(tmp, All.Nodes, by=c("DSPump"="node_id")) %>% count(asset_id) %>% View()
# 
# # Use pumps for join on NA fields instead of Link
# 
# All.Nodes %>% 
#   filter(str_detect(asset_id, "LS-*")) %>%
#   select(node_id, asset_id) %>%
#   right_join(tmp, by=c("node_id"="DSPump")) %>% View()
# 
# 
# 
# All.Nodes %>% filter(str_detect(asset_id, "LS-*")) %>% distinct(node_id, asset_id)
