library(shiny)
library(visNetwork)
library(dplyr)
library(igraph)
library(RColorBrewer)

shinyServer(
  function(input, output) {
    
    edges_banks <- read.table("edges_csv.csv", header=T, sep=",", fill = TRUE)
		
	from_x <- c(1:20)
	to_x <- c(1:20)
	edges_banks$order_day <- as.Date(edges_banks$order_day)
	
	edges_banks$from_coord_x <- as.numeric(edges_banks$from_coord_x)
	edges_banks$from_coord_y <- as.numeric(edges_banks$from_coord_y)
	edges_banks$to_coord_x <- as.numeric(edges_banks$to_coord_x)
	edges_banks$to_coord_y <- as.numeric(edges_banks$to_coord_y)
	
	edges_banks$from_fixed <- as.logical(edges_banks$from_fixed)
	edges_banks$to_fixed <- as.logical(edges_banks$to_fixed) 
	
	edges_banks$from_coord_x <- as.numeric(edges_banks$from_coord_x)
	edges_banks$from_coord_y <- as.numeric(edges_banks$from_coord_y)
	
	
	order_day_begin_x <- as.Date("2016-06-20")
	order_day_end_x <- as.Date("2016-06-22")
	color_n = c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837")

 ################################################################	!!!! begin  ###################

	order_day_b <- as.Date("2016-06-20")
	order_day_e <- as.Date("2016-06-20")
	from_list <- c(1:20)
  to_list <- c(1:20)
	perm <- c(1)
#	banks_gr <- c("Privat", "With State Participation", "Belonging to foreign banking groups", "Russian", "Group 1")
	
##### edges for graph		
	querly_edges <- data.frame (
	  edges_banks %>%
	    group_by(order_day,from,to,per5mnum) %>%
	    select(id, order_day, from, to, sum_transaction, from_Outstanding_DayBegin, from_DayChange_Outcome, from_DayChange_Income, from_Outstanding_DayEnd, from_DayChange_pr, per5mnum, per5mend) %>%
	    summarise(sum_transaction = sum(sum_transaction), 
	              from_Outstanding_DayBegin =last(from_Outstanding_DayBegin),
	              from_DayChange_Outcome = last(from_DayChange_Outcome),
	              from_DayChange_Income = last(from_DayChange_Income),
				  from_Outstanding_DayEnd = last(from_Outstanding_DayEnd),
	              from_DayChange_pr = last(from_DayChange_pr),
				  id = last(id),
				  per5mend = last(per5mend)
	    ) %>%
	    filter(order_day == order_day_b, 
			 #  order_day<=order_day_e, 
	           from %in% from_list, 
			   to %in% to_list,
			   per5mnum %in% perm
			   )
							)
	
##### nodes for graph					
	querly_nodes <- data.frame (
	    edges_banks %>%
	    group_by(order_day,from, from_name, from_group, per5mnum) %>%
	    select(id, order_day, from, from_name, from_group, from_Outstanding_DayBegin, from_Outstanding_DayEnd, from_DayChange_pr, per5mnum, per5mend, color_from, from_fixed, from_coord_x,	from_coord_y) %>%
	    summarise(from_Outstanding_DayBegin =last(from_Outstanding_DayBegin),
	              from_Outstanding_DayEnd = last(from_Outstanding_DayEnd),
				   from_DayChange_pr = last(from_DayChange_pr), 
				  color_from = last(color_from),
				  id = last(id),
				  per5mend = last(per5mend),
				  from_fixed = last(from_fixed), 
				  from_coord_x = last(from_coord_x),	
				  from_coord_y = last(from_coord_y)
	    ) %>%
	    filter(order_day == order_day_b, 
		#	   order_day<=order_day_e, 
	       from %in% from_list, 
			   per5mnum %in% perm
			   )
							)	


	querly_nodes2 <- data.frame (
	    edges_banks %>%
	    group_by(order_day, to, to_name, to_group, per5mnum) %>%
	    select(id, order_day, to, to_name, to_group, to_Outstanding_DayBegin, to_Outstanding_DayEnd, to_DayChange_pr, per5mnum, per5mend, color_to) %>%
	    summarise(to_Outstanding_DayBegin =last(to_Outstanding_DayBegin),
	              to_Outstanding_DayEnd = last(to_Outstanding_DayEnd),
				  to_DayChange_pr = last(to_DayChange_pr), 
				  color_to = last(color_to),
				  id = last(id),
				  per5mend = last(per5mend)
	    ) %>%
	    filter(order_day == order_day_b, 
		#	   order_day<=order_day_e, 
	           to %in% to_list, 
			   per5mnum %in% perm
			   )
							)								
							
	
	
	
	
	######### !!!!!!!!! node !!!!!!    ################################
	#	data <- reactive({
		
	#	order_day_b_obs_mod <- as.character(input$days)
	#  order_day_e_obs_mod <- as.character(input$days[2])
	#	from_list_obs_mod <- c(input$checkBank)
	#	to_list_obs_mod <- c(input$checkBank)
	#	perm_obs_mod <- as.character(input$perm)
	#	edges_banks$order_day <- as.Date(edges_banks$order_day)
	#	edges_banks$order_day <- as.character(edges_banks$order_day)
	
	
	
	order_day_b_obs_mod <- as.Date("2016-06-20")
	order_day_e_obs_mod <- as.Date("2016-06-20")
	from_list_obs_mod <- c(1:20)
	to_list_obs_mod <- c(1:20)
	perm_obs_mod <- c(1)
	
	querly_nodes_p1 <- data.frame (
	  edges_banks %>%
	    group_by(order_day, from, from_name, from_group, per5mnum) %>%
	    select(order_day, from, from_name, from_group, per5mnum,from_Outstanding_DayBegin, from_Outstanding_DayEnd, from_DayChange_pr,from_fixed, from_coord_x,from_coord_y, color_from, per5mend, order_time, id) %>%
	    summarise(from_Outstanding_DayBegin =last(from_Outstanding_DayBegin),
	              from_Outstanding_DayEnd = last(from_Outstanding_DayEnd),
	              from_DayChange_pr = last(from_DayChange_pr), 				  
	              per5mend = last(per5mend),
	              order_time = last(order_time),
	              from_fixed = last(from_fixed), 
	              from_coord_x = last(from_coord_x),	
	              from_coord_y = last(from_coord_y),
	              color_from = last(color_from),
	              id = last(id)
	    ) %>%
	    filter(order_day == order_day_b_obs_mod, 
	           from %in% from_list_obs_mod, 
	           per5mnum %in% perm_obs_mod
	    )
	)	
	
	#########################   part 2				
	querly_nodes_p2 <- data.frame (
	  edges_banks %>%
	    group_by(order_day, to, to_name, to_group, per5mnum) %>%
	    select(order_day, to, to_name, to_group,per5mnum, to_Outstanding_DayBegin, to_Outstanding_DayEnd, to_DayChange_pr,to_fixed, to_coord_x,to_coord_y, color_to, per5mend, order_time,id) %>%
	    summarise(to_Outstanding_DayBegin =last(to_Outstanding_DayBegin),
	              to_Outstanding_DayEnd = last(to_Outstanding_DayEnd),
	              to_DayChange_pr = last(to_DayChange_pr), 
	              per5mend = last(per5mend),
	              order_time = last(order_time),
	             to_fixed = last(to_fixed), 
	              to_coord_x = last(to_coord_x),	
	              to_coord_y = last(to_coord_y),
	              color_to = last(color_to),
	              id = last(id)
	    ) %>%
	    filter(order_day == order_day_b_obs_mod, 
	           #	   order_day<=order_day_e, 
	           to %in% to_list_obs_mod, 
	           per5mnum %in% perm_obs_mod
	    )
	)			
	
	names(querly_nodes_p1)[2] <- "node"                          # 1
	names(querly_nodes_p1)[3] <- "node_name"
	names(querly_nodes_p1)[4] <- "node_group"
	names(querly_nodes_p1)[6] <- "Outstanding_DayBegin"
	names(querly_nodes_p1)[7] <- "Outstanding_DayEnd"
	names(querly_nodes_p1)[8] <- "DayChange_pr"
	names(querly_nodes_p1)[11] <- "fixed"
	names(querly_nodes_p1)[12] <- "coord_x"
	names(querly_nodes_p1)[13] <- "coord_y"
	names(querly_nodes_p1)[14] <- "color"

	names(querly_nodes_p2)[2] <- "node"                          # 2
	names(querly_nodes_p2)[3] <- "node_name"
	names(querly_nodes_p2)[4] <- "node_group"
	names(querly_nodes_p2)[6] <- "Outstanding_DayBegin"
	names(querly_nodes_p2)[7] <- "Outstanding_DayEnd"
	names(querly_nodes_p2)[8] <- "DayChange_pr"
	names(querly_nodes_p2)[11] <- "fixed"
	names(querly_nodes_p2)[12] <- "coord_x"
	names(querly_nodes_p2)[13] <- "coord_y"
	names(querly_nodes_p2)[14] <- "color"
	
	querly_nodes_all_sum_3 <- rbind(querly_nodes_p1, querly_nodes_p2) # 1  + 2
	
	querly_nodes_all_uniq <- data.frame (
	  querly_nodes_all_sum_3  %>%
	    arrange(node, order_time) %>%
	    group_by(order_day, node, node_name, node_group, per5mnum) %>%
	    select(order_day, node, node_name, node_group, Outstanding_DayBegin, Outstanding_DayEnd, DayChange_pr, per5mnum, per5mend, order_time, fixed, coord_x,coord_y, color, id) %>%
	    summarise(Outstanding_DayBegin =last(Outstanding_DayBegin),
	              Outstanding_DayEnd = last(Outstanding_DayEnd),
	              DayChange_pr = last(DayChange_pr), 
	              per5mend = last(per5mend),
	              order_time = last(order_time),
	              fixed = last(fixed), 
	              coord_x = last(coord_x),	
	              coord_y = last(coord_y),
	              color = last(color),
	              id = last(id)
	    ))													
	#_____________!!!
	
	#	})
	
################################################################	!!!! end  ###################
   
   querly_nodes_all_uniq_tb <- reactive({
   
   order_day_b_obs_mod <- as.character(input$days)
 #   order_day_e_obs <- as.character(input$days[2])
    from_list_obs_mod <- c(input$checkBank)
    to_list_obs_mod <- c(input$checkBank)
    perm_obs_mod <- as.character(input$perm)
	
	
  querly_nodes_p1_tb <- data.frame (
	  edges_banks %>%
	    group_by(order_day, from, from_name, from_group, per5mnum) %>%
	    select(order_day, from, from_name, from_group, per5mnum,from_Outstanding_DayBegin, from_Outstanding_DayEnd, from_DayChange_pr,from_fixed, from_coord_x,from_coord_y, color_from, per5mend, order_time, id) %>%
	    summarise(from_Outstanding_DayBegin =last(from_Outstanding_DayBegin),
	              from_Outstanding_DayEnd = last(from_Outstanding_DayEnd),
	              from_DayChange_pr = last(from_DayChange_pr), 				  
	              per5mend = last(per5mend),
	              order_time = last(order_time),
	              from_fixed = last(from_fixed), 
	              from_coord_x = last(from_coord_x),	
	              from_coord_y = last(from_coord_y),
	              color_from = last(color_from),
	              id = last(id)
	    ) %>%
	    filter(order_day == order_day_b_obs_mod, 
	           from %in% from_list_obs_mod, 
	           per5mnum %in% perm_obs_mod
	    )
	)	
	
	#########################   part 2				
	querly_nodes_p2_tb <- data.frame (
	  edges_banks %>%
	    group_by(order_day, to, to_name, to_group, per5mnum) %>%
	    select(order_day, to, to_name, to_group,per5mnum, to_Outstanding_DayBegin, to_Outstanding_DayEnd, to_DayChange_pr,to_fixed, to_coord_x,to_coord_y, color_to, per5mend, order_time,id) %>%
	    summarise(to_Outstanding_DayBegin =last(to_Outstanding_DayBegin),
	              to_Outstanding_DayEnd = last(to_Outstanding_DayEnd),
	              to_DayChange_pr = last(to_DayChange_pr), 
	              per5mend = last(per5mend),
	              order_time = last(order_time),
	             to_fixed = last(to_fixed), 
	              to_coord_x = last(to_coord_x),	
	              to_coord_y = last(to_coord_y),
	              color_to = last(color_to),
	              id = last(id)
	    ) %>%
	    filter(order_day == order_day_b_obs_mod, 
	           #	   order_day<=order_day_e, 
	           to %in% to_list_obs_mod, 
	           per5mnum %in% perm_obs_mod
	    )
	)			
	
	names(querly_nodes_p1_tb)[2] <- "node"                          # 1
	names(querly_nodes_p1_tb)[3] <- "node_name"
	names(querly_nodes_p1_tb)[4] <- "node_group"
	names(querly_nodes_p1_tb)[6] <- "Outstanding_DayBegin"
	names(querly_nodes_p1_tb)[7] <- "Outstanding_DayEnd"
	names(querly_nodes_p1_tb)[8] <- "DayChange_pr"
	names(querly_nodes_p1_tb)[11] <- "fixed"
	names(querly_nodes_p1_tb)[12] <- "coord_x"
	names(querly_nodes_p1_tb)[13] <- "coord_y"
	names(querly_nodes_p1_tb)[14] <- "color"

	names(querly_nodes_p2_tb)[2] <- "node"                          # 2
	names(querly_nodes_p2_tb)[3] <- "node_name"
	names(querly_nodes_p2_tb)[4] <- "node_group"
	names(querly_nodes_p2_tb)[6] <- "Outstanding_DayBegin"
	names(querly_nodes_p2_tb)[7] <- "Outstanding_DayEnd"
	names(querly_nodes_p2_tb)[8] <- "DayChange_pr"
	names(querly_nodes_p2_tb)[11] <- "fixed"
	names(querly_nodes_p2_tb)[12] <- "coord_x"
	names(querly_nodes_p2_tb)[13] <- "coord_y"
	names(querly_nodes_p2_tb)[13] <- "coord_y"
	names(querly_nodes_p2_tb)[14] <- "color"
	
	querly_nodes_all_sum_3_tb <- rbind(querly_nodes_p1_tb, querly_nodes_p2_tb) # 1  + 2


  querly_nodes_all_uniq_tb <- data.frame (
	  querly_nodes_all_sum_3_tb  %>%
	    arrange(node, order_time) %>%
	    group_by(order_day, node, node_name, node_group, per5mnum) %>%
	    select(order_day, node, node_name, node_group, Outstanding_DayBegin, Outstanding_DayEnd, DayChange_pr, per5mnum, per5mend, order_time, fixed, coord_x,coord_y, color, id) %>%
	    summarise(Outstanding_DayBegin =last(Outstanding_DayBegin),
	              Outstanding_DayEnd = last(Outstanding_DayEnd),
	              DayChange_pr = last(DayChange_pr), 
	              per5mend = last(per5mend),
	              order_time = last(order_time),
	              fixed = last(fixed), 
	              coord_x = last(coord_x),	
	              coord_y = last(coord_y),
	              color = last(color),
	              id = last(id)
	    ))						
   
   
					})
   
   
   
dataup <- reactive({
	 
     nb <- length(unique(as.character(edges_banks$from)))
	 color_n = c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837")
	#запит на залишок у заданому проміжку часу

	 nodes <- data.frame(id = querly_nodes_all_uniq$node, 
						 label = paste(querly_nodes_all_uniq$node, querly_nodes_all_uniq$node_name),
                         value = querly_nodes_all_uniq$Outstanding_DayEnd,
				#		 group = querly_nodes_all_uniq$node_group,
						 x = querly_nodes_all_uniq$coord_x,
						 y = querly_nodes_all_uniq$coord_y,
	                     fixed = querly_nodes_all_uniq$fixed,
						 # physics = FALSE,
						 color.background = color_n[querly_nodes_all_uniq$color],
						 color.highlight.background = color_n[querly_nodes_all_uniq$color],
						 color.hover.background = color_n[querly_nodes_all_uniq$color],
						 title = paste0("<p>", querly_nodes_all_uniq$node_name, " id: ", querly_nodes_all_uniq$node,
						"<br>Група: ", querly_nodes_all_uniq$node_group ,
						"<br>На початок дня: ", querly_nodes_all_uniq$Outstanding_DayBegin, 
						"<br>Залишок: ", querly_nodes_all_uniq$Outstanding_DayEnd,
						"<br>Зміна: ", querly_nodes_all_uniq$DayChange_pr, 
						"<br> за станом на: ", querly_nodes_all_uniq$per5mend,  " (", querly_nodes_all_uniq$order_day, ") ",
						"<br> x: ", querly_nodes_all_uniq$coord_x, 
						    " y: ", querly_nodes_all_uniq$coord_y,
						 "</p>"),
						stringsAsFactors = FALSE) 

	 #запит на унікальні звязки у заданому проміжку часу  https://rpubs.com/mfwade/in-degree-centrality
	 
    edges <- data.frame(id = querly_edges$id,
                        from = querly_edges$from,
                         to = querly_edges$to,
                         value = querly_edges$sum_transaction, 
						 label = paste("Рух $", querly_edges$from, " -> ", querly_edges$to),
                         color = list("#2B7CE9"),
						 shadow = TRUE,
                         arrows =list(to = list(enabled = TRUE, scaleFactor = 2)),
                         title = paste0("Рух $ ", querly_edges$from, " -> ", querly_edges$to, 
						 "<br>Отримано з початку дня: ", querly_edges$from_DayChange_Income,
						 "<br>Переказано з початку дня: ", querly_edges$from_DayChange_Outcome, 
						 "<br> за станом на: ", querly_edges$per5mend, 
						 "<br> Обсяг операцій за 5 хв: ", querly_edges$sum_transaction))
 					  
  list(nodes = nodes, edges = edges)
})

output$network_vis <- renderVisNetwork({
  visNetwork(dataup()$nodes, dataup()$edges)  %>% 
 
    
   # visIgraphLayout() %>%
     #     visIgraphLayout(layout = "layout_in_circle") %>%
		  visOptions( highlightNearest = TRUE, nodesIdSelection = TRUE) %>%   #selectedBy = "group",
		  visInteraction(hover = TRUE, navigationButtons = TRUE) %>%
    visEdges(smooth = FALSE)%>%
	#	   visConfigure(enabled = TRUE)%>%

	#	  visPhysics(enabled = FALSE)
	#    visPhysics(stabilization = TRUE)#%>%
	    visPhysics(solver = "forceAtlas2Based")
})

# observe({
#    visNetworkProxy("network_proxy_nodes") %>%
#     visFocus(id = input$focus_n, scale = 4)
#  })

observe({

########################################     begin new data    ########################################
    order_day_b_obs <- as.character(input$days)
 #   order_day_e_obs <- as.character(input$days[2])
    from_list_obs <- c(input$checkBank)
    to_list_obs <- c(input$checkBank)
    perm_obs <- as.character(input$perm)
	color_n = c("#A50026", "#D73027", "#F46D43", "#FDAE61", "#FEE08B", "#FFFFBF", "#D9EF8B", "#A6D96A", "#66BD63", "#1A9850", "#006837")
	
	##### edges for obs graph	
	querly_edges_obs <- data.frame (
	  edges_banks %>%
	    group_by(order_day,from,to,per5mnum) %>%
	    select(id, order_day, from, to, sum_transaction, from_Outstanding_DayBegin, from_DayChange_Outcome, from_DayChange_Income, from_Outstanding_DayEnd, from_DayChange_pr, per5mnum, per5mend) %>%
	    summarise(sum_transaction = sum(sum_transaction), 
	              from_Outstanding_DayBegin =last(from_Outstanding_DayBegin),
	              from_DayChange_Outcome = last(from_DayChange_Outcome),
	              from_DayChange_Income = last(from_DayChange_Income),
				  from_Outstanding_DayEnd = last(from_Outstanding_DayEnd),
	              from_DayChange_pr = last(from_DayChange_pr),
				  id = last(id),
				  per5mend = last(per5mend)
	    ) %>%
	    filter(order_day == order_day_b_obs, 
		#	   order_day<=order_day_e_obs, 
	           from %in% from_list_obs, 
			   to %in% to_list_obs,
			   per5mnum %in% perm_obs
			   )
							)
							
##### nodes for obs graph					
		querly_nodes_obs <- data.frame (
	    edges_banks %>%
	    group_by(order_day,from, from_name, from_group, per5mnum) %>%
	    select(id, order_day, from, from_name, from_group, from_Outstanding_DayBegin, from_Outstanding_DayEnd, from_DayChange_pr, per5mnum, per5mend, color_from, from_fixed, from_coord_x, from_coord_y) %>%
	    summarise(from_Outstanding_DayBegin =last(from_Outstanding_DayBegin),
	              from_Outstanding_DayEnd = last(from_Outstanding_DayEnd),
				  from_DayChange_pr = last(from_DayChange_pr), 
				  color_from = last(color_from),
				  id = last(id),				  
				  per5mend = last(per5mend),
				  from_fixed = last(from_fixed), 
				  from_coord_x = last(from_coord_x),	
				  from_coord_y = last(from_coord_y)
	    ) %>%
	    filter(order_day == order_day_b_obs, 
		#	   order_day<=order_day_e_obs, 
	           from %in% from_list_obs,
			   per5mnum %in% perm_obs
			   )
							)	

##### nodes_2 for obs graph				 to	
	querly_nodes_obs2 <- data.frame (
	    edges_banks %>%
	    group_by(order_day, to, to_name, to_group, per5mnum) %>%
	    select(id, order_day, to, to_name, to_group, to_Outstanding_DayBegin, to_Outstanding_DayEnd, to_DayChange_pr, per5mnum, per5mend, color_to) %>%
	    summarise(to_Outstanding_DayBegin =last(to_Outstanding_DayBegin),
	              to_Outstanding_DayEnd = last(to_Outstanding_DayEnd),
				  to_DayChange_pr = last(to_DayChange_pr), 
				  color_to = last(color_to),
				  id = last(id),				  
				  per5mend = last(per5mend)
	    ) %>%
	    filter(order_day == order_day_b_obs, 
		#	   order_day<=order_day_e_obs, 
	           to %in% to_list_obs,
			   per5mnum %in% perm_obs
			   )
							)		


							
							
########################################     end new data   ########################################
    nodes <- data.frame(id = querly_nodes_obs$from, 
						  x = querly_nodes_obs$from_coord_x,
						  y = querly_nodes_obs$from_coord_y,
	                      fixed = querly_nodes_obs$from_fixed,
						  label = paste(querly_nodes_obs$from, querly_nodes_obs$from_name),
                 #        group = querly_nodes_obs$from_group,
						  value = querly_nodes_obs$from_Outstanding_DayEnd,
						  color.background = color_n[querly_nodes_obs$color_from],
						  color.highlight.background = color_n[querly_nodes_obs$color_from],
						  color.hover.background = color_n[querly_nodes_obs$color_from],
					   #  color = list(background = color_n[querly_nodes_obs$color_from], border = "#2B7CE9"),						  
						  title = paste0("<p>", querly_nodes_obs$from_name, " id: ", querly_nodes_obs$from,
						  "<br>Група: ", querly_nodes_obs$from_group ,
						  "<br>На початок дня: ", querly_nodes_obs$from_Outstanding_DayBegin, 
						  "<br>Залишок: ", querly_nodes_obs$from_Outstanding_DayEnd,
						  "<br>Зміна: ", querly_nodes_obs$from_DayChange_pr, 
						  "<br> за станом на: ", querly_nodes_obs$per5mend, " (", querly_nodes_obs$order_day, ") ", 
						  "<br> x: ", querly_nodes_obs$from_coord_x, 
						      " y: ", querly_nodes_obs$from_coord_y,
						  "</p>"), 
						   stringsAsFactors = FALSE) 

	
	 
    edges <- data.frame(id = querly_edges_obs$id,
                          from = querly_edges_obs$from,
                         to = querly_edges_obs$to,
                         value = querly_edges_obs$sum_transaction, 
						 label = paste("Рух $", querly_edges_obs$from, " -> ", querly_edges_obs$to),
						 color = list("#2B7CE9"),
						 shadow = TRUE,
                         arrows =list(to = list(enabled = TRUE, scaleFactor = 2)),
                         title = paste0("Рух $ ", querly_edges_obs$from, " -> ", querly_edges_obs$to, 
						 "<br>Отримано з початку дня: ", querly_edges_obs$from_DayChange_Income,
						 "<br>Переказано з початку дня: ", querly_edges_obs$from_DayChange_Outcome, 
						 "<br> за станом на: ", querly_edges_obs$per5mend, 
						 "<br> Обсяг операцій за 5 хв: ", querly_edges_obs$sum_transaction))

    visNetworkProxy("network_vis") %>%
	visRemoveEdges(id = c(1:20000)) %>%
	visUpdateNodes(nodes = nodes) %>%
	visUpdateEdges(edges = edges)
	
	  })
   
################ !!!!!!!!!  coords   var1
   
  vals <- reactiveValues(coords_base=NULL)
  output$c_base <- renderPrint( vals$coords_base )
  
  observe({
    invalidateLater(1000)
    visNetworkProxy("network_vis") %>% visGetPositions()
    vals$coords_base <- if (!is.null(input$network_vis_positions)) 
						    do.call(rbind, input$network_vis_positions)
		})

 
 #Filter
 info_q_e <- reactive({
    
	########################################     begin new data    ########################################   !!!   для таблиці
	
    order_day_b_obs <- as.character(input$days)
 #   order_day_e_obs <- as.character(input$days[2])
    from_list_obs <- c(input$checkBank)
    to_list_obs <- c(input$checkBank)
    perm_obs <- as.character(input$perm)

#	edges_banks$order_day <- as.Date(edges_banks$order_day)
	
	##### edges for obs graph	
	querly_edges_obs <- data.frame (
	  edges_banks %>%
	    group_by(order_day,from,to,per5mnum) %>%
	    select(order_day, from, to, sum_transaction, from_Outstanding_DayBegin, from_DayChange_Outcome, from_DayChange_Income, from_Outstanding_DayEnd, from_DayChange_pr, per5mend,from_fixed,from_coord_x,from_coord_y) %>%
	    summarise(sum_transaction = sum(sum_transaction), 
	              from_Outstanding_DayBegin =last(from_Outstanding_DayBegin),
	              from_DayChange_Outcome = last(from_DayChange_Outcome),
	              from_DayChange_Income = last(from_DayChange_Income),
				  from_Outstanding_DayEnd = last(from_Outstanding_DayEnd),
	              from_DayChange_pr = last(from_DayChange_pr),
				  per5mend = last(per5mend),
				  from_fixed = last(from_fixed), 
				  from_coord_x = last(from_coord_x),	
				  from_coord_y = last(from_coord_y)
	    ) %>%
	    filter(order_day == order_day_b_obs, 
			#   order_day<=order_day_e_obs, 
	           from %in% from_list_obs, 
			   to %in% to_list_obs,
			   per5mnum %in% perm_obs
			   )
							)  
  }) 
 info_q_n <- reactive({

	order_day_b_obs <- as.character(input$days)
#   order_day_e_obs <- as.character(input$days[2])
    from_list_obs <- c(input$checkBank)
    to_list_obs <- c(input$checkBank)
    perm_obs <- as.character(input$perm)
	
 
	##### nodes for obs graph					
	querly_nodes_obs <- data.frame (
	    edges_banks %>%
	    group_by(order_day,from, from_name, from_group, per5mnum) %>%
	    select(id, order_day, from, from_name, from_group, from_Outstanding_DayBegin, from_Outstanding_DayEnd, from_DayChange_pr, per5mnum, per5mend, color_from, from_fixed, from_coord_x, from_coord_y) %>%
	    summarise(from_Outstanding_DayBegin =last(from_Outstanding_DayBegin),
	              from_Outstanding_DayEnd = last(from_Outstanding_DayEnd),
				  from_DayChange_pr = last(from_DayChange_pr), 
				  color_from = last(color_from),
				  id = last(id),				  
				  per5mend = last(per5mend),
				  from_fixed = last(from_fixed), 
				  from_coord_x = last(from_coord_x),	
				  from_coord_y = last(from_coord_y)
	    ) %>%
	    filter(order_day == order_day_b_obs, 
		#	   order_day<=order_day_e_obs, 
	           from %in% from_list_obs,
			   per5mnum %in% perm_obs
			   )
							)												
########################################     end new data   ######################################## 
  })  
  
   info_q_nn <- reactive({

	order_day_b_obs2 <- as.character(input$days)
#   order_day_e_obs2 <- as.character(input$days[2])
    from_list_obs2 <- c(input$checkBank)
    to_list_obs2 <- c(input$checkBank)
    perm_obs2 <- as.character(input$perm)
	
#	edges_banks$order_day <- as.Date(edges_banks$order_day)
 
	##### nodes for obs graph					
	querly_nodes2 <- data.frame (
	    edges_banks %>%
	    group_by(order_day, to, to_name, to_group, per5mnum) %>%
	    select(id, order_day, to, to_name, to_group, to_Outstanding_DayBegin, to_Outstanding_DayEnd, to_DayChange_pr, per5mend) %>%
	    summarise(to_Outstanding_DayBegin =last(to_Outstanding_DayBegin),
	              to_Outstanding_DayEnd = last(to_Outstanding_DayEnd),
				  to_DayChange_pr = last(to_DayChange_pr), 
				  id = last(id),
				  per5mend = last(per5mend)
	    ) %>%
	    filter(order_day == order_day_b_obs2, 
		#	   order_day<=order_day_e, 
	           to %in% to_list_obs2, 
			   per5mnum %in% perm_obs2
			   )
							)														
########################################     end new data   ######################################## 
  })  
  
  
     info_q_nn_con <- reactive({

	order_day_b_obs <- as.character(input$days)
#   order_day_e_obs <- as.character(input$days[2])
    from_list_obs <- c(input$checkBank)
    to_list_obs <- c(input$checkBank)
    perm_obs <- as.character(input$perm)
	 
	order_day_b_obs2 <- as.character(input$days)
#   order_day_e_obs2 <- as.character(input$days[2])
    from_list_obs2 <- c(input$checkBank)
    to_list_obs2 <- c(input$checkBank)
    perm_obs2 <- as.character(input$perm)
	
#	edges_banks$order_day <- as.Date(edges_banks$order_day)
  
 
 #########################   part 1
 		querly_nodes_part1 <- data.frame (
	    edges_banks %>%
	    group_by(order_day,from, from_name, from_group, per5mend, per5mnum) %>%
	    select(order_day, from, from_name, from_group, from_Outstanding_DayBegin, from_Outstanding_DayEnd, from_DayChange_pr, per5mend, per5mnum) %>%
	    summarise(from_Outstanding_DayBegin =last(from_Outstanding_DayBegin),
	              from_Outstanding_DayEnd = last(from_Outstanding_DayEnd),
                  from_DayChange_pr = last(from_DayChange_pr), 				  
				  per5mend = last(per5mend)
	    ) %>%
	    filter(order_day == order_day_b_obs, 
		#	   order_day<=order_day_e_obs, 
	           from %in% from_list_obs, 
			   per5mnum %in% perm_obs
			   )
							)												
###############################
 
 
#########################   part 2				
	querly_nodes_part2 <- data.frame (
	    edges_banks %>%
	    group_by(order_day, to, to_name, to_group, per5mend, per5mnum) %>%
	    select(id, order_day, to, to_name, to_group, to_Outstanding_DayBegin, to_Outstanding_DayEnd, to_DayChange_pr, per5mend, per5mnum) %>%
	    summarise(to_Outstanding_DayBegin =last(to_Outstanding_DayBegin),
	              to_Outstanding_DayEnd = last(to_Outstanding_DayEnd),
				  to_DayChange_pr = last(to_DayChange_pr), 
				  id = last(id),
				  per5mend = last(per5mend)
	    ) %>%
	    filter(order_day == order_day_b_obs2, 
		#	   order_day<=order_day_e, 
	           to %in% to_list_obs2, 
			   per5mnum %in% perm_obs2
			   )
							)														
########################################   
 
   querly_nodes_all <- full_join(querly_nodes_part1, querly_nodes_part2, by.x = "from", by.y = "to")

 }) 

  
 output$volume_text <- renderText({
     perm_obs_t <- as.character(input$perm)

	##### edges for obs graph	
	querly_edges_obs_t <- data.frame (
	  edges_banks %>%
	    group_by(per5mnum) %>%
	    select(per5mnum, per5mbeg, per5mend) %>%
	    summarise(
				  per5mbeg = last(per5mbeg),
				  per5mend = last(per5mend)
				  ) %>%
	    filter(per5mnum %in% perm_obs_t)
							)
		
    paste0(querly_edges_obs_t$per5mbeg, " - ", querly_edges_obs_t$per5mend)
  })
  
  
###############################  !!!!!   full_join 
   info_q_n_mod <- reactive({

	order_day_b_obs_mod <- as.character(input$days)
 #  order_day_e_obs_mod <- as.character(input$days[2])
    from_list_obs_mod <- c(input$checkBank)
    to_list_obs_mod <- c(input$checkBank)
    perm_obs_mod <- as.character(input$perm)
#	edges_banks$order_day <- as.Date(edges_banks$order_day)
 
 
 edges_banks$order_day <- as.character(edges_banks$order_day)
 
 
querly_nodes_part1 <- data.frame (
  edges_banks %>%
    group_by(order_day, from, from_name, from_group, per5mnum) %>%
    select(order_day, from, from_name, from_group, from_Outstanding_DayBegin, from_Outstanding_DayEnd, from_DayChange_pr, per5mnum,per5mend, order_time) %>%
    summarise(from_Outstanding_DayBegin =last(from_Outstanding_DayBegin),
              from_Outstanding_DayEnd = last(from_Outstanding_DayEnd),
              from_DayChange_pr = last(from_DayChange_pr), 				  
              per5mend = last(per5mend),
              order_time = last(order_time)
    ) %>%
    filter(order_day == order_day_b_obs_mod, 
          from %in% from_list_obs_mod, 
          per5mnum %in% perm_obs_mod
    )
)	

#########################   part 2				
querly_nodes_part2 <- data.frame (
  edges_banks %>%
    group_by(order_day, to, to_name, to_group, per5mnum) %>%
    select(order_day, to, to_name, to_group, to_Outstanding_DayBegin, to_Outstanding_DayEnd, to_DayChange_pr, per5mnum,per5mend, order_time) %>%
    summarise(to_Outstanding_DayBegin =last(to_Outstanding_DayBegin),
              to_Outstanding_DayEnd = last(to_Outstanding_DayEnd),
              to_DayChange_pr = last(to_DayChange_pr), 
              per5mend = last(per5mend),
              order_time = last(order_time)
               
    ) %>%
    filter(order_day == order_day_b_obs_mod, 
           #	   order_day<=order_day_e, 
           to %in% to_list_obs_mod, 
           per5mnum %in% perm_obs_mod
    )
)			



											
########################################   
names(querly_nodes_part1)[2] <- "node"
names(querly_nodes_part1)[3] <- "node_name"
names(querly_nodes_part1)[4] <- "node_group"
names(querly_nodes_part1)[6] <- "Outstanding_DayBegin"
names(querly_nodes_part1)[7] <- "Outstanding_DayEnd"
names(querly_nodes_part1)[8] <- "DayChange_pr"

names(querly_nodes_part2)[2] <- "node"
names(querly_nodes_part2)[3] <- "node_name"
names(querly_nodes_part2)[4] <- "node_group"
names(querly_nodes_part2)[6] <- "Outstanding_DayBegin"
names(querly_nodes_part2)[7] <- "Outstanding_DayEnd"
names(querly_nodes_part2)[8] <- "DayChange_pr"

querly_nodes_all3 <- rbind(querly_nodes_part1, querly_nodes_part2)

#_____________!!!
querly_nodes_all_uniq_ <- data.frame (
  querly_nodes_all3  %>%
    arrange(node, order_time) %>%
    group_by(order_day, node, node_name, node_group, per5mnum) %>%
    select(order_day, node, node_name, node_group, Outstanding_DayBegin, Outstanding_DayEnd, DayChange_pr, per5mnum, per5mend, order_time) %>%
    summarise(Outstanding_DayBegin =last(Outstanding_DayBegin),
              Outstanding_DayEnd = last(Outstanding_DayEnd),
              DayChange_pr = last(DayChange_pr), 
              per5mend = last(per5mend),
              order_time = last(order_time)
            ))													
#_____________!!!
							
########################################     end new data   ######################################## 
  }) 
  
  
  
output$info_q_n <- renderTable({info_q_n()})
output$info_q_nn <- renderTable({info_q_nn()}) 
#output$dataup <- renderTable({dataup()}) 

output$info_q_nn_con  <- renderTable({querly_nodes_all_uniq()}) 
output$info_q_e <- renderTable({info_q_e()})
output$info_q_n_mod <- renderTable({querly_nodes_all_uniq_tb()})    
# output$info_q_n = DT::renderDataTable(info_q_n, filter = 'top')
# output$info_q_e = DT::renderDataTable(info_q_e, filter = 'top')
  
  }) 
