setwd("/Users/apple/Box Sync/SNA Hillary")
list.files()


#################
###           ###
###  Hillary  ###
###           ###
#################
Aliases = read.csv('Aliases.csv')
EmailReceivers = read.csv('EmailReceivers.csv')
Emails = read.csv('Emails.csv')
Persons = read.csv('Persons.csv')

#getting table dimensions
dims = as.data.frame(rbind(

	t(c('Aliases', dim(Aliases))),

	t(c('Email Receivers', dim(EmailReceivers))),

	t(c('Emails', dim(Emails))),

	t(c('Persons', dim(Persons)))

))

names(dims) = c('Table Name', 'Rows', 'Columns')

print(dims)
###        Table Name Rows Columns
### 1         Aliases  850       3
### 2 Email Receivers 9306       3
### 3          Emails 7945      22
### 4         Persons  513       2

###################
###################
###             ### 
### Creat NODES ###
###             ### 
###################
###################### from "Persons": Id, Name [1:513]

	###################################
	#								  #
	# each person frequency as Sender #
	#								  #
	###################################
	sender.count = data.frame(table(Emails$SenderPersonId))
	## names(sender.count) [1] "Var1" "Freq"

	hillary_nodes = merge(Persons, sender.count, by.x="Id", by.y="Var1",all.x=T)
	
	
	hillary_nodes[is.na(hillary_nodes)] = 0  # NA: not as a sender

	names(hillary_nodes) = c("Id","Name","send_freq") 				#head(hillary_nodes)


	#####################################
	#								    #
	# each person frequency as Receiver #
	#								    #
	#####################################
	receiver.count = data.frame(table(EmailReceivers[,3]))
	## names(receiver.count) [1] "Var1" "Freq"

	hillary_nodes = merge(hillary_nodes, receiver.count, by.x="Id", by.y="Var1",all.x=T)

	hillary_nodes[is.na(hillary_nodes)] = 0 # NA: not as a receiver
	
	names(hillary_nodes) = c(names(hillary_nodes)[-4],"receive_freq") # head(hillary_nodes)

	
	#################################
	# 							    #
	# Node Size: activeness measure #
	# 							    #
	#################################
	# active_size = send_freq + receive_freq
	active_size = hillary_nodes[,3]+hillary_nodes[,4]
	
	
	
	##################################
	# 							     #
	# Node Type: @state.gov or other #
	# 							     #
	##################################
	type_id = rep(2,dim(Persons)[1]) ### type_id = 2 "@state gov" 
	hillary_nodes = data.frame(hillary_nodes,active_size,type_id)
	
	state.index = grepl("state",unlist(hillary_nodes[,2]),ignore.case=T)
	hillary_nodes$type_id[state.index==FALSE] = 1  ### other
	
		### 
		# > Persons[80,]
		#    Id            Name
		# 80 80 Hillary Clinton

		# > Persons[81,]
		#    Id        Name
		# 81 81 Huma Abedin

		# > Persons[87,]
		#    Id          Name
		# 87 87 Jake Sullivan

		# > Persons[32,]
		#    Id         Name
		# 32 32 Cheryl Mills

		table(hillary_nodes$type_id)
		#  1    2    
		#  356 157 


	########################
	# 			           #
	# Export Nodes as file #
	# 				       #
	########################
	h.nodes = data.frame(hillary_nodes$Id, hillary_nodes$Name, hillary_nodes$type_id, hillary_nodes$active_size)
	names(h.nodes) =c("id","name","person_type","active_size")
	# dim(h.nodes) 513 4 
	
	write.table(h.nodes,file="Hillary_nodes.csv",sep=",", col.names = NA, qmethod = "double")



###################
###             ### 
### Creat EDGES ###
###             ### 
###################
###################### from "EmailReceivers" 7945

	#######################
	# 			          #
	# Building From & To  #
	# 				      #
	#######################
	
	# names(Emails) # 'Id', 'SenderPersonId', 'ExtractedCc'
	hillary_edges = data.frame(Emails$Id, Emails$SenderPersonId, Emails$ExtractedCc)
	hillary_edges = merge(hillary_edges, EmailReceivers[-1], by.x="Emails.Id", by.y="EmailId")

	names(hillary_edges) = c("email_id", "sender_id","cc_list","receiver_id")
	
	
	##############################################
	# 			          						 #
	# Edge Size: by number of Receivers + Cc'ed  #
	# 				      						 #
	##############################################

	## Examing the Cc list per email
		## emails with NO Cc
		no.cc = nchar(as.character(hillary_edges$cc_list)) # value 0 --> no Cc recipient

		## emails with people Cc'ed and headcounts
		library(stringr)
		yes.cc = str_count(hillary_edges$cc_list,";") # gives the number of ";" between two recipients
		yes.cc =  yes.cc + 1 # no. of counts = no. of ";" + 1
		yes.cc[which(no.cc==0)] = 0  # excluding the ones with NO Cc which also have no ";"
		length(yes.cc)

		hillary_edges = data.frame(hillary_edges, yes.cc)
		head(hillary_edges)

	## Examing the Receiver list per email
		## emails with multiple receipients and headcounts
		no.receiver = data.frame(table(hillary_edges$email_id))
	
		hillary_edges = merge(hillary_edges, no.receiver, by.x="email_id",by.y="Var1")
	
		dim(hillary_edges) # 9306 6
	
		names(hillary_edges) = c(names(hillary_edges)[-6], "group.size")
		head(hillary_edges)
	
	
	## weight = 20 - no. of ppl Cc'ed - no. of Receiver
	weight = rep(20,dim(hillary_edges)[1])
	hillary_edges$weight = hillary_edges$weight  - hillary_edges$yes.cc - hillary_edges$group.size


	##########################################
	# 			          				     #
	# Edges Type: Sent/ Received by Hillary  #
	# 			          				     #
	##########################################

	type = rep(NA,dim(hillary_edges)[1])
	
	hillary_edges = data.frame(hillary_edges, weight,type)
	head(hillary_edges)

	hillary_edges$type[which(hillary_edges$sender_id=="80")] = "sent"
	hillary_edges$type[which(hillary_edges$sender_id!="80")] = "received"
	hillary_edges$type[which(hillary_edges$receiver_id=="80")] = "received"
	
			### sender of the following emails is "NA"
			hillary_edges$type[is.na(hillary_edges$type)] = "other"
			#     email_id sender_id                               cc_list receiver_id yes.cc group.size weight  type
			# 1820     1377        NA                                                79      0          1     20 other
			# 2995     2418        NA                                                18      0          1     20 other
			# 3117     2532        NA                                                32      0          1     20 other
			# 3172     2581        NA                                               195      0          1     20 other
			# 4640     3906        NA Muscatine, Lissa; S_SpecialAssistants          56      2          7     20 other
			# 4641     3906        NA Muscatine, Lissa; S_SpecialAssistants         168      2          7     20 other
			# 4642     3906        NA Muscatine, Lissa; S_SpecialAssistants          32      2          7     20 other
			# 4643     3906        NA Muscatine, Lissa; S_SpecialAssistants         427      2          7     20 other
			# 4644     3906        NA Muscatine, Lissa; S_SpecialAssistants          10      2          7     20 other
			# 4645     3906        NA Muscatine, Lissa; S_SpecialAssistants          87      2          7     20 other
			# 4646     3906        NA Muscatine, Lissa; S_SpecialAssistants         156      2          7     20 other
			# 4679     3935        NA                                               208      0          1     20 other
			# 6889     5891        NA                                               217      0          1     20 other


	
	
	########################
	# 			           #
	# Export Edges as file #
	# 				       #
	########################
	
	h.edges = data.frame(hillary_edges$sender_id, hillary_edges$receiver_id, hillary_edges$weight, hillary_edges$type)
	names(h.edges) = c("from","to", "weight","type")
	head(h.edges)
	dim(h.edges) # 9306  5
	
	write.table(h.edges,file="Hillary_edges.csv",sep=",", col.names = NA, qmethod = "double")



