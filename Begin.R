library(dplyr)
library(chron)
library(Hmisc)
library(lubridate)
library(party)
library(class) #knn algorithm
library(e1071) #svm
library(pROC) #Plotting ROC curve
library(gbm)
library(rpart.plot) #plotting rpart tree
library(Metrics)
library(randomForest)
library(caret)
library(data.table)
library(tidyr)
library(tm)
library(glmnet)
library(xgboost)
library(Matrix)

local_run = 0

cs = function(){for (i in 1:50)  cat("\014")}
arrange.vars <- function(data, vars){
  ##stop if not a data.frame (but should work for matrices as well)   
  stopifnot(is.data.frame(data))
  
  ##sort out inputs
  data.nms <- names(data)
  var.nr <- length(data.nms)
  var.nms <- names(vars)
  var.pos <- vars
  ##sanity checks
  stopifnot( !any(duplicated(var.nms)), 
             !any(duplicated(var.pos)) )
  stopifnot( is.character(var.nms), 
             is.numeric(var.pos) )
  stopifnot( all(var.nms %in% data.nms) )
  stopifnot( all(var.pos > 0), 
             all(var.pos <= var.nr) )
  
  ##prepare output
  out.vec <- character(var.nr)
  out.vec[var.pos] <- var.nms
  out.vec[-var.pos] <- data.nms[ !(data.nms %in% var.nms) ]
  stopifnot( length(out.vec)==var.nr )
  
  ##re-arrange vars by position
  data <- data[ , out.vec]
  return(data)
}

handle_outliers = function(X){
  
  k = 3
  IQ = (quantile(X , na.rm = TRUE , names = FALSE)[4] - quantile(X , na.rm = TRUE , names = FALSE)[2])
  fense = quantile(X , na.rm = TRUE , names = FALSE)[4] + k * IQ
  X = ifelse(X > fense , fense, X)
  
}

visualize = function(temp){
  qqnorm(sqrt(temp$price))
  qqline(sqrt(temp$price))
  hist((temp$price) , 
       main="Histogram for Price", 
       xlab="Price", 
       border="blue", 
       col="green",
       xlim=c(0,100),
       las=1, 
       breaks= seq(0,100,10))
  
  temp$price  = handle_outliers(temp$price)
  hist(sqrt(temp$price) , 
       main="Histogram for Price", 
       xlab="Price", 
       border="blue", 
       col="green",
       xlim=c(0,10),
       las=1, 
       breaks= 10)
  boxplot(temp$price~ temp$section , las = 2 ,at = c(1,2,3,4, 6,7,8,9, 11,12,13) , na.rm = TRUE )
  
  hist((temp$length_of_description) , 
       main="Histogram for Length of Description", 
       xlab="Length of Description", 
       border="blue", 
       col="green",
       xlim=c(0,500),
       las=1,
       breaks= 100)
  
  scatter.smooth(temp$price , temp$length_of_description)
  
  explore = temp[Data$section=='Men',]
  boxplot(explore$price~ explore$category , las = 2 , na.rm = TRUE )
  
}

analyze_subcategories = function(){
  temp1 = Data
  temp1$section = factor(temp1$section)
  for(sec in levels(temp1$section)){
    temp = Data[Data$section == sec ,]
    if(sec == '')
    {sec = 'Not available'}
    else
    {
      temp$sub_category1 = factor(temp$sub_category1)
      print(sec)
      d = data.frame(table(temp$section , temp$sub_category1))
      write.xlsx(d, file = "D:\\Analytics\\kaggle\\mercari\\explore\\subcategory.xls"  , row.names = FALSE ,sheetName = sec, append = TRUE)
      hist(d$Freq) }
  }
  d = table(Data$sub_category1)
  write.xlsx(d, file = "D:\\Analytics\\kaggle\\mercari\\explore\\subcategory.xls"  , row.names = FALSE ,sheetName = 'all', append = TRUE)
}


analyze_variables= function(Data){
  temp = Data  #[Data$section == 'Women',]
  
  
  dumping_model = xgb.dump(model)
  names <- dimnames(data.matrix(x))[[2]]
  importance_matrix <- xgb.importance(names, model)
  # Nice graph
  xgb.plot.importance(importance_matrix[1:20,])
  barplot(importance_matrix[,1])
  
}


perform_t.test = function(Data , variable , product_category){
  temp = Data[Data$category == product_category ,]
  if(nrow(temp) == 0 )
    return(0)
  factor_1 = temp[temp[,variable] == 1,'price']
  factor_0 = temp[temp[,variable] == 0,'price']
  
  # cat(sprintf(" \n\n\n                 Analysing the variable   : %s \n\n\n"  , variable))
  
  if(length(factor_1) <= 10 | length(factor_0) <= 10)
    return(0)
  if(skewness(factor_1) < -2.5 | skewness(factor_0) < -2.5)
    #  cat('beware left skewed')
    
    if(abs(skewness(factor_1)) > 2.5 | abs(skewness(factor_0)) > 2.5) 
    {
      #cat('Taking log transform because of skewness....\n\n')
      factor_1 = ifelse(is.finite(log(factor_1)), log(factor_1), 0)
      factor_0 = ifelse(is.finite(log(factor_0)), log(factor_0), 0)
      # cat('Skewness after taking the log transform....\n')
      # cat(abs(skewness(factor_1) ) ,'\n')
      # cat(abs(skewness(factor_0)) , '\n')
    }  
  
  
  f = var.test(factor_1 , factor_0 )
  t = t.test(factor_1 , factor_0 , var.equal = f$p.value > 0.001 , p.value = 0.05 )
  if(t$p.value > 0.05)
    return(0) 
  else
    return (1)
  
}    

one_time = function(Data , test){
  t.results = {}
  for(col in colnames(Data)[10:55]){
    t.results[col] = perform_t.test(Data,col , "Jeans" )
    
  }
  
  for(col in colnames(Data)[10:55]){
    if(t.results[col] == 0){
      Data[,col] = NULL
      test[,col] = NULL
    }
  }
}

select_top_sub_categories = function(Data ) {
  top_sub_categories <<- data.frame(table(Data$sub_category1))
  top_sub_categories <<- head(top_sub_categories[order(top_sub_categories$Freq , decreasing = T),], 0.35 * nrow(top_sub_categories))
  
}

select_top_brands = function(Data ) {
  top_brands <<- data.frame(table(Data$brand_name))
  top_brands <<- head(top_brands[order(top_brands$Freq , decreasing = T),], 0.1* nrow(top_brands))
  # write.csv(top_brands , 'D:/Kaggle/Mercari/temp/brand_frequency.csv' , row.names = FALSE)
}

prepare_data = function(Data , testset){
  Data[, c('section' , 'category' ,'sub_category1')] = tstrsplit(Data$category_name , split = "/", 
                                                                 keep = c(1,2,3))
  
  Data$category_name = NULL
  
  if(testset ==2){
    Data$price = NULL
  }
  
  # Data$length_of_description = nchar(Data$item_description) #read more button for 80
  Data$length_of_name = nchar(Data$name)
  
  Data$section = ifelse(is.na(Data$section), 'not available'  , Data$section)
  Data$section = factor(Data$section)
  
  Data$category = ifelse(is.na(Data$category), 'not available'  , Data$category)
  Data$category = factor(Data$category)
  Data$brand_name_available = ifelse(Data$brand_name == '' , 0,1 )
  Data$brand_name_available = factor(Data$brand_name_available )
  Data$shipping = factor(Data$shipping)
  
  ############################################################
  
  popular_subcategories = data.frame(categories = c( "Pants, Tights, Leggings", "Other","Face","Shoes","Lips","Games",
                                                     "Athletic","Eyes","Cases, Covers & Skins","Shorts","Bras","Tank, Cami","Blouse","Boots",
                                                     "Above Knee, Mini","Necklaces","Makeup Palettes","Women","Shirts & Tops","Sandals",
                                                     "Shoulder Bag","Fashion Sneakers","T-shirts","Knee-Length","Wallets","Tops & T-Shirts",
                                                     "Bracelets","Slim, Skinny","Dolls & Accessories" , 'Jewelry' ,'Cell Phones & Smartphones',
                                                     'Watches' ,'iPad' ,'Laptops & Netbooks' ,'Backpacks, Bags & Briefcases','Digital Cameras',
                                                     'Handbag','Motorcycle' ,'Gadgets' ,'Camera & Photo Accessories' ,'Film Photography' ,'Skateboard',
                                                     'Televisions' , 'Home Speakers & Subwoofers' ))
  
  
  cat('Analyzing subcategories...\n')
  Data$sub_category1 = ifelse(Data$sub_category1 == 'Necklace', 'Necklaces' ,Data$sub_category1)
  #Data[!(Data$sub_category1 %in% popular_subcategories$categories) , 'sub_category1'] = 'Other_subcategory'
  if(testset ==0){
    select_top_sub_categories(Data)
  }
  
  Data[!(Data$sub_category1 %in% top_sub_categories$Var1) , 'sub_category1'] = 'Other_subcategory'
  Data$sub_category1 = factor(Data$sub_category1)
  
  #################################################################
  cat('Analyzing Brands...\n')
  
  rare_costly_brands = data.frame(brand =c('Vitamix','Canada Goose','MCM Worldwide','Moncler',
                                           'Carolina Herrera','Saint Laurent','Escort Radar','MICHELE' ,'Kendra Scott',
                                           'Tiffany & Co.' ,'David Yurman' , 'Chanel'))
  
  costly_popular_brands =data.frame(brand = c('David Yurman'	,'Louis Vuitton',	'Christian Louboutin',
                                              'Sherri Hill',	'Apple',	'Canon',	'Nikon',	'Air Jordan',	'Gucci',
                                              'Givenchy'	,'Bose',	'Spin Master' ,'Nintendo' ,'Lululemon' ))
  
  affordable_popular_brands = data.frame(brand =c('Apple', 'Michael Kors' ,'Adidas', 'LuLaRoe', 'PINK','Nike',
                                                  "Victoria's Secret" , 'Disney', 'FOREVER 21'))
  
  
  Data$rare_costly_brand = 0
  Data$costly_popular_brand = 0
  Data$affordable_popular_brand = 0
  
  for(brand in rare_costly_brands$brand){
    Data$rare_costly_brand = ifelse(Data$brand_name == brand , 1, Data$rare_costly_brand)
  }
  for(brand in costly_popular_brands$brand){
    Data$costly_popular_brand = ifelse(Data$brand_name == brand , 1, Data$costly_popular_brand)
  }
  for(brand in affordable_popular_brands$brand){
    Data$affordable_popular_brand = ifelse(Data$brand_name == brand , 1, Data$affordable_popular_brand)
  }
  
  Data$brand_name = ifelse(grepl('lularoe' , Data$name , ignore.case = TRUE ,perl = TRUE ) ,'LuLaRoe' ,Data$brand_name)
  
  Data$brand_name = ifelse(Data$brand_name == '' , 'Unknown' , Data$brand_name)
  if(testset ==0){
    select_top_brands(Data)
  }
  
  Data[!(Data$brand_name %in% top_brands$Var1) , 'brand_name'] = 'other_brand'
  Data$brand_name = factor(Data$brand_name)
  
  
  Data$brand_name = factor(Data$brand_name)
  Data$rare_costly_brand = factor(Data$rare_costly_brand)
  Data$costly_popular_brand = factor(Data$costly_popular_brand)
  Data$affordable_popular_brand = factor(Data$affordable_popular_brand)
  
  #############################################################################
  cat('Analyzing defects...\n')
  #Description mentions about defects
  Data$no_description = ifelse(Data$item_description == 'No description yet' |
                                 Data$item_description == '' | Data$item_description == ' ', 1, 0)
  Data$no_description = factor(Data$no_description)
  
  defects = data.frame(words = c('stains' ,'holes' ,'tears', 'rips', 'scratches', 'scuffs' , 'smell',
                                 'stain' ,'hole' ,'tear', 'rip', 'scratche', 'scuff' ,'torn'  ))
  
  Data$defect_mentioned_dsn =0
  for(keyword in defects$words){
    Data$defect_mentioned_dsn = ifelse(grepl(keyword , Data$item_description , ignore.case = TRUE ,perl = TRUE ) , 1,Data$defect_mentioned_dsn)
  }
  
  Data$defect_mentioned_dsn = factor(Data$defect_mentioned_dsn)
  
  usage = data.frame(words = c('used once' , 'once used', 'worn once','worn twice','used twice',
                               'used only once'))
  Data$usage_mentioned_in_dsn =0
  for(keyword in usage$words){
    Data$usage_mentioned_in_dsn = ifelse(grepl(keyword , Data$item_description , ignore.case = TRUE ,perl = TRUE ) , 1,Data$usage_mentioned_in_dsn)
  }
  Data$usage_mentioned_in_dsn = factor(Data$usage_mentioned_in_dsn)
  
  Data$worn_a_few_times = ifelse(grepl('worn a few times' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) , 1,0)
  Data$worn_a_few_times = factor(Data$worn_a_few_times)
  
  Data$product_without_tag = ifelse(grepl('without tag' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) , 1, 0)
  Data$product_without_tag = factor(Data$product_without_tag)
  
  Data$product_with_tag = ifelse(grepl('with tag' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) , 1, 0)
  Data$product_with_tag = factor(Data$product_with_tag)
  
  Data$ship_next_day = ifelse(grepl('ship next day' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) , 1, 0)
  Data$ship_next_day = factor(Data$ship_next_day)
  
  Data$negetive_words_in_dsn = 0
  negetive_words = data.frame(words = c('no ', 'not ' , 'never ' ))
  for(keyword in negetive_words$words){
    Data$negetive_words_in_dsn = ifelse(grepl(keyword , Data$item_description , ignore.case = TRUE ,perl = TRUE ) &
                                          Data$no_description == 0, 1,Data$negetive_words_in_dsn)
  }
  
  Data$negetive_words_in_dsn = factor(Data$negetive_words_in_dsn)
  
  #################
  #Analyzing jewelry
  Data$jewelry_is_of_gold = ifelse(Data$category == 'Jewelry' & 
                                     (grepl('gold' , Data$item_description , ignore.case = TRUE ,perl = TRUE )  ), 1,0)
  Data$jewelry_is_of_silver = ifelse(Data$category == 'Jewelry' & 
                                       (grepl('silver' , Data$item_description , ignore.case = TRUE ,perl = TRUE )  ), 1,0)
  #Data$jewelry_is_of_brass = ifelse(Data$category == 'Jewelry' & 
  #                                     (grepl('brass' , Data$item_description , ignore.case = TRUE ,perl = TRUE )  ), 1,0)
  #Data$jewelry_is_of_crystal = ifelse(Data$category == 'Jewelry' & 
  #                                    (grepl('crystal' , Data$item_description , ignore.case = TRUE ,perl = TRUE )  ), 1,0)
  
  Data$jewelry_is_of_gold = factor(Data$jewelry_is_of_gold)
  Data$jewelry_is_of_silver = factor(Data$jewelry_is_of_silver)
  # Data$jewelry_is_of_brass = factor(Data$jewelry_is_of_brass)
  # Data$jewelry_is_of_crystal = factor(Data$jewelry_is_of_crystal)
  
  Data$jewelry_contains_diamond = ifelse(Data$category == 'Jewelry' & 
                                           (grepl('diamond' , Data$item_description , ignore.case = TRUE ,perl = TRUE )  ), 1,0)
  # Data$jewelry_contains_ruby = ifelse(Data$category == 'Jewelry' & 
  #                                          (grepl('ruby' , Data$item_description , ignore.case = TRUE ,perl = TRUE )  ), 1,0)
  # Data$jewelry_contains_gemstone = ifelse(Data$category == 'Jewelry' & 
  #                                          (grepl('gemstone' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
  #                                             grepl('gem stone' , Data$item_description , ignore.case = TRUE ,perl = TRUE )), 1,0)
  # Data$jewelry_contains_pearl = ifelse(Data$category == 'Jewelry' & 
  #                                          (grepl('pearl' , Data$item_description , ignore.case = TRUE ,perl = TRUE )  ), 1,0)
  # Data$jewelry_contains_rosequarz = ifelse(Data$category == 'Jewelry' & 
  #                                          (grepl('rosequartz' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
  #                                             grepl('rose quarz' , Data$item_description , ignore.case = TRUE ,perl = TRUE )), 1,0)
  # Data$jewelry_contains_emerald = ifelse(Data$category == 'Jewelry' & 
  #                                          (grepl('emerald' , Data$item_description , ignore.case = TRUE ,perl = TRUE )  ), 1,0)
  
  
  Data$jewelry_contains_diamond = factor(Data$jewelry_contains_diamond)
  # Data$jewelry_contains_ruby = factor(Data$jewelry_contains_ruby)
  # Data$jewelry_contains_gemstone = factor(Data$jewelry_contains_gemstone)
  # Data$jewelry_contains_pearl = factor(Data$jewelry_contains_pearl)
  # Data$jewelry_contains_rosequarz = factor(Data$jewelry_contains_rosequarz)
  # Data$jewelry_contains_emerald = factor(Data$jewelry_contains_emerald)
  
  Data$gold_10k = ifelse(Data$jewelry_is_of_gold == 1 &
                           (grepl('10 k' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
                              grepl('10-k' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
                              grepl('10k' , Data$item_description , ignore.case = TRUE ,perl = TRUE )), 1,0) 
  Data$gold_14k = ifelse(Data$jewelry_is_of_gold == 1 &
                           (grepl('14 k' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
                              grepl('14-k' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
                              grepl('14k' , Data$item_description , ignore.case = TRUE ,perl = TRUE )), 1,0) 
  
  Data$gold_18k = ifelse(Data$jewelry_is_of_gold == 1 &
                           (grepl('18 k' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
                              grepl('18-k' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
                              grepl('18k' , Data$item_description , ignore.case = TRUE ,perl = TRUE )), 1,0) 
  
  Data$gold_22k = ifelse(Data$jewelry_is_of_gold == 1 &
                           (grepl('22 k' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
                              grepl('22-k' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
                              grepl('22k' , Data$item_description , ignore.case = TRUE ,perl = TRUE )), 1,0) 
  Data$gold_24k = ifelse(Data$jewelry_is_of_gold == 1 &
                           (grepl('24 k' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
                              grepl('24-k' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
                              grepl('24k' , Data$item_description , ignore.case = TRUE ,perl = TRUE )), 1,0) 
  
  
  Data$gold_10k = factor(Data$gold_10k)
  Data$gold_14k = factor(Data$gold_14k)
  Data$gold_18k = factor(Data$gold_18k)
  Data$gold_22k = factor(Data$gold_22k)
  Data$gold_24k = factor(Data$gold_24k)
  
  Data$have_pendant = ifelse((grepl('pendant' , Data$item_description , ignore.case = TRUE ,perl = TRUE )  ), 1,0)
  Data$have_pendant = factor(Data$have_pendant)
  
  #######
  #Games
  Data$game_systems = ifelse(Data$category == 'Video Games & Consoles' &
                               (grepl('game system' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
                                  grepl('gaming system' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
                                  grepl('gamesystem' , Data$item_description , ignore.case = TRUE ,perl = TRUE )), 1,0) 
  Data$game_systems = factor(Data$game_systems)
  
  #######
  #shoulder bag
  Data$bag_has_dustbag = ifelse(Data$category == "Women's Handbags" &
                                  (grepl('dust bag' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) |
                                     grepl('dustbag' , Data$item_description , ignore.case = TRUE ,perl = TRUE ) ), 1,0) 
  
  Data$bag_has_dustbag = factor(Data$bag_has_dustbag)
  
  ##########################################################################
  cat('Analyzing frequent words...\n')
  
  freq = data.frame(words = c('for' ,'new' ,'and'))
  #frequent words
  for(keyword in freq$words){
    Data[paste('name_contains', keyword , sep = '_')] = ifelse(grepl(keyword , Data$name , ignore.case = TRUE ,perl = TRUE ) , 1,0)
  }
  
  
  for(column in colnames(Data)){
    if(grepl('name_contains_' , column,fixed = TRUE))
      Data[,column] = factor(Data[,column])
  }
  
  ####################################################
  
  
  
  #color mentioned in the product name
  colors = data.frame(col = c('black', 'blue', 'gold', 'white', 'pink', 'green', 'purple'))
  Data$color_mentioned_in_name = 0
  for(keyword in colors$col){
    Data$color_mentioned_in_name = ifelse(grepl(keyword , Data$name , ignore.case = TRUE ,perl = TRUE ), 1, Data$color_mentioned_in_name )
  }
  Data$color_mentioned_in_name = factor(Data$color_mentioned_in_name)
  
  
  #size
  sizes = data.frame(size = c('small', 'medium', 'large'))
  Data$name_contains_size = 0
  for(keyword in sizes$size){
    Data$name_contains_size = ifelse(grepl(keyword , Data$name , ignore.case = TRUE ,perl = TRUE ), 1, Data$name_contains_size )
  }
  Data$name_contains_size = factor(Data$name_contains_size)
  
  #Identifying packs and bundles
  packs = data.frame(pack = c('pack' , 'bundle', 'set'))
  Data$bundle_of_items = 0
  for(keyword in packs$pack){
    Data$bundle_of_items = ifelse(grepl(keyword , Data$name , ignore.case = TRUE ,perl = TRUE ), 1, Data$bundle_of_items )
  }
  Data$bundle_of_items = factor(Data$bundle_of_items)
  
  
  Data$name_contains_number = ifelse(grepl("[0-9] ",Data$name)| grepl(" [0-9]",Data$name), 1, 0)
  Data$name_contains_number = factor(Data$name_contains_number)
  
  colnames(Data) = make.names(colnames(Data) , unique = TRUE)
  
  #######################################
  #variables after n gram analysis
  Data$product_in_ori_box = ifelse(grepl('original box',Data$item_description , ignore.case = TRUE ,perl = TRUE) , 1,0)
  Data$price_is_firm = ifelse(grepl('price is firm',Data$item_description , ignore.case = TRUE ,perl = TRUE) , 1,0)
  Data$smoke_free_home = ifelse(grepl('smoke free home',Data$item_description , ignore.case = TRUE ,perl = TRUE) , 1,0)
  Data$cute_product = ifelse(grepl('cute',Data$item_description , ignore.case = TRUE ,perl = TRUE) , 1,0)
  Data$never_used = ifelse(grepl('never used',Data$item_description , ignore.case = TRUE ,perl = TRUE) , 1,0)
  Data$free_shipping = ifelse(grepl('free ship',Data$item_description , ignore.case = TRUE ,perl = TRUE) , 1,0)
  Data$authentic_product = ifelse(grepl('authentic',Data$item_description , ignore.case = TRUE ,perl = TRUE) , 1,0)
  
  Data$product_in_ori_box = factor(Data$product_in_ori_box)
  Data$price_is_firm = factor(Data$price_is_firm)
  Data$smoke_free_home = factor(Data$smoke_free_home)
  Data$cute_product = factor(Data$cute_product)
  Data$never_used = factor(Data$never_used)
  Data$free_shipping = factor(Data$free_shipping)
  Data$authentic_product = factor(Data$authentic_product)
  
  ########################################################################
  
  Data$costly_bag = ifelse(Data$category == "Women's Handbags" & Data$brand_name_available == 1 , 1 , 0)
  Data$costly_shoe = ifelse(Data$category == "Shoes" & Data$brand_name_available == 1 , 1 , 0)
  Data$costly_women_accessory = ifelse(Data$category == "Women's Accessories" & Data$brand_name_available == 1 , 1 , 0)
  Data$costly_cell_phone = ifelse(Data$category == "Cell Phones & Accessories" & Data$brand_name_available == 1 , 1 , 0)
  
  Data$costly_bag = factor(Data$costly_bag)
  Data$costly_shoe = factor(Data$costly_shoe)
  Data$costly_women_accessory = factor(Data$costly_women_accessory)
  Data$costly_cell_phone = factor(Data$costly_cell_phone)
  
  
  #Separating numeric and factor variables
  numeric_v = sapply(X = Data , is.numeric)
  factor_v = sapply(X = Data , is.factor)
  
  Data = cbind(Data[,numeric_v],  Data[,factor_v])
  if(testset ==0){
    Data = arrange.vars(Data, c('price'=  ncol(Data)) )  # very costly
    Data$price = ifelse(is.finite(log(Data$price)) , log(Data$price) , 3.278) # if NaN, use log(mean(price)) value
  }
  numeric_variables = 3
  #Scaling numeric variables
  
  col_max = data.frame(apply(Data[,2:numeric_variables],2 , max))
  col_max = t(col_max)
  
  col_min = data.frame(apply(Data[,2:numeric_variables],2 , min))
  col_min = t(col_min)
  
  Data[,2:numeric_variables] =   data.frame(round(scale(Data[,2:numeric_variables], scale=col_max, center = col_min),9))
  
  #    Data = Data[!is.na(Data) ,]
  
  return(Data)
}

#testing   
#testing again
text_mining = function(sample){
  cat("\014")
  sample$item_description = ifelse(grepl('No description yet',sample$item_description , ignore.case = TRUE ,perl = TRUE) , '',sample$item_description)
  gc()
  cat("tolower...\n")
  sample$item_description = tolower(sample$item_description)
  cat("creating corpus...\n")
  corpus = Corpus(VectorSource(sample$item_description)) 
  cat("stop words...\n")
  corpus = tm_map(corpus , removeWords , stopwords('english'))
  cat("punct...\n")
  corpus = tm_map(corpus , removePunctuation)
  #cat("tolower...\n")
  #corpus = tm_map(corpus , tolower) #taking longer time!
  cat("stem doc...\n")
  corpus = tm_map(corpus , stemDocument)
  cat("creating quanteda...\n")
  
  corpus1 = quanteda::corpus(corpus)
  
  cat("creating dfm matrix...\n")
  dfm_matrix = quanteda::dfm(quanteda::tokens(corpus1 ) , 
                             ngrams =2 , tolower = FALSE , stem = FALSE , remove_punct = FALSE)
  return (dfm_matrix)
}



if(local_run == 1){
  
  Data_ori = fread('D:/Kaggle/Mercari/Data/train.tsv', sep='\t')
  test_s2 = fread('D:/Kaggle/Mercari/Data/test.tsv', sep='\t' )
} else{
  
  Data_ori = fread('../input/train.tsv', sep='\t')
  #test_ori = fread('../input/test.tsv', sep='\t')  
  test_s2 = fread('../input/test_stg2.tsv', sep='\t')
}


test_s3 = test_s2
colnames(test_s3)[1] = 'train_id'
test_s3$price = 0
sample = rbind(Data_ori ,test_s3)
data_dfm = text_mining(Data_ori)
gc()

###############################################

if(local_run == 0){
  
  test_length = nrow(test_s3)     #3460725
  test_s21 = test_s2[1:1153575,]
  test_dfm1 = text_mining(test_s21)
  
  test_s22 = test_s2[1153576:2307150,]
  test_dfm2 = text_mining(test_s22)
  
  test_s23 = test_s2[2307151:3460725,]
  test_dfm3 = text_mining(test_s23)
  
  test_dfm = rbind(test_dfm1 , test_dfm2, test_dfm3)
  dim(test_dfm)
  
}

###############################################
#test_dfm = text_mining(test_s2)

rm(corpus, corpus1 , sample, test_s3)

if(local_run == 0)
{
  
  dfm_matrix = rbind(data_dfm , test_dfm)
}else
{
  dfm_matrix = data_dfm 
}
rm(test_dfm1 , test_dfm2, test_dfm3 , test_s21 , test_s22 , test_s23)
gc()
#data_dfm1 = quanteda::dfm_trim(data_dfm , min_docfreq = 5000 )
#test_dfm1 = quanteda::dfm_trim(test_dfm , min_docfreq = 500 )

#reducing the sparse matrix
data_desc = quanteda::dfm_trim(dfm_matrix , min_docfreq = 28000 ) #if we are deleting dfm_matrix earlier , we can decrease this value

my_idf = quanteda::dfm_tfidf(data_desc)      # my_idf contains the matrix of train + test prepared for pca
colnames(my_idf) = make.names(colnames(my_idf) , unique = TRUE )
rm(data_desc,   Data_ori , test_s2 , data_dfm1, test_dfm1   )

dim(my_idf)

train_de = my_idf[1:1400000,]  #1482535
test_de = my_idf[1400000:1482535,]
gc()

train_de =  data.frame(train_de)
test_de =  data.frame(test_de)

# rm(data_desc , dfm_matrix)
#train_des = rbind(train_de ,train_de2 )

#PCA 

rm(Data_ori , Data_price , steps , test , test_price , train , Data, data_dfm,Data_price, dfm_matrix  , pca,test_dfm ,x, validation.x )


if(local_run == 1){
  
  Data_ori = fread('D:/Kaggle/Mercari/Data/train.tsv', sep='\t')
  test_s2 = fread('D:/Kaggle/Mercari/Data/test.tsv', sep='\t' )
} else{
  
  Data_ori = fread('../input/train.tsv', sep='\t')
  #test_ori = fread('../input/test.tsv', sep='\t')  
  test_s2 = fread('../input/test_stg2.tsv', sep='\t')
}


Data = data.frame(Data_ori)
#test = data.frame(test_ori)

train = Data[1:1400000,]
test = Data[1400000:1482535,]

top_sub_categories = data.frame()
top_brands = data.frame()
very_costly_items = vector()
train = prepare_data(train , 0)
very_costly_items = ifelse(train$price > log(300) , 1 ,0)
#test = prepare_data(test, 1)

test_price = data.frame(test$price)
test = prepare_data(test, 2)

#Keeping a copy of y for calculating rmsle
Data_price = data.frame(train$price)


train = cbind(train , train_de)
test = cbind(test , test_de)
train = arrange.vars(train, c('price'=  ncol(train)) )


train_variables = ncol(train)
test_variables = ncol(train) -1
weights = 1 + very_costly_items * 1.2
model_matrix_needed = 1


#colnames(test)[1] = 'train_id'

# ################################################################################
#             #Creating Train and validation set
#             train_size = 0.8
#             set.seed(11)
#             train_i = sample(1:nrow(Data), size = train_size*nrow(Data))
#             #validation_i = sample(1:nrow(Data), size = 0.2*nrow(Data))
#             
#             
#             validation = Data[-train_i , ]
#             train = Data[train_i,]
#             train_price = Data_price[train_i,]
#             validation_price = Data_price[-train_i , ]
#             weights_train = weights[train_i  ]
#             
#             results = {}
#             print('done creating validation set!')
#             
# ############################################################################################################


cat('modelling....')
rm(pca_test , pca_train , temp)

results = {}
gc()

#lasso using elastic net
x = sparse.model.matrix(train$price ~. , data= train[,2: test_variables] )
y = train$price
gc()
#test.x = sparse.model.matrix( ~. , data=test_s2[,2:test_variables ])

validation.x = sparse.model.matrix( ~. , data=test[,2:test_variables ])
gc()
#model ##
xg.param = list(eta = c( 0.1) , max_depth = c(90) , min_child_weight  = c(300) 
                ,colsample_bytree = c(0.5),print_every_n = 15 )
model = xgboost(x, y , param = xg.param , nrounds = 300 , verbose= TRUE , nthread= 4) #weight = weights_train,

print('training done!')

#Validation set   
lasso.model.predicted = predict(model, validation.x )
lasso.model.predicted = ifelse(lasso.model.predicted < 0 , -lasso.model.predicted , lasso.model.predicted)

#summary(lasso.model)
model.predicted = ifelse(is.finite(exp(lasso.model.predicted)) , exp(lasso.model.predicted) , 25)
results['lasso.model.model'] = rmsle((test_price$test.price) , model.predicted)
print(results)





test_s2_rows = 3460725 


testset_index_begin = 1482536
testset_index_end = 4943261

test_idf = my_idf[testset_index_begin : testset_index_end,]
#Appending the column names



###########################################################################################
#Predicting private test data

if(local_run == 0){
  test_s2 = fread('../input/test_stg2.tsv', sep='\t')}

test = prepare_data(test, 1)


#trying direct prediction
test = cbind(test, test_idf)

#Trying to apply pca to complete data
for (i in 0:33  )
{
  cat(i)
  cat(' preparing data...\n')
  x =   i * 100000
  
  steps = data.frame((test_idf[x : (x + 99999) , ]  ))
  steps = steps[,2:ncol(steps)]
  temp = predict(pca , steps )  #contains our test set
  
  pca_tuning = 50
  pca_test = temp[,1:pca_tuning]
  
  test_step = cbind(test_no_pca[x : (x + 99999 ), ] , pca_test)
  
  test_step.x = sparse.model.matrix( ~. , data= test_step[,2:test_variables ])
  
  step_predicted = predict(model, test_step.x )           #Model prediction done here
  
  step_predicted = ifelse(step_predicted < 0 , -step_predicted , step_predicted)
  step_predicted = ifelse(is.finite(exp(step_predicted)) , exp(step_predicted) , 25)
  
  test_step$price = step_predicted
  
  if(local_run == 1){
    write.table(test_step[,c('test_id' , 'price_predicted')], file = "D:/Kaggle/Mercari/Results/Description/testset.csv"  , row.names = FALSE , append = TRUE , sep = "," , col.names = FALSE)
  }
  else{
    
    fwrite(test_step[,c('test_id' , 'price')] , 'myoutput3.csv' , append = TRUE)
  }
}


#Last test case rows
steps = data.frame((test_idf[3400000 : 3460725 , ]  ))

steps = steps[,2:ncol(steps)]
temp = predict(pca , steps )  #contains our test set

pca_tuning = 50
pca_test = temp[,1:pca_tuning]

test_step = cbind(test_no_pca[3400000: (3460725 ), ] , pca_test)

test_step.x = sparse.model.matrix( ~. , data= test_step[,2:test_variables ])

step_predicted = predict(model, test_step.x )             #Model prediction done here

step_predicted = ifelse(step_predicted < 0 , -step_predicted , step_predicted)
step_predicted = ifelse(is.finite(exp(step_predicted)) , exp(step_predicted) , 25)

test_step$price_predicted = step_predicted

if(local_run == 1){
  write.table(test_step[,c('test_id' , 'price_predicted')], file = "D:/Kaggle/Mercari/Results/Description/testset.csv"  , row.names = FALSE , append = TRUE , sep = "," , col.names = FALSE)
}else{
  
  fwrite(test_step[,c('test_id' , 'price')] , 'myoutput3.csv' , append = TRUE)
}
