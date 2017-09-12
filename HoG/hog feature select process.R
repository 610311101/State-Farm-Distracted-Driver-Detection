library("jpeg")
library("magrittr");
library("imager");
source("State-Farm-Distracted-Driver-Detection/HoG/hog.R")
##  train
index <- paste0("c",0:9);
for(i in index){
  path      <- paste0("imgs/train/",i,"/");
  img_index <- list.files(path = path);
  feat <- list();
  for( j in seq(length(img_index)) ){
    m <- load.image(paste0(path,img_index)[j]) %>% grayscale();
    m <- resize(m, size_x = width(m)/10, size_y = height(m)/10);
    m <- as.matrix(m)
    feat[[j]] <- hog(m, block = c(32,32), cell = c(8,8), stride = c(16,16),histsize = 9);
    show(j);
  }
  feat <- do.call("rbind", feat);
  saveRDS(feat, paste(i))
}

df <- lapply(index, readRDS);
df <- lapply(df, as.data.frame);
df <- lapply(seq(length(index)), function(i){
  df[[i]]$y <- index[i];
  return(df[[i]])
  })
df   <- do.call("rbind", df);
df$y <- as.factor(df$y);
saveRDS(df,"train")

##  test
path      <- paste0("imgs/test/");
img_index <- list.files(path = path);
feat <- list();
for( j in seq(length(img_index)) ){
  m <- load.image(paste0(path,img_index)[j]) %>% grayscale();
  m <- resize(m, size_x = width(m)/10, size_y = height(m)/10);
  m <- as.matrix(m)
  feat[[j]] <- hog(m, block = c(32,32), cell = c(8,8), stride = c(16,16),histsize = 9);
  show(j);
}
feat <- do.call("rbind", feat);
saveRDS(feat, "test");



