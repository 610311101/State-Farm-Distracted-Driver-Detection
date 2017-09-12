hog <- function(image, block, cell, stride, histsize, returntype = "numeric"){
  image_matrix <- image
  ##  add zero row
  image_matrix <- cbind(
    rep(0, nrow(image_matrix)),
    image_matrix
  );
  image_matrix <- cbind(
    image_matrix,
    rep(0, nrow(image_matrix))
  );
  
  ##  add zero col
  image_matrix <- rbind(
    rep(0, ncol(image_matrix)),
    image_matrix
  );
  image_matrix <- rbind(
    image_matrix,
    rep(0, ncol(image_matrix))
  );
  
  ##  x-gradient
  r <- c(1,2);  ##  right side
  l <- c(ncol(image_matrix)-1,ncol(image_matrix))  ##  left side
  x_grad <- image_matrix[,-r] - image_matrix[,-l];
  x_grad <- x_grad[-c(1,nrow(x_grad)),];
  
  ##  y-gradient 
  d <- c(1,2);  ##  down side
  u <- c(nrow(image_matrix)-1,nrow(image_matrix));  ##  up side
  y_grad <- image_matrix[-u,] - image_matrix[-d,];
  y_grad <- y_grad[,-c(1,ncol(y_grad))];
  y_grad <- y_grad + 0.5
  
  ##  angle xy-gradient
  grad_angle <- atan(y_grad/x_grad);
  ##  L2 norm xy-gradient
  grad_value <- sqrt(x_grad^2 + y_grad^2);
  hog <- list();
  
  block_row_index  <- c(1:block[1]);
  while( !max(block_row_index)>dim(grad_value)[1] ){
    block_col_index <- c(1:block[2]);
    repeat{
      if( max(block_col_index)>dim(grad_value)[2] ) break;
      block_hog <- list();
      block_grad_value <- grad_value[block_row_index, block_col_index];
      block_grad_angle <- grad_angle[block_row_index, block_col_index];
      ##  cell
      cell_row_index <- c(1:cell[1]);
      while( !max(cell_row_index)>dim(block_grad_value)[1] ){
        cell_col_index <- c(1:cell[2]);
        repeat{
          if( max(cell_col_index)>dim(block_grad_value)[2] ) break;
          cell_hog <- list();
          cell_grad_value <- block_grad_value[cell_row_index, cell_col_index];
          cell_grad_angle <- block_grad_angle[cell_row_index, cell_col_index];
          range_point <- seq(-pi/2,pi/2,length.out = histsize+1);
          cell_hog <- rep(NA,histsize);
          for( i in seq(histsize) ){
            cell_hog[i] <-
              sum(
                cell_grad_value[
                  cell_grad_angle>range_point[i] & cell_grad_angle<range_point[i+1]
                  ]
              )
          }
          block_hog <- append(block_hog, list(cell_hog));
          cell_col_index <- cell_col_index + cell[2];
        }
        cell_row_index <- cell_row_index + cell[1];
      }
      ##  cell
      block_col_index <- block_col_index + stride[2];
      hog <- append(hog, list(block_hog));
    }
    block_row_index <- block_row_index + stride[1];
  }
  if( returntype == "list" ) return(hog)
  if( returntype == "numeric") return(unlist(hog))
}

# debug(hog)
# hog(array(runif(2048), dim = c(32,64)),
#     block  = c(8,8),cell   = c(4,4),stride = c(4,4),histsize = 9)


     
  