# 森林图组件 -------------------------
## 组件1：生成n个空白列放置森林图 --------------------------------------------------------------------

blank_cols <- function(nrow, ncol) {
  col_names <- paste("newcol", 1:ncol, sep = "_")
  data <- matrix(paste(rep(" ", 20), collapse = " "), nrow = nrow, ncol = ncol)
  colnames(data) <- col_names
  return(as.data.frame(data))
}

## 组件2：将2个df交叉合并 -----------------------------------------------------------

interleave_columns <- function(df1, df2) {
  n <- min(ncol(df1), ncol(df2))
  df_combined <- data.frame(matrix(ncol = 0, nrow = nrow(df1)))
  
  for (i in 1:n) {
    df_combined[names(df1)[i]] <- df1[[i]]
    df_combined[names(df2)[i]] <- df2[[i]]
  }
  
  if (ncol(df1) > n) {
    remaining_columns_df1 <- df1[(n+1):ncol(df1)]
    colnames(remaining_columns_df1) <- names(remaining_columns_df1)
    df_combined <- cbind(df_combined, remaining_columns_df1)
  }
  
  if (ncol(df2) > n) {
    remaining_columns_df2 <- df2[(n+1):ncol(df2)]
    colnames(remaining_columns_df2) <- names(remaining_columns_df2)
    df_combined <- cbind(df_combined, remaining_columns_df2)
  }
  
  return(df_combined)
}

## 组件3：b和se转换为显示内容 -------------------------------------------------------------------

calculate_hr_lo_hi <- function(b, se, decimal_places) {
  
  n <- length(b)
  
  result_char <- vector("character", n)
  result_hr <- vector("numeric", n)
  result_lo <- vector("numeric", n)
  result_hi <- vector("numeric", n)
  
  for (i in 1:n) {
    
    if (is.na(b[i]) || is.na(se[i])) {
      
      result_char[i] <- ""
      result_hr[i] <- ""
      result_lo[i] <- ""
      result_hi[i] <- ""
      
    } else {
      
      hr <- exp(b[i])
      lo <- exp(b[i] - 1.96 * se[i])
      hi <- exp(b[i] + 1.96 * se[i])
      
      result_char[i] <- sprintf(paste0("%.", decimal_places, "f (%.", decimal_places, "f to %.", decimal_places, "f)"), hr, lo, hi)
      result_hr[i] <- hr
      result_lo[i] <- lo
      result_hi[i] <- hi
    }
  }
  
  result = list(char = result_char, hr = as.numeric(result_hr), lo = as.numeric(result_lo),
                hi = as.numeric(result_hi))
  return(result)
}

## 组件4：生成森林图需要的char，hr，lo，hi ---------------------------------------------------

get_fp_components <- function(data, b, se, decimal_places, hr_text){
  
  # 选择b和se
  df1 = data
  b_selectd = b
  se_selectd = se
  
  # 确定有n2组b和se
  n2 = length(b_selectd)
  
  # 循环n2次
  
  results = vector("list", n2)
  
  for (i in 1:n2) {
    
    b  = b_selectd[i]
    se = se_selectd[i]
    
    results[[i]] = calculate_hr_lo_hi(df1[[b]], df1[[se]], decimal_places)
    
  }
  
  results
  
  # 分别把char, hr, lo, hi提取到不同的list
  # 其中char要变为df
  
  chars <- map_dfc(results, ~ .x$char)
  names(chars) = paste(hr_text, 1:n2, sep = '_')
  
  hrs <- map(results, ~ .x$hr)
  los <- map(results, ~ .x$lo)
  his <- map(results, ~ .x$hi)
  
  final = list(chars = chars, hrs = hrs, los = los, his = his)
  
  return(final)
}
