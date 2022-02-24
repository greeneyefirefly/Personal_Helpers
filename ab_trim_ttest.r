# Samantha Deokinanan

# A Gaussian distribution enables possible identification of outliers—ideally data points that are considered 
# very low and very high within the data set. Observing these outliers can be advantageous to a study because 
# understanding the fundamental reason as to why they were even recorded can, in turn, lead to more fitting 
# statistical analysis and better inferences. Thus, asymmetrical trimming of special data sets can provide 
# accurate statistical inferences than classical and common robust statistical methods because data sets are 
# trimmed to target the ideal cases for analysis rather than symmetrically trimming or substituting a specific
# percentage of data.

ab_trim_ttest = function(data1, data2, a, b){

  # Variables 
  # data1, data2 : column of each group of data 
  # a : alpha, minimum score at alpha-th percentile
  # b : beta, maximum score at beta-th percentile
  
  data1 = sort(data1)            # Order Statistics
  n = dim(as.data.frame(data1))
  data2 = sort(data2)            # Order Statistics
  n = rbind(n, dim(as.data.frame(data2)))
  
  an = c() ; bn = c();
  for (i in 1:2){
      an[i] = floor(a*n[i]+1)	# Locates minimum score at alpha-th percentile
      bn[i] = floor(b*n[i])		# Locates maximum score at beta-th percentile
  }    
  Tdata1 = data1[an[1]:bn[1]]	# Trims dataset #1
  p = dim(as.data.frame(Tdata1))
  Tdata2 = data2[an[2]:bn[2]]	# Trims dataset #2
  p = rbind(p, dim(as.data.frame(Tdata2)))
  tm = c(); tv = c(); vtm = c();
  tm[1] = mean(Tdata1)	
  tm[2] = mean(Tdata2) # Trimmed data
  tv[1] = var(Tdata1) 
  tv[2] = var(Tdata2)  # Variance of trimmed data
  # Trimmed Variance
  vtm[1] = (tv[1]/(b-a)) + (1/(b-a)^2) * 
    ((b*(1-b) * ((data1[bn[1]] - tm[1])^2)) - ((2*a) * (1-b) * (data1[an[1]] - tm[1]) * (data1[bn[1]] - tm[1])) + 
       (a*(1-a) * (data1[an[1]] - tm[1])^2)) 
  vtm[2] = (tv[2]/(b-a)) + (1/(b-a)^2) * 
    ((b*(1-b) * ((data2[bn[2]] - tm[2])^2)) - ((2*a) * (1-b) * (data2[an[2]] - tm[2]) * (data2[bn[2]] - tm[2])) + 
       (a*(1-a) * (data2[an[2]] - tm[2])^2)) 
  
  tt = (tm[1]-tm[2]) / sqrt((vtm[1]/p[1]) + (vtm[2]/p[2])) # Independent Sample t-test of trimmed data
  
  # ~~~~~~~~~~~~~~~~Output~~~~~~~~~~~~~~~~~~~~~~~
cat('
   Adaptive Trimmed Mean Two Sample t-test
----------------------------------------------
Data Sets  |  Trimmed Mean   |  Variance
----------------------------------------------
Data Set 1 |    ',round(tm[1],2),'      |  ',round(vtm[1],2),'          
Data Set 2 |    ',round(tm[2],2),'      |  ',round(vtm[2],2),'     
----------------------------------------------
t-score based on trimmed mean = ',tt,'
degrees of freedom = ',bn[1]-an[1],' 
----------------------------------------------')
}
