# LUIS MIGUEL ROLDAN ALZATE
# FRANCISCO ZULUAGA 
# MULTIVARIATE FUNCTIONAL OUTLIER DETECTION USING ENVIRONMENTAL DATA

# CALLING PACKAGES
# install.packages("FUNTA")
library(DepthProc)
library(sn)
library(mvtnorm)
library(devtools)
library(fda)
library(fda.usc)
library(mrfDepth)
library(ggplot2)
library(rainbow)
library(roahd)
library(MFHD)
library(zoo)
library(ggplot2)
library(depthTools)
library(MFHD)
library(MFPCA)

# FUNCTION FOR READING FUNCTIONAL DATA IN ORDER TO BE PLOTTED
ready2plot<-function(data){
  data<-t(data$data)
  library(reshape2)
  data_t_long<-reshape2::melt(data)
  return(data.frame(data_t_long))
}

# FUNCTION FOR GENERATING HEAVY-TAILED DATASETS

generate_cauchy_mfdata<-function (N, L, centerline, correlations, listCov = NULL, listCholCov = NULL) {
  if (length(correlations) != 0.5 * (L) * (L - 1)) {
    stop("Error in generate_cauchy_mfdata: you have to provide all the\n          correlations among functional components")
  }
  if (nrow(centerline) != L) {
    stop("Error in generate_cauchy_mfdata: you have to provide a centerline for\neach dimension")
  }
  if (is.null(listCov) & is.null(listCholCov)) {
    stop("Error: You have to provide at least either covariance matrices or\n          their cholesky factors to generate_cauchy_mfdata")
  }
  else if (!is.null(listCholCov)) {
    if (length(listCholCov) != L) {
      stop("Error: You have to provide a covariance Cholesky factor for each\n              dimension")
    }
    P = ncol(listCholCov[[1]])
    if (ncol(centerline) != P | any(sapply(listCholCov, nrow) != 
                                    P) | any(sapply(listCholCov, ncol) != P)) {
      stop("Error: You provided mismatching centerline and covariance\nmatrices Cholesky factors to generate_cauchy_mfdata")
    }
  }
  else if (!is.null(listCov)) {
    P = ncol(listCov[[1]])
    if (ncol(centerline) != P | any(sapply(listCov, nrow) != 
                                    P) | any(sapply(listCov, ncol) != P)) {
      stop("Error: You provided mismatching centerline and covariance\nmatrices to generate_cauchy_mfdata")
    }
    listCholCov = lapply(listCov, chol)
  }
  R = matrix(1, ncol = L, nrow = L)
  R[upper.tri(R)] = as.numeric(correlations)
  R[lower.tri(R)] = as.numeric(correlations)
  R_chol = chol(R)
  Data = matrix(rt(N * L * P, 3), ncol = L, nrow = N * P)
  Data = Data %*% R_chol
  return(values = eval(parse(text = paste("list( ", paste("t( t( matrix( Data[ , ", 
                                                          1:L, " ], nrow = N, ncol = P ) %*% listCholCov[[ ", 
                                                          1:L, " ]] ) + as.numeric( centerline[ ", 1:L, ", ] ) )", 
                                                          sep = "", collapse = ", "), " )", sep = ""))))
}


# PARAMETERS
# ----
# DIMENSIONS OF THE MULTIVARIATE FUNCTIONAL DATASET

# N=NUMBER OF FUNCTIONS (NUMBER OF ROWS)
N = 200
# P=NUMBER OF DISCRETIZATION POINTS (TIME POINTS)
P = 100
# L=NUMBER OF VARIABLES
L = 2

# DEFINING THE TIME GRID
time_grid = seq( 0, 1, length.out = P )

# DEFINING THE COVARIANCE STRUCTURE (FOLLOWING IEVA AND PAGANONI 2017)
C1 = exp_cov_function(time_grid, alpha = 0.5, beta = 0.4 )
C2 = exp_cov_function(time_grid, alpha = 0.7, beta = 0.4 )


# COLOR PARAMETERS FOR PLOTS
noout_col=rep("blue", 200)
col95=noout_col
col95[191:200]=rep("black",10)
col90=noout_col
col90[181:200]=rep("black", 20)
col85=noout_col
col85[171:200]=rep("black", 30)
col80=noout_col
col80[161:200]=rep("black", 40)


# BASE FUNCTIONS PARAMETERS FOR MAGNITUDE OUTLIER DEFINITION
b_x=rbinom(N,1,0.5)
b_y=rbinom(N,1,0.5)
mu_x=sin( 2 * pi * time_grid )
mu_y=sin( 4 * pi * time_grid )
reg=matrix(c(mu_x, mu_y),nrow = 2, byrow = TRUE )


# MAGNITUDE OUTLIERS
w_x=2+rexp(time_grid,rate=2)
w_y=2+rexp(time_grid,rate=2)

outlier_mag_x=((b_x*(w_x))-((1-b_x)*(w_x)))
outlier_mag_y=((b_y*(w_y))-((1-b_y)*(w_y)))

# SHAPE OUTLIERS
mu2_x=sin( 2 * pi *(time_grid-0.5) )
mu2_y=sin( 4 * pi * (time_grid-0.25) )

# COVARIANCE OUTLIERS
C3=exp_cov_function( time_grid, alpha = 1.5, beta = 1 )
C4=exp_cov_function( time_grid, alpha = 1.7, beta = 1 )

# MIXED OUTLIERS PARAMETERS AND CONSTRUCTION
mu3_x=((5/2)+w_x)*mu_x
mu3_y=((5/2)+w_y)*mu_y
outlier_mix<-matrix(c(mu3_x, mu3_y), nrow=2, byrow=T)

# SHAPE OUTLIERS
outlier_shape<-matrix(c(mu2_x, mu2_y), nrow=2, byrow=T)

# ISOLATED OUTLIERS
outlier_iso<-reg
outlier_iso[,90:100]<-outlier_mix[,90:100]

# TREND OUTLIERS
mu4_x= 2 * (2-(time_grid*4))^2 -4
mu4_y=-2 * (2-(time_grid*4))^2 +4
outlier_trend<-matrix(c(mu4_x, mu4_y), nrow=2, byrow=T)

# ----
## DATASETS
# MAGNITUDE OUTLIERS
# ----
data_t_noout_validation<-list()
for (i in 1:100){
  data_t_noout_validation[[i]]<-generate_cauchy_mfdata(N, L, reg, correlations = 0.7, listCov = list( C1, C2) )
}



data_t_out_mag95_validation<-data_t_t_noout_validation
data_t_out_mag90_validation<-data_t_t_noout_validation
data_t_out_mag85_validation<-data_t_noout_validation
data_t_out_mag80_validation<-data_t_noout_validation

fdata_t_out_mag95_validation<-list()
fdata_t_out_mag90_validation<-list()
fdata_t_out_mag85_validation<-list()
fdata_t_out_mag80_validation<-list()


data_t_noout<-generate_cauchy_mfdata(N, L, reg, correlations = 0.7, listCov = list( C1, C2) )

data_t_out_mag95<-data_t_noout
data_t_out_mag95[[1]][191:200,]=data_t_noout[[1]][191:200,]+outlier_mag_x[191:200]
data_t_out_mag95[[2]][191:200,]=data_t_noout[[2]][191:200,]+outlier_mag_y[191:200]

data_t_out_mag90<-data_t_noout
data_t_out_mag90[[1]][181:200,]=data_t_noout[[1]][181:200,]+outlier_mag_x[181:200]
data_t_out_mag90[[2]][181:200,]=data_t_noout[[2]][181:200,]+outlier_mag_y[181:200]

data_t_out_mag85<-data_t_noout
data_t_out_mag85[[1]][171:200,]=data_t_noout[[1]][171:200,]+outlier_mag_x[171:200]
data_t_out_mag85[[2]][171:200,]=data_t_noout[[2]][171:200,]+outlier_mag_y[171:200]

data_t_out_mag80<-data_t_noout
data_t_out_mag80[[1]][161:200,]=data_t_noout[[1]][161:200,]+outlier_mag_x[161:200]
data_t_out_mag80[[2]][161:200,]=data_t_noout[[2]][161:200,]+outlier_mag_y[161:200]


fdata_t_noout<-mfData(time_grid,data_t_noout)

fdata_t_out_mag95<-mfData(time_grid,data_t_out_mag95)
fdata_t_out_mag90<-mfData(time_grid,data_t_out_mag90)
fdata_t_out_mag85<-mfData(time_grid,data_t_out_mag85)
fdata_t_out_mag80<-mfData(time_grid,data_t_out_mag80)
# ----
# GENERAR LAS BASES DE DATOS PARA LA PRUEBA
# 5%
for (i in 1:100){
  data_t_out_mag95_validation[[i]][[1]][191:200,]<-data_t_noout_validation[[i]][[1]][191:200,]+outlier_mag_x[191:200]
  data_t_out_mag95_validation[[i]][[2]][191:200,]<-data_t_noout_validation[[i]][[2]][191:200,]+outlier_mag_y[191:200]
  fdata_t_out_mag95_validation[[i]]<-roahd::mfData(time_grid, data_t_out_mag95_validation[[i]])
}
# 10%
for (i in 1:100){
  data_t_out_mag90_validation[[i]][[1]][181:200,]<-data_t_noout_validation[[i]][[1]][181:200,]+outlier_mag_x[181:200]
  data_t_out_mag90_validation[[i]][[2]][181:200,]<-data_t_noout_validation[[i]][[2]][181:200,]+outlier_mag_y[181:200]
  fdata_t_out_mag90_validation[[i]]<-roahd::mfData(time_grid, data_t_out_mag90_validation[[i]])
}
# 15%
for (i in 1:100){
  data_t_out_mag85_validation[[i]][[1]][171:200,]<-data_t_noout_validation[[i]][[1]][171:200,]+outlier_mag_x[171:200]
  data_t_out_mag85_validation[[i]][[2]][171:200,]<-data_t_noout_validation[[i]][[2]][171:200,]+outlier_mag_y[171:200]
  fdata_t_out_mag85_validation[[i]]<-roahd::mfData(time_grid, data_t_out_mag85_validation[[i]])
  
}
# 20%
for (i in 1:100){
  data_t_out_mag80_validation[[i]][[1]][161:200,]<-data_t_noout_validation[[i]][[1]][161:200,]+outlier_mag_x[161:200]
  data_t_out_mag80_validation[[i]][[2]][161:200,]<-data_t_noout_validation[[i]][[2]][161:200,]+outlier_mag_y[161:200]
  fdata_t_out_mag80_validation[[i]]<-roahd::mfData(time_grid, data_t_out_mag80_validation[[i]])
  
}


# SHAPE OUTLIERS
# ----
data_t_out_shape<-generate_cauchy_mfdata(N, L, outlier_shape, correlations = 0.7, listCov = list( C1, C2) )

data_t_out_shape95<-data_t_noout
data_t_out_shape95[[1]][191:200,]=data_t_out_shape[[1]][191:200,]
data_t_out_shape95[[2]][191:200,]=data_t_out_shape[[2]][191:200,]

data_t_out_shape90<-data_t_noout
data_t_out_shape90[[1]][181:200,]=data_t_out_shape[[1]][181:200,]
data_t_out_shape90[[2]][181:200,]=data_t_out_shape[[2]][181:200,]

data_t_out_shape85<-data_t_noout
data_t_out_shape85[[1]][171:200,]=data_t_out_shape[[1]][171:200,]
data_t_out_shape85[[2]][171:200,]=data_t_out_shape[[2]][171:200,]

data_t_out_shape80<-data_t_noout
data_t_out_shape80[[1]][161:200,]=data_t_out_shape[[1]][161:200,]
data_t_out_shape80[[2]][161:200,]=data_t_out_shape[[2]][161:200,]

fdata_t_out_shape95<-mfData(time_grid,data_t_out_shape95)
fdata_t_out_shape90<-mfData(time_grid,data_t_out_shape90)
fdata_t_out_shape85<-mfData(time_grid,data_t_out_shape85)
fdata_t_out_shape80<-mfData(time_grid,data_t_out_shape80)
fdata_t_out_shape0<-mfData(time_grid, data_t_out_shape)


# CONJUNTOS DE VALIDACIÓN PARA PRUEBA DE ATÍPICOS

data_t_out_shape95_validation<-data_t_noout_validation
data_t_out_shape90_validation<-data_t_noout_validation
data_t_out_shape85_validation<-data_t_noout_validation
data_t_out_shape80_validation<-data_t_noout_validation

data_t_out_shape_validation<-list()
fdata_t_out_shape95_validation<-list()
fdata_t_out_shape90_validation<-list()
fdata_t_out_shape85_validation<-list()
fdata_t_out_shape80_validation<-list()
# ----
# SHAPE OUTLIERS
# 100%
for (i in 1:100){
  data_t_out_shape_validation[[i]]<-generate_cauchy_mfdata(N, L, outlier_shape, correlations = 0.7, listCov = list( C1, C2) )
}
# 5%
for (i in 1:100){
  data_t_out_shape95_validation[[i]][[1]][191:200,]<-data_t_out_shape_validation[[i]][[1]][191:200,]
  data_t_out_shape95_validation[[i]][[2]][191:200,]<-data_t_out_shape_validation[[i]][[2]][191:200,]
  fdata_t_out_shape95_validation[[i]]<-roahd::mfData(time_grid, data_t_out_shape95_validation[[i]])
}
# 10%
for (i in 1:100){
  data_t_out_shape90_validation[[i]][[1]][181:200,]<-data_t_out_shape_validation[[i]][[1]][181:200,]
  data_t_out_shape90_validation[[i]][[2]][181:200,]<-data_t_out_shape_validation[[i]][[2]][181:200,]
  fdata_t_out_shape90_validation[[i]]<-roahd::mfData(time_grid, data_t_out_shape90_validation[[i]])
}
# 15%
for (i in 1:100){
  data_t_out_shape85_validation[[i]][[1]][171:200,]<-data_t_out_shape_validation[[i]][[1]][171:200,]
  data_t_out_shape85_validation[[i]][[2]][171:200,]<-data_t_out_shape_validation[[i]][[2]][171:200,]
  fdata_t_out_shape85_validation[[i]]<-roahd::mfData(time_grid, data_t_out_shape85_validation[[i]])
}
# 20%
for (i in 1:100){
  data_t_out_shape80_validation[[i]][[1]][161:200,]<-data_t_out_shape_validation[[i]][[1]][161:200,]
  data_t_out_shape80_validation[[i]][[2]][161:200,]<-data_t_out_shape_validation[[i]][[2]][161:200,]
  fdata_t_out_shape80_validation[[i]]<-roahd::mfData(time_grid, data_t_out_shape80_validation[[i]])
}



# COVARIANCE OUTLIERS
# ----
data_t_out_cov<-generate_cauchy_mfdata(N, L, reg, correlations = 0, listCov = list( C3,C4) )

data_t_out_cov95<-data_t_noout
data_t_out_cov95[[1]][191:200,]=data_t_out_cov[[1]][191:200,]
data_t_out_cov95[[2]][191:200,]=data_t_out_cov[[2]][191:200,]

data_t_out_cov90<-data_t_noout
data_t_out_cov90[[1]][181:200,]=data_t_out_cov[[1]][181:200,]
data_t_out_cov90[[2]][181:200,]=data_t_out_cov[[2]][181:200,]

data_t_out_cov85<-data_t_noout
data_t_out_cov85[[1]][171:200,]=data_t_out_cov[[1]][171:200,]
data_t_out_cov85[[2]][171:200,]=data_t_out_cov[[2]][171:200,]

data_t_out_cov80<-data_t_noout
data_t_out_cov80[[1]][161:200,]=data_t_out_cov[[1]][161:200,]
data_t_out_cov80[[2]][161:200,]=data_t_out_cov[[2]][161:200,]

fdata_t_out_cov95<-mfData(time_grid,data_t_out_cov95)
fdata_t_out_cov90<-mfData(time_grid,data_t_out_cov90)
fdata_t_out_cov85<-mfData(time_grid,data_t_out_cov85)
fdata_t_out_cov80<-mfData(time_grid,data_t_out_cov80)
fdata_t_out_cov0<-mfData(time_grid, data_t_out_cov)
# CONJUNTO DE VALIDACIÓN PARA PRUEBA DE ATÍPICOS

data_t_out_cov95_validation<-data_t_noout_validation
data_t_out_cov90_validation<-data_t_noout_validation
data_t_out_cov85_validation<-data_t_noout_validation
data_t_out_cov80_validation<-data_t_noout_validation

data_t_out_cov_validation<-list()
fdata_t_out_cov95_validation<-list()
fdata_t_out_cov90_validation<-list()
fdata_t_out_cov85_validation<-list()
fdata_t_out_cov80_validation<-list()
# ----

# COVARIANCE OUTLIERS

# 100%
for (i in 1:100){
  data_t_out_cov_validation[[i]]<-generate_cauchy_mfdata(N, L, reg, correlations = 0, listCov = list( C3,C4) )
}
# 5%
for (i in 1:100){
  data_t_out_cov95_validation[[i]][[1]][191:200,]<-data_t_out_cov_validation[[i]][[1]][191:200,]
  data_t_out_cov95_validation[[i]][[2]][191:200,]<-data_t_out_cov_validation[[i]][[2]][191:200,]
  fdata_t_out_cov95_validation[[i]]<-roahd::mfData(time_grid, data_t_out_cov95_validation[[i]])
}
# 10%
for (i in 1:100){
  data_t_out_cov90_validation[[i]][[1]][181:200,]<-data_t_out_cov_validation[[i]][[1]][181:200,]
  data_t_out_cov90_validation[[i]][[2]][181:200,]<-data_t_out_cov_validation[[i]][[2]][181:200,]
  fdata_t_out_cov90_validation[[i]]<-roahd::mfData(time_grid, data_t_out_cov90_validation[[i]])
}
# 15%
for (i in 1:100){
  data_t_out_cov85_validation[[i]][[1]][171:200,]<-data_t_out_cov_validation[[i]][[1]][171:200,]
  data_t_out_cov85_validation[[i]][[2]][171:200,]<-data_t_out_cov_validation[[i]][[2]][171:200,]
  fdata_t_out_cov85_validation[[i]]<-roahd::mfData(time_grid, data_t_out_cov85_validation[[i]])
}
# 20%
for (i in 1:100){
  data_t_out_cov80_validation[[i]][[1]][161:200,]<-data_t_out_cov_validation[[i]][[1]][161:200,]
  data_t_out_cov80_validation[[i]][[2]][161:200,]<-data_t_out_cov_validation[[i]][[2]][161:200,]
  fdata_t_out_cov80_validation[[i]]<-roahd::mfData(time_grid, data_t_out_cov80_validation[[i]])
}


# MIXED OUTLIERS:
# ----
data_t_out_mix<-generate_cauchy_mfdata(N, L, outlier_mix, correlations = 0.7, listCov = list( C1, C2) )

data_t_out_mix95<-data_t_noout
data_t_out_mix95[[1]][191:200,]=data_t_out_mix[[1]][191:200,]
data_t_out_mix95[[2]][191:200,]=data_t_out_mix[[2]][191:200,]

data_t_out_mix90<-data_t_noout
data_t_out_mix90[[1]][181:200,]=data_t_out_mix[[1]][181:200,]
data_t_out_mix90[[2]][181:200,]=data_t_out_mix[[2]][181:200,]

data_t_out_mix85<-data_t_noout
data_t_out_mix85[[1]][171:200,]=data_t_out_mix[[1]][171:200,]
data_t_out_mix85[[2]][171:200,]=data_t_out_mix[[2]][171:200,]

data_t_out_mix80<-data_t_noout
data_t_out_mix80[[1]][161:200,]=data_t_out_mix[[1]][161:200,]
data_t_out_mix80[[2]][161:200,]=data_t_out_mix[[2]][161:200,]

fdata_t_out_mix95<-mfData(time_grid,data_t_out_mix95)
fdata_t_out_mix90<-mfData(time_grid,data_t_out_mix90)
fdata_t_out_mix85<-mfData(time_grid,data_t_out_mix85)
fdata_t_out_mix80<-mfData(time_grid,data_t_out_mix80)
fdata_t_out_mix0<-mfData(time_grid, data_t_out_mix)


# CONJUNTO DE DATOS PARA VALIDACIÓN

data_t_out_mix95_validation<-data_t_noout_validation
data_t_out_mix90_validation<-data_t_noout_validation
data_t_out_mix85_validation<-data_t_noout_validation
data_t_out_mix80_validation<-data_t_noout_validation
data_t_out_mix_validation<-list()

fdata_t_out_mix95_validation<-list()
fdata_t_out_mix90_validation<-list()
fdata_t_out_mix85_validation<-list()
fdata_t_out_mix80_validation<-list()
# ----
# MIXED OUTLIERS

# 100%
for (i in 1:100){
  data_t_out_mix_validation[[i]]<-generate_cauchy_mfdata(N, L, outlier_mix, correlations = 0.7, listCov = list( C1, C2) )
}
# 5%
for (i in 1:100){
  data_t_out_mix95_validation[[i]][[1]][191:200,]<-data_t_out_mix_validation[[i]][[1]][191:200,]
  data_t_out_mix95_validation[[i]][[2]][191:200,]<-data_t_out_mix_validation[[i]][[2]][191:200,]
  fdata_t_out_mix95_validation[[i]]<-roahd::mfData(time_grid, data_t_out_mix95_validation[[i]])
}
# 10%
for (i in 1:100){
  data_t_out_mix90_validation[[i]][[1]][181:200,]<-data_t_out_mix_validation[[i]][[1]][181:200,]
  data_t_out_mix90_validation[[i]][[2]][181:200,]<-data_t_out_mix_validation[[i]][[2]][181:200,]
  fdata_t_out_mix90_validation[[i]]<-roahd::mfData(time_grid, data_t_out_mix90_validation[[i]])
}
# 15%
for (i in 1:100){
  data_t_out_mix85_validation[[i]][[1]][171:200,]<-data_t_out_mix_validation[[i]][[1]][171:200,]
  data_t_out_mix85_validation[[i]][[2]][171:200,]<-data_t_out_mix_validation[[i]][[2]][171:200,]
  fdata_t_out_mix85_validation[[i]]<-roahd::mfData(time_grid, data_t_out_mix85_validation[[i]])
}
# 20%
for (i in 1:100){
  data_t_out_mix80_validation[[i]][[1]][161:200,]<-data_t_out_mix_validation[[i]][[1]][161:200,]
  data_t_out_mix80_validation[[i]][[2]][161:200,]<-data_t_out_mix_validation[[i]][[2]][161:200,]
  fdata_t_out_mix80_validation[[i]]<-roahd::mfData(time_grid, data_t_out_mix80_validation[[i]])
}


#ISOLATED OUTLIERS:
# ----
data_t_out_iso<-generate_cauchy_mfdata(N, L, outlier_iso, correlations = 0.7, listCov = list( C1, C2) )

data_t_out_iso95<-data_t_noout
data_t_out_iso95[[1]][191:200,]=data_t_out_iso[[1]][191:200,]
data_t_out_iso95[[2]][191:200,]=data_t_out_iso[[2]][191:200,]

data_t_out_iso90<-data_t_noout
data_t_out_iso90[[1]][181:200,]=data_t_out_iso[[1]][181:200,]
data_t_out_iso90[[2]][181:200,]=data_t_out_iso[[2]][181:200]

data_t_out_iso85<-data_t_noout
data_t_out_iso85[[1]][171:200,]=data_t_out_iso[[1]][171:200,]
data_t_out_iso85[[2]][171:200,]=data_t_out_iso[[2]][171:200,]

data_t_out_iso80<-data_t_noout
data_t_out_iso80[[1]][161:200,]=data_t_out_iso[[1]][161:200,]
data_t_out_iso80[[2]][161:200,]=data_t_out_iso[[2]][161:200,]

fdata_t_out_iso95<-mfData(time_grid,data_t_out_iso95)
fdata_t_out_iso90<-mfData(time_grid,data_t_out_iso90)
fdata_t_out_iso85<-mfData(time_grid,data_t_out_iso85)
fdata_t_out_iso80<-mfData(time_grid,data_t_out_iso80)
fdata_t_out_iso0<-mfData(time_grid, data_t_out_iso)


# CONJUNTO DE DATOS PARA VALIDACIÓN

data_t_out_iso95_validation<-data_t_noout_validation
data_t_out_iso90_validation<-data_t_noout_validation
data_t_out_iso85_validation<-data_t_noout_validation
data_t_out_iso80_validation<-data_t_noout_validation

data_t_out_iso_validation<-list()
fdata_t_out_iso95_validation<-list()
fdata_t_out_iso90_validation<-list()
fdata_t_out_iso85_validation<-list()
fdata_t_out_iso80_validation<-list()
# ----
# ISOLATED OUTLIERS - VALIDATION
# 100%
for (i in 1:100){
  data_t_out_iso_validation[[i]]<-generate_cauchy_mfdata(N, L, outlier_iso, correlations = 0.7, listCov = list( C1, C2) )
}
# 5%
for (i in 1:100){
  data_t_out_iso95_validation[[i]][[1]][191:200,]<-data_t_out_iso_validation[[i]][[1]][191:200,]
  data_t_out_iso95_validation[[i]][[2]][191:200,]<-data_t_out_iso_validation[[i]][[2]][191:200,]
  fdata_t_out_iso95_validation[[i]]<-roahd::mfData(time_grid, data_t_out_iso95_validation[[i]])
}
# 10%
for (i in 1:100){
  data_t_out_iso90_validation[[i]][[1]][181:200,]<-data_t_out_iso_validation[[i]][[1]][181:200,]
  data_t_out_iso90_validation[[i]][[2]][181:200,]<-data_t_out_iso_validation[[i]][[2]][181:200,]
  fdata_t_out_iso90_validation[[i]]<-roahd::mfData(time_grid, data_t_out_iso90_validation[[i]])
}
# 15%
for (i in 1:100){
  data_t_out_iso85_validation[[i]][[1]][171:200,]<-data_t_out_iso_validation[[i]][[1]][171:200,]
  data_t_out_iso85_validation[[i]][[2]][171:200,]<-data_t_out_iso_validation[[i]][[2]][171:200,]
  fdata_t_out_iso85_validation[[i]]<-roahd::mfData(time_grid, data_t_out_iso85_validation[[i]])
}
# 20%
for (i in 1:100){
  data_t_out_iso80_validation[[i]][[1]][161:200,]<-data_t_out_iso_validation[[i]][[1]][161:200,]
  data_t_out_iso80_validation[[i]][[2]][161:200,]<-data_t_out_iso_validation[[i]][[2]][161:200,]
  fdata_t_out_iso80_validation[[i]]<-roahd::mfData(time_grid, data_t_out_iso80_validation[[i]])
}

# TREND OUTLIERS
# ----
data_t_out_trend<-generate_gauss_mfdata(N, L, outlier_trend, correlations = 0.7, listCov = list( C1, C2) )

data_t_out_trend95<-data_noout
data_t_out_trend95[[1]][191:200,]=data_t_out_trend[[1]][191:200,]
data_t_out_trend95[[2]][191:200,]=data_t_out_trend[[2]][191:200,]

data_t_out_trend90<-data_noout
data_t_out_trend90[[1]][181:200,]=data_t_out_trend[[1]][181:200,]
data_t_out_trend90[[2]][181:200,]=data_t_out_trend[[2]][181:200,]

data_t_out_trend85<-data_noout
data_t_out_trend85[[1]][171:200,]=data_t_out_trend[[1]][171:200,]
data_t_out_trend85[[2]][171:200,]=data_t_out_trend[[2]][171:200,]

data_t_out_trend80<-data_noout
data_t_out_trend80[[1]][161:200,]=data_t_out_trend[[1]][161:200,]
data_t_out_trend80[[2]][161:200,]=data_t_out_trend[[2]][161:200,]

fdata_t_out_trend95<-mfData(time_grid,data_t_out_trend95)
fdata_t_out_trend90<-mfData(time_grid,data_t_out_trend90)
fdata_t_out_trend85<-mfData(time_grid,data_t_out_trend85)
fdata_t_out_trend80<-mfData(time_grid,data_t_out_trend80)
fdata_t_out_trend0<-mfData(time_grid, data_t_out_trend)


# CONJUNTO DE DATOS PARA VALIDACIÓN

data_t_out_trend95_validation<-data_t_noout_validation
data_t_out_trend90_validation<-data_t_noout_validation
data_t_out_trend85_validation<-data_t_noout_validation
data_t_out_trend80_validation<-data_t_noout_validation
data_t_out_trend_validation<-list()

fdata_t_out_trend95_validation<-list()
fdata_t_out_trend90_validation<-list()
fdata_t_out_trend85_validation<-list()
fdata_t_out_trend80_validation<-list()
# ----
# TREND OUTLIERS

# 100%
for (i in 1:100){
  data_t_out_trend_validation[[i]]<-generate_gauss_mfdata(N, L, outlier_trend, correlations = 0.7, listCov = list( C1, C2) )
}
# 5%
for (i in 1:100){
  data_t_out_trend95_validation[[i]][[1]][191:200,]<-data_t_out_trend_validation[[i]][[1]][191:200,]
  data_t_out_trend95_validation[[i]][[2]][191:200,]<-data_t_out_trend_validation[[i]][[2]][191:200,]
  fdata_t_out_trend95_validation[[i]]<-roahd::mfData(time_grid, data_t_out_trend95_validation[[i]])
}
# 10%
for (i in 1:100){
  data_t_out_trend90_validation[[i]][[1]][181:200,]<-data_t_out_trend_validation[[i]][[1]][181:200,]
  data_t_out_trend90_validation[[i]][[2]][181:200,]<-data_t_out_trend_validation[[i]][[2]][181:200,]
  fdata_t_out_trend90_validation[[i]]<-roahd::mfData(time_grid, data_t_out_trend90_validation[[i]])
}
# 15%
for (i in 1:100){
  data_t_out_trend85_validation[[i]][[1]][171:200,]<-data_t_out_trend_validation[[i]][[1]][171:200,]
  data_t_out_trend85_validation[[i]][[2]][171:200,]<-data_t_out_trend_validation[[i]][[2]][171:200,]
  fdata_t_out_trend85_validation[[i]]<-roahd::mfData(time_grid, data_t_out_trend85_validation[[i]])
}
# 20%
for (i in 1:100){
  data_t_out_trend80_validation[[i]][[1]][161:200,]<-data_t_out_trend_validation[[i]][[1]][161:200,]
  data_t_out_trend80_validation[[i]][[2]][161:200,]<-data_t_out_trend_validation[[i]][[2]][161:200,]
  fdata_t_out_trend80_validation[[i]]<-roahd::mfData(time_grid, data_t_out_trend80_validation[[i]])
}




# PLOTS

# ---- 
plot_t_noout=plot(fdata_t_out_cov80_validation[[1]], main = list("Dataset without outliers, variable 1", "Dataset without outliers, variable 2"), col=col80, bty="n", cex.main=0.7, cex.axis=0.7)

# MAGNITUDE
plot(fdata_t_out_mag80, main = list("80% Magnitude outliers, variable 1", "80% Magnitude outliers, variable 2"), col=col80, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_mag85=plot(fdata_t_out_mag85, main = list("85% Magnitude outliers, variable 1", "85% Magnitude outliers, variable 2"), col=col85, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_mag90=plot(fdata_t_out_mag90, main = list("90% Magnitude outliers, variable 1", "90% Magnitude outliers, variable 2"), col=col90, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_mag95=plot(fdata_t_out_mag95, main = list("95% Magnitude outliers, variable 1", "95% Magnitude outliers, variable 2"), col=col95, bty="n", cex.main=0.7, cex.axis=0.7)

# SHAPE

plot_t_shape80=plot(fdata_t_out_shape80, main = list("80% Shape outliers, variable 1", "80% Shape outliers, variable 2"), col=col80, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_shape85=plot(fdata_t_out_shape85, main = list("85% Shape outliers, variable 1", "85% Shape outliers, variable 2"), col=col85, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_shape90=plot(fdata_t_out_shape90, main = list("90% Shape outliers, variable 1", "90% Shape outliers, variable 2"), col=col90, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_shape95=plot(fdata_t_out_shape95, main = list("95% Shape outliers, variable 1", "95% Shape outliers, variable 2"), col=col95, bty="n", cex.main=0.7, cex.axis=0.7)

# COVARIANCE

plot_t_cov80=plot(fdata_t_out_cov80, main = list("80% Covariance outliers, variable 1", "80% Covariance outliers, variable 2"), col=col80, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_cov85=plot(fdata_t_out_cov85, main = list("85% Covariance outliers, variable 1", "85% Covariance outliers, variable 2"), col=col85, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_cov90=plot(fdata_t_out_cov90, main = list("90% Covariance outliers, variable 1", "90% Covariance outliers, variable 2"), col=col90, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_cov95=plot(fdata_t_out_cov95, main = list("95% Covariance outliers, variable 1", "95% Covariance outliers, variable 2"), col=col95, bty="n", cex.main=0.7, cex.axis=0.7)

# MIXED

plot_t_mix80=plot(fdata_t_out_mix80, main = list("80% Mixed outliers, variable 1", "80% Mixed outliers, variable 2"), col=col80, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_mix85=plot(fdata_t_out_mix85, main = list("85% Mixed outliers, variable 1", "85% Mixed outliers, variable 2"), col=col85, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_mix90=plot(fdata_t_out_mix90, main = list("90% Mixed outliers, variable 1", "90% Mixed outliers, variable 2"), col=col90, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_mix95=plot(fdata_t_out_mix95, main = list("95% Mixed outliers, variable 1", "95% Mixed outliers, variable 2"), col=col95, bty="n", cex.main=0.7, cex.axis=0.7)

# ISOLATED

plot_t_iso80=plot(fdata_t_out_iso80, main = list("80% Isolated outliers, variable 1", "80% Isolated outliers, variable 2"), col=col80, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_iso85=plot(fdata_t_out_iso85, main = list("85% Isolated outliers, variable 1", "85% Isolated outliers, variable 2"), col=col85, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_iso90=plot(fdata_t_out_iso90, main = list("90% Isolated outliers, variable 1", "90% Isolated outliers, variable 2"), col=col90, bty="n", cex.main=0.7, cex.axis=0.7)
plot_t_iso95=plot(fdata_t_out_iso95, main = list("95% Isolated outliers, variable 1", "95% Isolated outliers, variable 2"), col=col95, bty="n", cex.main=0.7, cex.axis=0.7)
# ----

# IDENTIFICADORES DE ATÍPICOS
