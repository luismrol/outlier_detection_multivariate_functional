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
  data_long<-reshape2::melt(data)
  return(data.frame(data_long))
}

# FUNCTION FOR GENERATING HEAVY-TAILED DATASETS

generate_cauchy_mfdata<-function (N, L, centerline, correlations, listCov = NULL, listCholCov = NULL) {
  if (length(correlations) != 0.5 * (L) * (L - 1)) {
    stop("Error in generate_gauss_mfdata: you have to provide all the\n          correlations among functional components")
  }
  if (nrow(centerline) != L) {
    stop("Error in generate_gauss_mfdata: you have to provide a centerline for\neach dimension")
  }
  if (is.null(listCov) & is.null(listCholCov)) {
    stop("Error: You have to provide at least either covariance matrices or\n          their cholesky factors to generate_gauss_mfdata")
  }
  else if (!is.null(listCholCov)) {
    if (length(listCholCov) != L) {
      stop("Error: You have to provide a covariance Cholesky factor for each\n              dimension")
    }
    P = ncol(listCholCov[[1]])
    if (ncol(centerline) != P | any(sapply(listCholCov, nrow) != 
                                    P) | any(sapply(listCholCov, ncol) != P)) {
      stop("Error: You provided mismatching centerline and covariance\nmatrices Cholesky factors to generate_gauss_mfdata")
    }
  }
  else if (!is.null(listCov)) {
    P = ncol(listCov[[1]])
    if (ncol(centerline) != P | any(sapply(listCov, nrow) != 
                                    P) | any(sapply(listCov, ncol) != P)) {
      stop("Error: You provided mismatching centerline and covariance\nmatrices to generate_gauss_mfdata")
    }
    listCholCov = lapply(listCov, chol)
  }
  R = matrix(1, ncol = L, nrow = L)
  R[upper.tri(R)] = as.numeric(correlations)
  R[lower.tri(R)] = as.numeric(correlations)
  R_chol = chol(R)
  Data = matrix(rt(N * L * P, 1), ncol = L, nrow = N * P)
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

data_noout_validation<-list()
for (i in 1:100){
  data_noout_validation[[i]]<-generate_gauss_mfdata(N, L, reg, correlations = 0.7, listCov = list( C1, C2) )
  }


data_out_mag95_validation<-data_noout_validation
data_out_mag90_validation<-data_noout_validation
data_out_mag85_validation<-data_noout_validation
data_out_mag80_validation<-data_noout_validation

fdata_out_mag95_validation<-list()
fdata_out_mag90_validation<-list()
fdata_out_mag85_validation<-list()
fdata_out_mag80_validation<-list()


data_noout<-generate_gauss_mfdata(N, L, reg, correlations = 0.7, listCov = list( C1, C2) )

data_out_mag95<-data_noout
data_out_mag95[[1]][191:200,]=data_noout[[1]][191:200,]+outlier_mag_x[191:200]
data_out_mag95[[2]][191:200,]=data_noout[[2]][191:200,]+outlier_mag_y[191:200]

data_out_mag90<-data_noout
data_out_mag90[[1]][181:200,]=data_noout[[1]][181:200,]+outlier_mag_x[181:200]
data_out_mag90[[2]][181:200,]=data_noout[[2]][181:200,]+outlier_mag_y[181:200]

data_out_mag85<-data_noout
data_out_mag85[[1]][171:200,]=data_noout[[1]][171:200,]+outlier_mag_x[171:200]
data_out_mag85[[2]][171:200,]=data_noout[[2]][171:200,]+outlier_mag_y[171:200]

data_out_mag80<-data_noout
data_out_mag80[[1]][161:200,]=data_noout[[1]][161:200,]+outlier_mag_x[161:200]
data_out_mag80[[2]][161:200,]=data_noout[[2]][161:200,]+outlier_mag_y[161:200]


fdata_noout<-mfData(time_grid,data_noout)

fdata_out_mag95<-mfData(time_grid,data_out_mag95)
fdata_out_mag90<-mfData(time_grid,data_out_mag90)
fdata_out_mag85<-mfData(time_grid,data_out_mag85)
fdata_out_mag80<-mfData(time_grid,data_out_mag80)
# ----
# GENERAR LAS BASES DE DATOS PARA LA PRUEBA
# 5%
for (i in 1:100){
  data_out_mag95_validation[[i]][[1]][191:200,]<-data_noout_validation[[i]][[1]][191:200,]+outlier_mag_x[191:200]
  data_out_mag95_validation[[i]][[2]][191:200,]<-data_noout_validation[[i]][[2]][191:200,]+outlier_mag_y[191:200]
  fdata_out_mag95_validation[[i]]<-roahd::mfData(time_grid, data_out_mag95_validation[[i]])
}
# 10%
for (i in 1:100){
  data_out_mag90_validation[[i]][[1]][181:200,]<-data_noout_validation[[i]][[1]][181:200,]+outlier_mag_x[181:200]
  data_out_mag90_validation[[i]][[2]][181:200,]<-data_noout_validation[[i]][[2]][181:200,]+outlier_mag_y[181:200]
  fdata_out_mag90_validation[[i]]<-roahd::mfData(time_grid, data_out_mag90_validation[[i]])
}
# 15%
for (i in 1:100){
  data_out_mag85_validation[[i]][[1]][171:200,]<-data_noout_validation[[i]][[1]][171:200,]+outlier_mag_x[171:200]
  data_out_mag85_validation[[i]][[2]][171:200,]<-data_noout_validation[[i]][[2]][171:200,]+outlier_mag_y[171:200]
  fdata_out_mag85_validation[[i]]<-roahd::mfData(time_grid, data_out_mag85_validation[[i]])
  
}
# 20%
for (i in 1:100){
  data_out_mag80_validation[[i]][[1]][161:200,]<-data_noout_validation[[i]][[1]][161:200,]+outlier_mag_x[161:200]
  data_out_mag80_validation[[i]][[2]][161:200,]<-data_noout_validation[[i]][[2]][161:200,]+outlier_mag_y[161:200]
  fdata_out_mag80_validation[[i]]<-roahd::mfData(time_grid, data_out_mag80_validation[[i]])
  
}


# SHAPE OUTLIERS
# ----
data_out_shape<-generate_gauss_mfdata(N, L, outlier_shape, correlations = 0.7, listCov = list( C1, C2) )

data_out_shape95<-data_noout
data_out_shape95[[1]][191:200,]=data_out_shape[[1]][191:200,]
data_out_shape95[[2]][191:200,]=data_out_shape[[2]][191:200,]

data_out_shape90<-data_noout
data_out_shape90[[1]][181:200,]=data_out_shape[[1]][181:200,]
data_out_shape90[[2]][181:200,]=data_out_shape[[2]][181:200,]

data_out_shape85<-data_noout
data_out_shape85[[1]][171:200,]=data_out_shape[[1]][171:200,]
data_out_shape85[[2]][171:200,]=data_out_shape[[2]][171:200,]

data_out_shape80<-data_noout
data_out_shape80[[1]][161:200,]=data_out_shape[[1]][161:200,]
data_out_shape80[[2]][161:200,]=data_out_shape[[2]][161:200,]

fdata_out_shape95<-mfData(time_grid,data_out_shape95)
fdata_out_shape90<-mfData(time_grid,data_out_shape90)
fdata_out_shape85<-mfData(time_grid,data_out_shape85)
fdata_out_shape80<-mfData(time_grid,data_out_shape80)
fdata_out_shape0<-mfData(time_grid, data_out_shape)


# CONJUNTOS DE VALIDACIÓN PARA PRUEBA DE ATÍPICOS

data_out_shape95_validation<-data_noout_validation
data_out_shape90_validation<-data_noout_validation
data_out_shape85_validation<-data_noout_validation
data_out_shape80_validation<-data_noout_validation

data_out_shape_validation<-list()
fdata_out_shape95_validation<-list()
fdata_out_shape90_validation<-list()
fdata_out_shape85_validation<-list()
fdata_out_shape80_validation<-list()
# ----
# SHAPE OUTLIERS
# 100%
for (i in 1:100){
  data_out_shape_validation[[i]]<-generate_gauss_mfdata(N, L, outlier_shape, correlations = 0.7, listCov = list( C1, C2) )
}
# 5%
for (i in 1:100){
  data_out_shape95_validation[[i]][[1]][191:200,]<-data_out_shape_validation[[i]][[1]][191:200,]
  data_out_shape95_validation[[i]][[2]][191:200,]<-data_out_shape_validation[[i]][[2]][191:200,]
  fdata_out_shape95_validation[[i]]<-roahd::mfData(time_grid, data_out_shape95_validation[[i]])
}
# 10%
for (i in 1:100){
  data_out_shape90_validation[[i]][[1]][181:200,]<-data_out_shape_validation[[i]][[1]][181:200,]
  data_out_shape90_validation[[i]][[2]][181:200,]<-data_out_shape_validation[[i]][[2]][181:200,]
  fdata_out_shape90_validation[[i]]<-roahd::mfData(time_grid, data_out_shape90_validation[[i]])
}
# 15%
for (i in 1:100){
  data_out_shape85_validation[[i]][[1]][171:200,]<-data_out_shape_validation[[i]][[1]][171:200,]
  data_out_shape85_validation[[i]][[2]][171:200,]<-data_out_shape_validation[[i]][[2]][171:200,]
  fdata_out_shape85_validation[[i]]<-roahd::mfData(time_grid, data_out_shape85_validation[[i]])
}
# 20%
for (i in 1:100){
  data_out_shape80_validation[[i]][[1]][161:200,]<-data_out_shape_validation[[i]][[1]][161:200,]
  data_out_shape80_validation[[i]][[2]][161:200,]<-data_out_shape_validation[[i]][[2]][161:200,]
  fdata_out_shape80_validation[[i]]<-roahd::mfData(time_grid, data_out_shape80_validation[[i]])
}



# COVARIANCE OUTLIERS
# ----
data_out_cov<-generate_gauss_mfdata(N, L, reg, correlations = 0, listCov = list( C3,C4) )

data_out_cov95<-data_noout
data_out_cov95[[1]][191:200,]=data_out_cov[[1]][191:200,]
data_out_cov95[[2]][191:200,]=data_out_cov[[2]][191:200,]

data_out_cov90<-data_noout
data_out_cov90[[1]][181:200,]=data_out_cov[[1]][181:200,]
data_out_cov90[[2]][181:200,]=data_out_cov[[2]][181:200,]

data_out_cov85<-data_noout
data_out_cov85[[1]][171:200,]=data_out_cov[[1]][171:200,]
data_out_cov85[[2]][171:200,]=data_out_cov[[2]][171:200,]

data_out_cov80<-data_noout
data_out_cov80[[1]][161:200,]=data_out_cov[[1]][161:200,]
data_out_cov80[[2]][161:200,]=data_out_cov[[2]][161:200,]

fdata_out_cov95<-mfData(time_grid,data_out_cov95)
fdata_out_cov90<-mfData(time_grid,data_out_cov90)
fdata_out_cov85<-mfData(time_grid,data_out_cov85)
fdata_out_cov80<-mfData(time_grid,data_out_cov80)
fdata_out_cov0<-mfData(time_grid, data_out_cov)
# CONJUNTO DE VALIDACIÓN PARA PRUEBA DE ATÍPICOS

data_out_cov95_validation<-data_noout_validation
data_out_cov90_validation<-data_noout_validation
data_out_cov85_validation<-data_noout_validation
data_out_cov80_validation<-data_noout_validation

data_out_cov_validation<-list()
fdata_out_cov95_validation<-list()
fdata_out_cov90_validation<-list()
fdata_out_cov85_validation<-list()
fdata_out_cov80_validation<-list()
# ----

# COVARIANCE OUTLIERS

# 100%
for (i in 1:100){
  data_out_cov_validation[[i]]<-generate_gauss_mfdata(N, L, reg, correlations = 0, listCov = list( C3,C4) )
}
# 5%
for (i in 1:100){
  data_out_cov95_validation[[i]][[1]][191:200,]<-data_out_cov_validation[[i]][[1]][191:200,]
  data_out_cov95_validation[[i]][[2]][191:200,]<-data_out_cov_validation[[i]][[2]][191:200,]
  fdata_out_cov95_validation[[i]]<-roahd::mfData(time_grid, data_out_cov95_validation[[i]])
}
# 10%
for (i in 1:100){
  data_out_cov90_validation[[i]][[1]][181:200,]<-data_out_cov_validation[[i]][[1]][181:200,]
  data_out_cov90_validation[[i]][[2]][181:200,]<-data_out_cov_validation[[i]][[2]][181:200,]
  fdata_out_cov90_validation[[i]]<-roahd::mfData(time_grid, data_out_cov90_validation[[i]])
}
# 15%
for (i in 1:100){
  data_out_cov85_validation[[i]][[1]][171:200,]<-data_out_cov_validation[[i]][[1]][171:200,]
  data_out_cov85_validation[[i]][[2]][171:200,]<-data_out_cov_validation[[i]][[2]][171:200,]
  fdata_out_cov85_validation[[i]]<-roahd::mfData(time_grid, data_out_cov85_validation[[i]])
}
# 20%
for (i in 1:100){
  data_out_cov80_validation[[i]][[1]][161:200,]<-data_out_cov_validation[[i]][[1]][161:200,]
  data_out_cov80_validation[[i]][[2]][161:200,]<-data_out_cov_validation[[i]][[2]][161:200,]
  fdata_out_cov80_validation[[i]]<-roahd::mfData(time_grid, data_out_cov80_validation[[i]])
}


# MIXED OUTLIERS:
# ----
data_out_mix<-generate_gauss_mfdata(N, L, outlier_mix, correlations = 0.7, listCov = list( C1, C2) )

data_out_mix95<-data_noout
data_out_mix95[[1]][191:200,]=data_out_mix[[1]][191:200,]
data_out_mix95[[2]][191:200,]=data_out_mix[[2]][191:200,]

data_out_mix90<-data_noout
data_out_mix90[[1]][181:200,]=data_out_mix[[1]][181:200,]
data_out_mix90[[2]][181:200,]=data_out_mix[[2]][181:200,]

data_out_mix85<-data_noout
data_out_mix85[[1]][171:200,]=data_out_mix[[1]][171:200,]
data_out_mix85[[2]][171:200,]=data_out_mix[[2]][171:200,]

data_out_mix80<-data_noout
data_out_mix80[[1]][161:200,]=data_out_mix[[1]][161:200,]
data_out_mix80[[2]][161:200,]=data_out_mix[[2]][161:200,]

fdata_out_mix95<-mfData(time_grid,data_out_mix95)
fdata_out_mix90<-mfData(time_grid,data_out_mix90)
fdata_out_mix85<-mfData(time_grid,data_out_mix85)
fdata_out_mix80<-mfData(time_grid,data_out_mix80)
fdata_out_mix0<-mfData(time_grid, data_out_mix)


# CONJUNTO DE DATOS PARA VALIDACIÓN

data_out_mix95_validation<-data_noout_validation
data_out_mix90_validation<-data_noout_validation
data_out_mix85_validation<-data_noout_validation
data_out_mix80_validation<-data_noout_validation
data_out_mix_validation<-list()

fdata_out_mix95_validation<-list()
fdata_out_mix90_validation<-list()
fdata_out_mix85_validation<-list()
fdata_out_mix80_validation<-list()
# ----
# MIXED OUTLIERS

# 100%
for (i in 1:100){
  data_out_mix_validation[[i]]<-generate_gauss_mfdata(N, L, outlier_mix, correlations = 0.7, listCov = list( C1, C2) )
}
# 5%
for (i in 1:100){
  data_out_mix95_validation[[i]][[1]][191:200,]<-data_out_mix_validation[[i]][[1]][191:200,]
  data_out_mix95_validation[[i]][[2]][191:200,]<-data_out_mix_validation[[i]][[2]][191:200,]
  fdata_out_mix95_validation[[i]]<-roahd::mfData(time_grid, data_out_mix95_validation[[i]])
}
# 10%
for (i in 1:100){
  data_out_mix90_validation[[i]][[1]][181:200,]<-data_out_mix_validation[[i]][[1]][181:200,]
  data_out_mix90_validation[[i]][[2]][181:200,]<-data_out_mix_validation[[i]][[2]][181:200,]
  fdata_out_mix90_validation[[i]]<-roahd::mfData(time_grid, data_out_mix90_validation[[i]])
}
# 15%
for (i in 1:100){
  data_out_mix85_validation[[i]][[1]][171:200,]<-data_out_mix_validation[[i]][[1]][171:200,]
  data_out_mix85_validation[[i]][[2]][171:200,]<-data_out_mix_validation[[i]][[2]][171:200,]
  fdata_out_mix85_validation[[i]]<-roahd::mfData(time_grid, data_out_mix85_validation[[i]])
}
# 20%
for (i in 1:100){
  data_out_mix80_validation[[i]][[1]][161:200,]<-data_out_mix_validation[[i]][[1]][161:200,]
  data_out_mix80_validation[[i]][[2]][161:200,]<-data_out_mix_validation[[i]][[2]][161:200,]
  fdata_out_mix80_validation[[i]]<-roahd::mfData(time_grid, data_out_mix80_validation[[i]])
}


#ISOLATED OUTLIERS:
# ----
data_out_iso<-generate_gauss_mfdata(N, L, outlier_iso, correlations = 0.7, listCov = list( C1, C2) )

data_out_iso95<-data_noout
data_out_iso95[[1]][191:200,]=data_out_iso[[1]][191:200,]
data_out_iso95[[2]][191:200,]=data_out_iso[[2]][191:200,]

data_out_iso90<-data_noout
data_out_iso90[[1]][181:200,]=data_out_iso[[1]][181:200,]
data_out_iso90[[2]][181:200,]=data_out_iso[[2]][181:200]

data_out_iso85<-data_noout
data_out_iso85[[1]][171:200,]=data_out_iso[[1]][171:200,]
data_out_iso85[[2]][171:200,]=data_out_iso[[2]][171:200,]

data_out_iso80<-data_noout
data_out_iso80[[1]][161:200,]=data_out_iso[[1]][161:200,]
data_out_iso80[[2]][161:200,]=data_out_iso[[2]][161:200,]

fdata_out_iso95<-mfData(time_grid,data_out_iso95)
fdata_out_iso90<-mfData(time_grid,data_out_iso90)
fdata_out_iso85<-mfData(time_grid,data_out_iso85)
fdata_out_iso80<-mfData(time_grid,data_out_iso80)
fdata_out_iso0<-mfData(time_grid, data_out_iso)


# CONJUNTO DE DATOS PARA VALIDACIÓN

data_out_iso95_validation<-data_noout_validation
data_out_iso90_validation<-data_noout_validation
data_out_iso85_validation<-data_noout_validation
data_out_iso80_validation<-data_noout_validation

data_out_iso_validation<-list()
fdata_out_iso95_validation<-list()
fdata_out_iso90_validation<-list()
fdata_out_iso85_validation<-list()
fdata_out_iso80_validation<-list()
# ----
# ISOLATED OUTLIERS - VALIDATION
# 100%
for (i in 1:100){
  data_out_iso_validation[[i]]<-generate_gauss_mfdata(N, L, outlier_iso, correlations = 0.7, listCov = list( C1, C2) )
}
# 5%
for (i in 1:100){
  data_out_iso95_validation[[i]][[1]][191:200,]<-data_out_iso_validation[[i]][[1]][191:200,]
  data_out_iso95_validation[[i]][[2]][191:200,]<-data_out_iso_validation[[i]][[2]][191:200,]
  fdata_out_iso95_validation[[i]]<-roahd::mfData(time_grid, data_out_iso95_validation[[i]])
}
# 10%
for (i in 1:100){
  data_out_iso90_validation[[i]][[1]][181:200,]<-data_out_iso_validation[[i]][[1]][181:200,]
  data_out_iso90_validation[[i]][[2]][181:200,]<-data_out_iso_validation[[i]][[2]][181:200,]
  fdata_out_iso90_validation[[i]]<-roahd::mfData(time_grid, data_out_iso90_validation[[i]])
}
# 15%
for (i in 1:100){
  data_out_iso85_validation[[i]][[1]][171:200,]<-data_out_iso_validation[[i]][[1]][171:200,]
  data_out_iso85_validation[[i]][[2]][171:200,]<-data_out_iso_validation[[i]][[2]][171:200,]
  fdata_out_iso85_validation[[i]]<-roahd::mfData(time_grid, data_out_iso85_validation[[i]])
}
# 20%
for (i in 1:100){
  data_out_iso80_validation[[i]][[1]][161:200,]<-data_out_iso_validation[[i]][[1]][161:200,]
  data_out_iso80_validation[[i]][[2]][161:200,]<-data_out_iso_validation[[i]][[2]][161:200,]
  fdata_out_iso80_validation[[i]]<-roahd::mfData(time_grid, data_out_iso80_validation[[i]])
}

# TREND OUTLIERS
# ----
data_out_trend<-generate_gauss_mfdata(N, L, outlier_trend, correlations = 0.7, listCov = list( C1, C2) )

data_out_trend95<-data_noout
data_out_trend95[[1]][191:200,]=data_out_trend[[1]][191:200,]
data_out_trend95[[2]][191:200,]=data_out_trend[[2]][191:200,]

data_out_trend90<-data_noout
data_out_trend90[[1]][181:200,]=data_out_trend[[1]][181:200,]
data_out_trend90[[2]][181:200,]=data_out_trend[[2]][181:200,]

data_out_trend85<-data_noout
data_out_trend85[[1]][171:200,]=data_out_trend[[1]][171:200,]
data_out_trend85[[2]][171:200,]=data_out_trend[[2]][171:200,]

data_out_trend80<-data_noout
data_out_trend80[[1]][161:200,]=data_out_trend[[1]][161:200,]
data_out_trend80[[2]][161:200,]=data_out_trend[[2]][161:200,]

fdata_out_trend95<-mfData(time_grid,data_out_trend95)
fdata_out_trend90<-mfData(time_grid,data_out_trend90)
fdata_out_trend85<-mfData(time_grid,data_out_trend85)
fdata_out_trend80<-mfData(time_grid,data_out_trend80)
fdata_out_trend0<-mfData(time_grid, data_out_trend)


# CONJUNTO DE DATOS PARA VALIDACIÓN

data_out_trend95_validation<-data_noout_validation
data_out_trend90_validation<-data_noout_validation
data_out_trend85_validation<-data_noout_validation
data_out_trend80_validation<-data_noout_validation
data_out_trend_validation<-list()

fdata_out_trend95_validation<-list()
fdata_out_trend90_validation<-list()
fdata_out_trend85_validation<-list()
fdata_out_trend80_validation<-list()
# ----
# TREND OUTLIERS

# 100%
for (i in 1:100){
  data_out_trend_validation[[i]]<-generate_gauss_mfdata(N, L, outlier_trend, correlations = 0.7, listCov = list( C1, C2) )
}
# 5%
for (i in 1:100){
  data_out_trend95_validation[[i]][[1]][191:200,]<-data_out_trend_validation[[i]][[1]][191:200,]
  data_out_trend95_validation[[i]][[2]][191:200,]<-data_out_trend_validation[[i]][[2]][191:200,]
  fdata_out_trend95_validation[[i]]<-roahd::mfData(time_grid, data_out_trend95_validation[[i]])
}
# 10%
for (i in 1:100){
  data_out_trend90_validation[[i]][[1]][181:200,]<-data_out_trend_validation[[i]][[1]][181:200,]
  data_out_trend90_validation[[i]][[2]][181:200,]<-data_out_trend_validation[[i]][[2]][181:200,]
  fdata_out_trend90_validation[[i]]<-roahd::mfData(time_grid, data_out_trend90_validation[[i]])
}
# 15%
for (i in 1:100){
  data_out_trend85_validation[[i]][[1]][171:200,]<-data_out_trend_validation[[i]][[1]][171:200,]
  data_out_trend85_validation[[i]][[2]][171:200,]<-data_out_trend_validation[[i]][[2]][171:200,]
  fdata_out_trend85_validation[[i]]<-roahd::mfData(time_grid, data_out_trend85_validation[[i]])
}
# 20%
for (i in 1:100){
  data_out_trend80_validation[[i]][[1]][161:200,]<-data_out_trend_validation[[i]][[1]][161:200,]
  data_out_trend80_validation[[i]][[2]][161:200,]<-data_out_trend_validation[[i]][[2]][161:200,]
  fdata_out_trend80_validation[[i]]<-roahd::mfData(time_grid, data_out_trend80_validation[[i]])
}




# PLOTS

# ---- 
plot_noout=plot(fdata_out_cov80_validation[[1]], main = list("Dataset without outliers, variable 1", "Dataset without outliers, variable 2"), col=col80, bty="n", cex.main=0.7, cex.axis=0.7)

# MAGNITUDE
plot_mag80=plot(fdata_out_mag80, main = list("80% Magnitude outliers, variable 1", "80% Magnitude outliers, variable 2"), col=col80, bty="n", cex.main=0.7, cex.axis=0.7)
plot_mag85=plot(fdata_out_mag85, main = list("85% Magnitude outliers, variable 1", "85% Magnitude outliers, variable 2"), col=col85, bty="n", cex.main=0.7, cex.axis=0.7)
plot_mag90=plot(fdata_out_mag90, main = list("90% Magnitude outliers, variable 1", "90% Magnitude outliers, variable 2"), col=col90, bty="n", cex.main=0.7, cex.axis=0.7)
plot_mag95=plot(fdata_out_mag95, main = list("95% Magnitude outliers, variable 1", "95% Magnitude outliers, variable 2"), col=col95, bty="n", cex.main=0.7, cex.axis=0.7)

# SHAPE

plot_shape80=plot(fdata_out_shape80, main = list("80% Shape outliers, variable 1", "80% Shape outliers, variable 2"), col=col80, bty="n", cex.main=0.7, cex.axis=0.7)
plot_shape85=plot(fdata_out_shape85, main = list("85% Shape outliers, variable 1", "85% Shape outliers, variable 2"), col=col85, bty="n", cex.main=0.7, cex.axis=0.7)
plot_shape90=plot(fdata_out_shape90, main = list("90% Shape outliers, variable 1", "90% Shape outliers, variable 2"), col=col90, bty="n", cex.main=0.7, cex.axis=0.7)
plot_shape95=plot(fdata_out_shape95, main = list("95% Shape outliers, variable 1", "95% Shape outliers, variable 2"), col=col95, bty="n", cex.main=0.7, cex.axis=0.7)

# COVARIANCE

plot_cov80=plot(fdata_out_cov80, main = list("80% Covariance outliers, variable 1", "80% Covariance outliers, variable 2"), col=col80, bty="n", cex.main=0.7, cex.axis=0.7)
plot_cov85=plot(fdata_out_cov85, main = list("85% Covariance outliers, variable 1", "85% Covariance outliers, variable 2"), col=col85, bty="n", cex.main=0.7, cex.axis=0.7)
plot_cov90=plot(fdata_out_cov90, main = list("90% Covariance outliers, variable 1", "90% Covariance outliers, variable 2"), col=col90, bty="n", cex.main=0.7, cex.axis=0.7)
plot_cov95=plot(fdata_out_cov95, main = list("95% Covariance outliers, variable 1", "95% Covariance outliers, variable 2"), col=col95, bty="n", cex.main=0.7, cex.axis=0.7)

# MIXED

plot_mix80=plot(fdata_out_mix80, main = list("80% Mixed outliers, variable 1", "80% Mixed outliers, variable 2"), col=col80, bty="n", cex.main=0.7, cex.axis=0.7)
plot_mix85=plot(fdata_out_mix85, main = list("85% Mixed outliers, variable 1", "85% Mixed outliers, variable 2"), col=col85, bty="n", cex.main=0.7, cex.axis=0.7)
plot_mix90=plot(fdata_out_mix90, main = list("90% Mixed outliers, variable 1", "90% Mixed outliers, variable 2"), col=col90, bty="n", cex.main=0.7, cex.axis=0.7)
plot_mix95=plot(fdata_out_mix95, main = list("95% Mixed outliers, variable 1", "95% Mixed outliers, variable 2"), col=col95, bty="n", cex.main=0.7, cex.axis=0.7)

# trend

plot_trend80=plot(fdata_out_trend80, main = list("80% Trend outliers, variable 1", "80% Trend outliers, variable 2"), col=col80, bty="n", cex.main=0.7, cex.axis=0.7)
plot_trend85=plot(fdata_out_trend85, main = list("85% Trend outliers, variable 1", "85% Trend outliers, variable 2"), col=col85, bty="n", cex.main=0.7, cex.axis=0.7)
plot_trend90=plot(fdata_out_trend90, main = list("90% Trend outliers, variable 1", "90% Trend outliers, variable 2"), col=col90, bty="n", cex.main=0.7, cex.axis=0.7)
plot_trend95=plot(fdata_out_trend95, main = list("95% Trend outliers, variable 1", "95% Trend outliers, variable 2"), col=col95, bty="n", cex.main=0.7, cex.axis=0.7)

# isolated

plot_iso80=plot(fdata_out_iso80, main = list("80% Isolated outliers, variable 1", "80% Isolated outliers, variable 2"), col=col80, bty="n", cex.main=0.7, cex.axis=0.7)
plot_iso85=plot(fdata_out_iso85, main = list("85% Isolated outliers, variable 1", "85% Isolated outliers, variable 2"), col=col85, bty="n", cex.main=0.7, cex.axis=0.7)
plot_iso90=plot(fdata_out_iso90, main = list("90% Isolated outliers, variable 1", "90% Isolated outliers, variable 2"), col=col90, bty="n", cex.main=0.7, cex.axis=0.7)
plot_iso95=plot(fdata_out_iso95, main = list("95% Isolated outliers, variable 1", "95% Isolated outliers, variable 2"), col=col95, bty="n", cex.main=0.7, cex.axis=0.7)


# ----

# IDENTIFICADORES DE ATÍPICOS

# IDENTIFICADOR BOXPLOT
boxplot_outliers<-function(mfdata, depth="MBD", inf_factor=1.5){
    if (depth=="MBD"){
      # Building the depths vector - Modified Band
    depths_vector<-roahd::multiMBD(mfdata)
  }
  else if (depth=="BD"){
    # Building the depths vector - Band depth
    depths_vector<-roahd::multiBD(mfdata)
  }
  else if (depth=="hmodal"){
    depths_vector<-fda.usc::depth.modep(list(one=fda.usc::fdata(mfdata$fDList[[1]]$values),two=fda.usc::fdata(mfdata$fDList[[2]]$values)))$dep
  }
  else if (depth=="FM"){
    depths_vector<-fda.usc::depth.FMp(list(one=fda.usc::fdata(mfdata$fDList[[1]]$values),two=fda.usc::fdata(mfdata$fDList[[2]]$values)))$dep
  }
  else if (depth=="RP"){
    depths_vector<-fda.usc::depth.RPp(list(one=fda.usc::fdata(mfdata$fDList[[1]]$values),two=fda.usc::fdata(mfdata$fDList[[2]]$values)))$dep
  }
    else stop ("Depth function not available")
  # bulding rank statistics
  index=sort.list(depths_vector, decreasing=T)
  m=ceiling(length(depths_vector)*0.5)
  # Building central regions
  center_1=mfdata$fDList[[1]][index[1:m],]
  center_2=mfdata$fDList[[2]][index[1:m],]
  
  sup_1=apply(center_1$values, 2, max)
  inf_1=apply(center_1$values, 2, min)
  
  sup_2=apply(center_2$values, 2, max)
  inf_2=apply(center_2$values, 2, min)
  
  # Building distance from the fences of central regions
  
  dist_1=1.5*(sup_1-inf_1)
  dist_2=1.5*(sup_2-inf_2)
  
  upper_1=sup_1+dist_1
  lower_1=inf_1-dist_1
  outly_1=c()
  # Identifying outliers_1
  out_1<-which(apply((mfdata$fDList[[1]]$values),1, function(x)(any(x>upper_1)|any(x<lower_1))))

  upper_2=sup_2+dist_2
  lower_2=inf_2-dist_2
  
  # Identifying outliers_2
  out_2<-which(apply((mfdata$fDList[[2]]$values),1, function(x)(any(x>upper_2)|any(x<lower_2))))
  remove=unique(c(out_1, out_2))
  a=remove
  return(a)
}
boxplot_outliers(fdata_out_mag80, depth="RP")
# IDENTIFICADOR OUTLIERGRAM
outliergram_outliers<-function(mfdata, inf_factor=1.5){
  depth<-roahd::multiMBD(mfdata)
  mei<-roahd::multiMEI(mfdata)
  n<-mfdata$N
  a_0<-(-2)/(n*(n-1))
  a_1<-2*(n+1)/(n-1)
  d<-c()
  for (i in 1:length(depth)){
    d[i]=a_0+a_1*mei[i]+a_0*(n^2)*mei[i]^2-depth[i]
  }
  crit_inf=c()
  crit_sup=c()
  mc=mrfDepth::medcouple(d)
  iqr_D<-IQR(d)
  if (mc>=0){
    crit_sup=quantile(d)[4]+inf_factor*iqr_D*(exp(3*mc))
  }
  else {
    crit_sup=quantile(d)[4]+inf_factor*iqr_D*(exp(4*mc))
  }
  out<-which(d>=crit_sup)
  return(out)
}
outliergram_outliers_<-function(mfdata, inf_factor=1.5){
  out=outliergram_outliers(mfdata)
  return(out)
}

# IDENTIFICADOR CENTRALITY-STABILITY
sk_mod_pd_outliers<-function (mfdata, alpha = 0.125, Beta = 0.5, Time = NULL, depth="SAPD") {
  # Function Parameters
  y1 <- data.matrix(mfdata$fDList[[1]]$values)
  y2 <- data.matrix(mfdata$fDList[[2]]$values)
  n <- nrow(y1)
  T <- ncol(y1)
  err.loc <- c()
  # Bivariate Y
  ybiv <- matrix(0, n, 2)
  # Bivariate Depths of each point in time
  depths.time <- matrix(NA, n, T)
  AO<-matrix(NA, n, T)
  # Weights for time points
  weights <- matrix(1, 1, T)
  disp <- matrix(0, 2, T)
  # Median
  MFHDmedian = matrix(0, 2, T)
  # Outlier location
  loc.outl = matrix(0, n, T)
  # Calculating depths
  for (j in 1:T) {
    ybiv[, 1] <- y1[, j]
    ybiv[, 2] <- y2[, j]
    if (depth =="tukey"){
      for (s in 1:n) 
        depths.time[s, j] <- depth(ybiv[s, ], 
                                   ybiv)
    }
    else if (depth=="SAPD"){
      temp<-mrfDepth::sprojdepth(ybiv)
      if (!is.null(temp$depthX)){
     depths.time[, j]<-temp$depthX
      }
    }
      temp2<-mrfDepth::adjOutl(ybiv)
      if (!is.null(temp2$outlyingnessX)){
    AO[, j]<-temp2$outlyingnessX}
  }
    weights <- weights/sum(weights)
    depths <- depths.time %*% t(weights)
    
    comp_2_=matrix(NA, n, T)
    
    w_1<-weights^(-1)
    
    for (i in 1:T){
      comp_2_[,i]<-(1+AO[,i])*w_1[i]
    }
    bound=c()
    for (i in 1:T){
      bound[i]<-T/i
    }
    comp_2__<-rowMeans(comp_2_)
    sec_=depths^(-1)
    sec=T*sec_
    comp_2<-comp_2__-sec
    dist<-as.vector(comp_2__-bound)
    iqrdist1<-quantile(dist, probs=0.75,na.rm = TRUE)
    iqrdist2<-1.5*IQR(dist, na.rm = TRUE)
    iqrdist<-iqrdist1+iqrdist2
    out<-which(dist>iqrdist)
  
  #return(  list( out=out, dist=dist, depth = depths, AO=AO, weights= weights, comp_2_=comp_2_, comp_2__=comp_2__, comp_2=comp_2))
  return(out)
}
sk_mod_pd_outliers(fdata_out_cov80)
# IDENTIFICADOR BAGPLOT
bp_crit_outliers<-function(mfdata){
  # Function Parameters
  y1 <- data.matrix(mfdata$fDList[[1]]$values)
  y2 <- data.matrix(mfdata$fDList[[2]]$values)
  n <- nrow(y1)
  T <- ncol(y1)
  # Bivariate Y
  ybiv <- matrix(0, n, 2)
  # Bivariate Depths of each point in time
  bagdistance.time <- matrix(NA, n, T)
  # Weights for time points
  # Median
  # Outlier location
  loc.outl = matrix(0, n, T)
  # Calculating depths
  out.time<- matrix(NA, n, T)
  for (j in 1:T) {
    ybiv[, 1] <- y1[, j]
    ybiv[, 2] <- y2[, j]
    out.time[,j]<-as.vector(mrfDepth::compBagplot(ybiv, sizesubset = 50)$flag)
  }
  a<-unique(unlist(apply(out.time, 2, function(x) which(x==0))))
  return(a)
}

# IDENTIFICADOR FUNTA y rFUNTA
library(FUNTA)
funta_crit_outliers<-function(mfdata, Nboot=50, sizeboot=30, treshold=0.01){
  y1 <- data.matrix(mfdata$fDList[[1]]$values)
  y2 <- data.matrix(mfdata$fDList[[2]]$values)
  funta_depth_1<-as.data.frame(FUNTA(y1))
  funta_depth_2<-as.data.frame(FUNTA(y2))
  funta_depth<-as.data.frame(rowMeans(cbind(funta_depth_1$`FUNTA(y1)`, funta_depth_2$`FUNTA(y2)`)))
  colnames(funta_depth)<-"depth"
  a<-list()
  c<-list()
  weight<-c()
  for (i in 1:nrow(funta_depth)){
    weight[i]<-(funta_depth$depth[i]-min(funta_depth$depth))/(max(funta_depth$depth)-min(funta_depth$depth))
  }
  for( i in 1:Nboot){
    a[[i]]<-sample(funta_depth$depth, sizeboot, replace=T, prob=weight)
    c[[i]]<-quantile(a[[i]], treshold)
  }
  c<-unlist(c)
  d<-median(unlist(c))
  f<-which(funta_depth$depth<d) 
  return(f)
}

rfunta_crit_outliers<-function(mfdata, Nboot=50, sizeboot=30, treshold=0.01){
  y1 <- data.matrix(mfdata$fDList[[1]]$values)
  y2 <- data.matrix(mfdata$fDList[[2]]$values)
  funta_depth_1<-as.data.frame(rFUNTA(y1))
  funta_depth_2<-as.data.frame(rFUNTA(y2))
  funta_depth<-as.data.frame(rowMeans(cbind(funta_depth_1$`rFUNTA(y1)`, funta_depth_2$`rFUNTA(y2)`)))
  colnames(funta_depth)<-"depth"
  a<-list()
  c<-list()
  weight<-c()
  for (i in 1:nrow(funta_depth)){
    weight[i]<-(funta_depth$depth[i]-min(funta_depth$depth))/(max(funta_depth$depth)-min(funta_depth$depth))
  }
  for( i in 1:Nboot){
    a[[i]]<-sample(funta_depth$depth, sizeboot, replace=T, prob=weight)
    c[[i]]<-quantile(a[[i]], treshold)
  }
  c<-unlist(c)
  d<-median(unlist(c))
  f<-which(funta_depth$depth<d) 
  return(f)
}

generic_crit_outliers<-function(mfdata, depth, Nboot=50, sizeboot=30, treshold=0.01){
  if (depth=="MBD"){
    # Building the depths vector - Modified Band
    depths_vector<-roahd::multiMBD(mfdata)
  }
  else if (depth=="BD"){
    # Building the depths vector - Band depth
    depths_vector<-roahd::multiBD(mfdata)
  }
  else if (depth=="hmodal"){
    depths_vector<-as.vector(fda.usc::depth.modep(list(one=fda.usc::fdata(mfdata$fDList[[1]]$values),two=fda.usc::fdata(mfdata$fDList[[2]]$values)))$dep)
  }
  else if (depth=="FM"){
    depths_vector<-as.vector(fda.usc::depth.FMp(list(one=fda.usc::fdata(mfdata$fDList[[1]]$values),two=fda.usc::fdata(mfdata$fDList[[2]]$values)))$dep)
  }
  else if (depth=="RP"){
    depths_vector<-as.vector(fda.usc::depth.RPp(list(one=fda.usc::fdata(mfdata$fDList[[1]]$values),two=fda.usc::fdata(mfdata$fDList[[2]]$values)))$dep)
  }
  a<-list()
  c<-list()
  weight<-c()
  for (i in 1:length(depths_vector)){
    weight[i]<-(depths_vector[i]-min(depths_vector))/(max(depths_vector)-min(depths_vector))
  }
  for( i in 1:Nboot){
    a[[i]]<-sample(depths_vector, sizeboot, replace=T, prob=weight)
    c[[i]]<-quantile(a[[i]], treshold)
  }
  c<-unlist(c)
  d<-median(unlist(c))
  f<-which(depths_vector<d) 
  return(f)
}


# IDENTIFICADOR DIRECTIONAL OUTLYINGNESS
library(fdaoutlier)
crit_dir<-function(mfdata){
  a<-list(mfdata$fDList[[1]]$values, mfdata$fDList[[2]]$values)
  c<-array(unlist(a), dim = c(nrow(a[[1]]), ncol(a[[1]]), length(a)))
d<-fdaoutlier::msplot(c)$outliers_index
return(d)
}


# IDENTIFICADOR SEQUENTIAL TRANSFORMATION

crit_seqo<-function(mfdata){
  a<-list(mfdata$fDList[[1]]$values, mfdata$fDList[[2]]$values)
  c<-array(unlist(a), dim = c(nrow(a[[1]]), ncol(a[[1]]), length(a)))
  d<-unique(unlist(fdaoutlier::seq_transform(c, sequence= c("O", "O", "O"), depth_method = "mbd")$outliers))
  return(d)
}

crit_seqt<-function(mfdata,df){
  b<-boxplot_outliers(mfdata, df)
  d1<-boxplot_outliers(mfData(time_grid, list(fdata.deriv(mfdata$fDList[[1]]$values)$data, fdata.deriv(mfdata$fDList[[2]]$values)$data)),df)
  d2<-boxplot_outliers(mfData(time_grid, list(fdata.deriv(mfdata$fDList[[1]]$values,nderiv = 2)$data, fdata.deriv(mfdata$fDList[[1]]$values, nderiv = 2)$data)),df)
  c<-c(b, d1, d2)
  return(c)
  }


# IDENTIFICADOR HDR
hdr_crit_outliers<-function(mfdata){
  P=ncol(mfdata$fDList[[1]]$values)
  time_grid=seq( 0, 1, length.out = P )
  multifundata<-multiFunData(funData(time_grid, mfdata[["fDList"]][[1]][["values"]]),funData(time_grid,mfdata[["fDList"]][[2]][["values"]]))
  a_5<-MFPCA(multifundata, 2, uniExpansions = list(list(type = "uFPCA"),
                                                   list(type = "uFPCA")))$scores
  c= which(mrfDepth::compBagplot(a_5, sizesubset = 50)$flag==0)
  return(c)
}



# FUNCION DE VALIDACION DE LOS IDENTIFICADORES

# LOS VALORES DE MFDATA_FUNCTION PUEDEN SER: 

# "boxplot-MBD",
# "boxplot-hmodal",
# "boxplot-FM",
# "boxplot-RP",
# "centrality.stability",
# "outliergram",
# "outliergram_m",
# "bagplot",
# "funta",
# "rfunta",
# "hdr",
# "directional",
# "seqo",
# "seqt-MBD",
# "seqt-hmodal",
# "seqt-FM",
# "seqt-RP",
# "alfa-MBD",
# "alfa-BD",
# "alfa-hmodal",
# "alfa-FM",
# "alfa-RP"

tp_validation<-function(mfdata, mfdata_function){
  start=Sys.time()
  if (mfdata_function=="boxplot-MBD"){
  list<-list()
  for(i in 1: length(mfdata)){
    list[[i]]<-boxplot_outliers(mfdata[[i]], depth="MBD")
  }
  # TASA DE VERDADEROS POSITIVOS
  a<-matrix(NA, 100, 200)
    for (i in 1:length(list)){
    for (j in 1:200){
      a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
    }
  } 
  }
  else if (mfdata_function=="boxplot-hmodal"){
      list<-list()
      for(i in 1: length(mfdata)){
        list[[i]]<-boxplot_outliers(mfdata[[i]], depth="hmodal")
      }
      a<-matrix(NA, 100, 200)
      # TASA DE VERDADEROS POSITIVOS
      for (i in 1:length(list)){
        for (j in 1:200){
          a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
        }
      } 
  }
  else if (mfdata_function=="boxplot-FM"){
      list<-list()
      for(i in 1: length(mfdata)){
        list[[i]]<-boxplot_outliers(mfdata[[i]], depth="FM")
      }
      a<-matrix(NA, 100, 200)
      # TASA DE VERDADEROS POSITIVOS
      for (i in 1:length(list)){
        for (j in 1:200){
          a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
        }
      } 
  }
  else if (mfdata_function=="boxplot-RP"){
      list<-list()
      for(i in 1: length(mfdata)){
        list[[i]]<-boxplot_outliers(mfdata[[i]], depth="RP")
      }
      a<-matrix(NA, 100, 200)
      # TASA DE VERDADEROS POSITIVOS
      for (i in 1:length(list)){
        for (j in 1:200){
          a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
        }
      } 
  }
  else if (mfdata_function=="centrality.stability"){
    list<-list()
    for(i in 1: length(mfdata)){
      list[[i]]<-sk_mod_pd_outliers(mfdata[[i]])
    }
    a<-matrix(NA, 100, 200)
    # TASA DE VERDADEROS POSITIVOS
    for (i in 1:length(list)){
      for (j in 1:200){
        a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
      }
    } 
  }
  else if (mfdata_function=="outliergram"){
    list<-list()
    for(i in 1: length(mfdata)){
      list[[i]]<-outliergram_outliers(mfdata[[i]])
    }
    a<-matrix(NA, 100, 200)
    # TASA DE VERDADEROS POSITIVOS
    for (i in 1:length(list)){
      for (j in 1:200){
        a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
      }
    } 
  }
  else if (mfdata_function=="outliergram_m"){
      list<-list()
      for(i in 1: length(mfdata)){
        list[[i]]<-roahd::multivariate_outliergram(mfdata[[i]], display = F)$ID_outliers
      }
      a<-matrix(NA, 100, 200)
      # TASA DE VERDADEROS POSITIVOS
      for (i in 1:length(list)){
        for (j in 1:200){
          a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
        }
      } 
  }
  else if (mfdata_function=="bagplot"){
    list<-list()
    for(i in 1: length(mfdata)){
      list[[i]]<-bp_crit_outliers(mfdata[[i]])
    }
    a<-matrix(NA, 100, 200)
    # TASA DE VERDADEROS POSITIVOS
    for (i in 1:length(list)){
      for (j in 1:200){
        a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
      }
    } 
  }
  else if (mfdata_function=="funta"){
    list<-list()
    for(i in 1: length(mfdata)){
      list[[i]]<-funta_crit_outliers(mfdata[[i]])
    }
    a<-matrix(NA, 100, 200)
    # TASA DE VERDADEROS POSITIVOS
    for (i in 1:length(list)){
      for (j in 1:200){
        a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
      }
    } 
  }
  else if (mfdata_function=="rfunta"){
    list<-list()
    for(i in 1: length(mfdata)){
      list[[i]]<-rfunta_crit_outliers(mfdata[[i]])
    }
      a<-matrix(NA, 100, 200)
        # TASA DE VERDADEROS POSITIVOS
        for (i in 1:length(list)){
          for (j in 1:200){
            a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
          }
        } 
      }
  else if (mfdata_function=="hdr"){
    list<-list()
    for(i in 1: length(mfdata)){
      list[[i]]<-hdr_crit_outliers(mfdata[[i]])
    }
    a<-matrix(NA, 100, 200)
    # TASA DE VERDADEROS POSITIVOS
    for (i in 1:length(list)){
      for (j in 1:200){
        a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
      }
    } 
  }
  else if (mfdata_function=="directional"){
      list<-list()
      for(i in 1: length(mfdata)){
        list[[i]]<-crit_dir(mfdata[[i]])
      }
      a<-matrix(NA, 100, 200)
      # TASA DE VERDADEROS POSITIVOS
      for (i in 1:length(list)){
        for (j in 1:200){
          a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
        }
      } 
  }
  else if (mfdata_function=="seqo"){
      list<-list()
      for(i in 1: length(mfdata)){
        list[[i]]<-crit_seqo(mfdata[[i]])
      }
      a<-matrix(NA, 100, 200)
      # TASA DE VERDADEROS POSITIVOS
      for (i in 1:length(list)){
        for (j in 1:200){
          a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
        }
      } 
  }
  else if (mfdata_function=="seqt-MBD"){
    list<-list()
    for(i in 1: length(mfdata)){
      list[[i]]<-crit_seqt(mfdata[[i]], "MBD")
    }
    a<-matrix(NA, 100, 200)
    # TASA DE VERDADEROS POSITIVOS
    for (i in 1:length(list)){
      for (j in 1:200){
        a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
      }
    } 
  }
  else if (mfdata_function=="seqt-hmodal"){
    list<-list()
    for(i in 1: length(mfdata)){
      list[[i]]<-crit_seqt(mfdata[[i]], "hmodal")
    }
    a<-matrix(NA, 100, 200)
    # TASA DE VERDADEROS POSITIVOS
    for (i in 1:length(list)){
      for (j in 1:200){
        a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
      }
    } 
  }
  else if (mfdata_function=="seqt-FM"){
    list<-list()
    for(i in 1: length(mfdata)){
      list[[i]]<-crit_seqt(mfdata[[i]], "FM")
    }
    a<-matrix(NA, 100, 200)
    # TASA DE VERDADEROS POSITIVOS
    for (i in 1:length(list)){
      for (j in 1:200){
        a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
      }
    } 
  }
  else if (mfdata_function=="seqt-RP"){
    list<-list()
    for(i in 1: length(mfdata)){
      list[[i]]<-crit_seqt(mfdata[[i]], "RP")
    }
    a<-matrix(NA, 100, 200)
    # TASA DE VERDADEROS POSITIVOS
    for (i in 1:length(list)){
      for (j in 1:200){
        a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
      }
    } 
  }
  else if (mfdata_function=="alfa-MBD"){
      list<-list()
      for(i in 1: length(mfdata)){
        list[[i]]<-generic_crit_outliers(mfdata[[i]], "MBD")
      }
      a<-matrix(NA, 100, 200)
      # TASA DE VERDADEROS POSITIVOS
      for (i in 1:length(list)){
        for (j in 1:200){
          a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
        }
      } 
  }
  else if (mfdata_function=="alfa-BD"){
      list<-list()
      for(i in 1: length(mfdata)){
        list[[i]]<-generic_crit_outliers(mfdata[[i]], "BD")
      }
      a<-matrix(NA, 100, 200)
      # TASA DE VERDADEROS POSITIVOS
      for (i in 1:length(list)){
        for (j in 1:200){
          a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
        }
      } 
  }
  else if (mfdata_function=="alfa-hmodal"){
      list<-list()
      for(i in 1: length(mfdata)){
        list[[i]]<-generic_crit_outliers(mfdata[[i]], "hmodal")
      }
      a<-matrix(NA, 100, 200)
      # TASA DE VERDADEROS POSITIVOS
      for (i in 1:length(list)){
        for (j in 1:200){
          a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
        }
      } 
  }
  else if (mfdata_function=="alfa-FM"){
      list<-list()
      for(i in 1: length(mfdata)){
        list[[i]]<-generic_crit_outliers(mfdata[[i]], "FM")
      }
      a<-matrix(NA, 100, 200)
      # TASA DE VERDADEROS POSITIVOS
      for (i in 1:length(list)){
        for (j in 1:200){
          a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
        }
      } 
  }
  else if (mfdata_function=="alfa-RP"){
      list<-list()
      for(i in 1: length(mfdata)){
        list[[i]]<-generic_crit_outliers(mfdata[[i]], "FM")
      }
      a<-matrix(NA, 100, 200)
      # TASA DE VERDADEROS POSITIVOS
      for (i in 1:length(list)){
        for (j in 1:200){
          a[i,j]<-as.numeric( c(1:200)[j] %in% list[[i]])
        }
      } 
  }
  end<-Sys.time()
  time<-difftime(end, start, units = "secs")
  print(paste("process lasted", paste((time), "seconds")))
  return(list("mfdata_function" = mfdata_function, "outlier_table (curves=columns, simulation=rows)"= a, "time"=time, "start"=start, "end"=end))
}


tp_validation(fdata_out_mag80_validation, "seqt-FM")

list_mfdata_function<-c("boxplot-MBD",
                        "boxplot-hmodal",
                        "boxplot-FM",
                        "boxplot-RP",
                        "centrality.stability",
                        "outliergram",
                        "outliergram_m",
                        "bagplot",
                        #"funta",
                        #"rfunta",
                        "hdr",
                        "directional",
                        "seqo",
                        "seqt-MBD",
                        "seqt-hmodal",
                        "seqt-FM",
                        "seqt-RP",
                        "alfa-MBD",
                        "alfa-BD",
                        "alfa-hmodal",
                        "alfa-FM",
                        "alfa-RP")
list_mfdata80<-list(fdata_out_mag80_validation, fdata_out_shape80_validation, fdata_out_cov80_validation, fdata_out_mix80_validation, fdata_out_trend80_validation, fdata_out_iso80_validation)
list_mfdata90<-list(fdata_out_mag90_validation, fdata_out_shape90_validation, fdata_out_cov90_validation, fdata_out_mix90_validation, fdata_out_trend90_validation, fdata_out_iso90_validation)
list_mfdata95<-list(fdata_out_mag95_validation, fdata_out_shape95_validation, fdata_out_cov95_validation, fdata_out_mix95_validation, fdata_out_trend95_validation, fdata_out_iso95_validation)




BD80<-list()
BD_<-list()
for (i in 1:length(list_mfdata_function)){
  for (j in 1:length(list_mfdata80)){
    BD_[[j]]<-tp_validation(list_mfdata80[[j]], list_mfdata_function[i])
  }
  BD80[[i]]<-BD_
}


BD95<-list()
for (i in 1:length(list_mfdata_function)){
  for (j in 1:length(list_mfdata95)){
    BD_[[j]]<-tp_validation(list_mfdata95[[j]], list_mfdata_function[i])
  }
  BD95[[i]]<-BD_
}

A1<-matrix(NA, 20,7)
A2<-matrix(NA, 20,7)
A3<-matrix(NA, 20,7)
A4<-matrix(NA, 20,7)

B1<-matrix(NA, 20,7)
B2<-matrix(NA, 20,7)
B3<-matrix(NA, 20,7)
B4<-matrix(NA, 20,7)

C1<-matrix(NA, 20,7)
C2<-matrix(NA, 20,7)
C3<-matrix(NA, 20,7)
C4<-matrix(NA, 20,7)

D1<-matrix(NA, 20,7)
D2<-matrix(NA, 20,7)
D3<-matrix(NA, 20,7)
D4<-matrix(NA, 20,7)


z=BD95

for (i in 1:length(BD80)){
  for(j in 1:length(BD80[[i]])){
    A1[i,j]<-mean(rowMeans(BD80[[i]][[j]]$`outlier_table (curves=columns, simulation=rows)`[,161:200]))
      }
}

for (i in 1:length(BD80)){
  for(j in 1:length(BD80[[i]])){
    C1[i,j]<-mean(rowMeans(BD80[[i]][[j]]$`outlier_table (curves=columns, simulation=rows)`[,1:160]))
  }
}

for (i in 1:length(BD95)){
  for(j in 1:length(BD95[[i]])){
    A2[i,j]<-mean(rowMeans(BD95[[i]][[j]]$`outlier_table (curves=columns, simulation=rows)`[,191:200]))
  }
}

for (i in 1:length(BD95)){
  for(j in 1:length(BD95[[i]])){
    z[[i]][[j]]<-rowMeans(BD95[[i]][[j]]$`outlier_table (curves=columns, simulation=rows)`[,1:190])
    C2[i,j]<-mean(z[[i]][[j]])  
  }
}


A1<-as.data.frame(A1)
A2<-as.data.frame(A2)


C1<-as.data.frame(C1)
C2<-as.data.frame(C2)

COLNAMES<-c("Magnitude", "Shape", "Covariance", "Mixed", "Trend", "Isolated")
ROWNAMES<-c("boxplot-MBD",
            "boxplot-hmodal",
            "boxplot-FM",
            "boxplot-RP",
            "centrality.stability",
            "outliergram",
            "outliergram_m",
            "bagplot",
            #"funta",
            #"rfunta",
            "hdr",
            "directional",
            "seq-O",
            "seq-T-MBD",
            "seq-T-hmodal",
            "seq-T-FM",
            "seq-T-RP",
            "alpha-MBD",
            "alpha-BD",
            "alpha-hmodal",
            "alpha-FM",
            "alpha-RP")

colnames(A1)<-COLNAMES
rownames(A1)<-ROWNAMES
colnames(A2)<-COLNAMES
rownames(A2)<-ROWNAMES

colnames(C1)<-COLNAMES
rownames(C1)<-ROWNAMES
colnames(C2)<-COLNAMES
rownames(C2)<-ROWNAMES

print(xtable::xtable(A1, type = "latex"), file = "truepositive_80.tex")
print(xtable::xtable(C1, type = "latex"), file = "falsepositive_80t.tex")

print(xtable::xtable(A2, type = "latex"), file = "truepositive_95.tex")
print(xtable::xtable(C2, type = "latex"), file = "falsepositive_95.tex")

print(xtable::xtable(D1, type = "latex"), file = "Indicator_80.tex")
print(xtable::xtable(D2, type = "latex"), file = "Indicator_95.tex")
print(xtable::xtable(D3, type = "latex"), file = "Indicator_total.tex")


D1<-(1+A1)/(1+C1)
D2<-(1+A2)/(1+C2)
D3<-rbind(D1, D2)
write.csv(A1, "A1.csv")
write.csv(A2, "A2.csv")

write.csv(C1, "C1.csv")
write.csv(C2, "C2.csv")




list_mfdata80t<-list(fdata_t_out_mag80_validation, fdata_t_out_shape80_validation, fdata_t_out_cov80_validation, fdata_t_out_mix80_validation, fdata_t_out_trend80_validation, fdata_t_out_iso80_validation)
list_mfdata90t<-list(fdata_t_out_mag90_validation, fdata_t_out_shape90_validation, fdata_t_out_cov90_validation, fdata_t_out_mix90_validation, fdata_t_out_trend90_validation, fdata_t_out_iso90_validation)
list_mfdata95t<-list(fdata_t_out_mag95_validation, fdata_t_out_shape95_validation, fdata_t_out_cov95_validation, fdata_t_out_mix95_validation, fdata_t_out_trend95_validation, fdata_t_out_iso95_validation)

BD80t<-list()
BD_<-list()
for (i in 1:length(list_mfdata_function)){
  for (j in 1:length(list_mfdata80t)){
    BD_[[j]]<-tp_validation(list_mfdata80t[[j]], list_mfdata_function[i])
  }
  BD80t[[i]]<-BD_
}


BD95t<-list()
for (i in 1:length(list_mfdata_function)){
  for (j in 1:length(list_mfdata95t)){
    BD_[[j]]<-tp_validation(list_mfdata95t[[j]], list_mfdata_function[i])
  }
  BD95t[[i]]<-BD_
}

A1t<-matrix(NA, 20,7)
A2t<-matrix(NA, 20,7)
A3t<-matrix(NA, 20,7)
A4t<-matrix(NA, 20,7)

B1t<-matrix(NA, 20,7)
B2t<-matrix(NA, 20,7)
B3t<-matrix(NA, 20,7)
B4t<-matrix(NA, 20,7)

C1t<-matrix(NA, 20,7)
C2t<-matrix(NA, 20,7)
C3t<-matrix(NA, 20,7)
C4t<-matrix(NA, 20,7)

D1t<-matrix(NA, 20,7)
D2t<-matrix(NA, 20,7)
D3t<-matrix(NA, 20,7)
D4t<-matrix(NA, 20,7)


z=BD95
z<-list()
for (i in 1:length(BD80t)){
  for(j in 1:length(BD80t[[i]])){
    A1t[i,j]<-mean(rowMeans(BD80t[[i]][[j]]$`outlier_table (curves=columns, simulation=rows)`[,161:200]))
  }
}

for (i in 1:length(BD80t)){
  for(j in 1:length(BD80t[[i]])){
    C1t[i,j]<-mean(rowMeans(BD80t[[i]][[j]]$`outlier_table (curves=columns, simulation=rows)`[,1:160]))
  }
}

for (i in 1:length(BD95t)){
  for(j in 1:length(BD95t[[i]])){
    z[[i]][[j]]<-rowMeans(BD95t[[i]][[j]]$`outlier_table (curves=columns, simulation=rows)`[,181:200])
    A2t[i,j]<-mean(z[[i]][[j]])  
  }
}

for (i in 1:length(BD95t)){
  for(j in 1:length(BD80t[[i]])){
    z[[i]][[j]]<-rowMeans(BD95t[[i]][[j]]$`outlier_table (curves=columns, simulation=rows)`[,1:180])
    C2t[i,j]<-mean(z[[i]][[j]])  
  }
}


A1t<-as.data.frame(A1t)
A2t<-as.data.frame(A2t)


C1t<-as.data.frame(C1t)
C2t<-as.data.frame(C2t)

COLNAMES<-c("Magnitude", "Shape", "Covariance", "Mixed", "Trend", "Isolated")
ROWNAMES<-c("boxplot-MBD",
            "boxplot-hmodal",
            "boxplot-FM",
            "boxplot-RP",
            "centrality.stability",
            "outliergram",
            "outliergram_m",
            "bagplot",
            #"funta",
            #"rfunta",
            "hdr",
            "directional",
            "seq-O",
            "seq-T-MBD",
            "seq-T-hmodal",
            "seq-T-FM",
            "seq-T-RP",
            "alpha-MBD",
            "alpha-BD",
            "alpha-hmodal",
            "alpha-FM",
            "alpha-RP")

colnames(A1t)<-COLNAMES
rownames(A1t)<-ROWNAMES
colnames(A2t)<-COLNAMES
rownames(A2t)<-ROWNAMES

colnames(C1t)<-COLNAMES
rownames(C1t)<-ROWNAMES
colnames(C2t)<-COLNAMES
rownames(C2t)<-ROWNAMES

print(xtable::xtable(A1t, type = "latex"), file = "truepositive_80.tex")
print(xtable::xtable(C1t, type = "latex"), file = "falsepositive_80.tex")

print(xtable::xtable(A2t, type = "latex"), file = "truepositive_95.tex")
print(xtable::xtable(C2t, type = "latex"), file = "falsepositive_95.tex")

print(xtable::xtable(D1t, type = "latex"), file = "Indicator_80.tex")
print(xtable::xtable(D2t, type = "latex"), file = "Indicator_95.tex")
print(xtable::xtable(D3t, type = "latex"), file = "Indicator_total.tex")


D1t<-(1+A1t)/(1+C1t)
D2t<-(1+A2t)/(1+C2t)
D3t<-rbind(D1, D2)
write.csv(A1t, "A1t.csv")
write.csv(A2t, "A2t.csv")

write.csv(C1t, "C1t.csv")
write.csv(C2t, "C2t.csv")

