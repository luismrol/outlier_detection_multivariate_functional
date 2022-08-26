# CLASSIFICATION IN FUNCTIONAL DATA
# LUIS M. ROLDÁN Y FRANCISCO ZULUAGA
# DEPURACIÓN DE DATOS
# se toman los datos de Construccion_bdatos_aire.R
library(fda.usc)
critical7<-as.numeric((pm_80$critical7+pm_28$critical7+pm_12$critical7+pm_48$critical7)>0)
critical7<-critical7[1:663]
names(critical7)<-"critical7"
pm_12<-dplyr::select(pm_12[1:663,], -c(Fecha, critical7, critical, mean, mean7))
pm_28<-dplyr::select(pm_28[1:663,], -c(Fecha, critical7, critical, mean, mean7))
pm_48<-dplyr::select(pm_48[1:663,], -c(Fecha, critical7, critical, mean, mean7))
pm_80<-dplyr::select(pm_80[1:663,], -c(Fecha, critical7, critical, mean, mean7))
# 
# pm_12<-fdata(as.matrix(pm_12))
# pm_28<-fdata(as.matrix(pm_28))
# pm_48<-fdata(as.matrix(pm_48))
# pm_80<-fdata(as.matrix(pm_80))
train<-sample(663, ceiling(0.7*663))
test<-setdiff(c(1:663),train)

formula1  <-formula(critical7~pm_12+pm_28+pm_48+pm_80)
data1=list("pm_12"=as.matrix(pm_12[train,1:24]),"pm_28"=as.matrix(pm_28[train,1:24]), "pm_48"=as.matrix(pm_48[train,1:24]), "pm_80"=as.matrix(pm_80[train,1:24]))

critical<-critical7
critical7<-critical7[train]
glm<-classif.glm(formula1, data1)
dd_np<-classif.DD(critical7, data1, classif="np")
test_result_np<-predict(dd_np, list("pm_12"=as.matrix(pm_12[test,1:24]),"pm_28"=as.matrix(pm_28[test,1:24]), "pm_48"=as.matrix(pm_48[test,1:24]), "pm_80"=as.matrix(pm_80[test,1:24])))
train_result_np<-dd_np$group.est

train_robust<-which(pm_12[train,]$out=="NO OUT")
data2=list("pm_12"=as.matrix(pm_12[train_robust,1:24]),"pm_28"=as.matrix(pm_28[train_robust,1:24]), "pm_48"=as.matrix(pm_48[train_robust,1:24]), "pm_80"=as.matrix(pm_80[train_robust,1:24]))
critical7_robust<-critical[train_robust]
dd_robust_np<-classif.DD(critical7_robust, data2, classif="np")
train_result_robust_np<-dd_robust_np$group.est
test_result_robust_np<-predict(dd_robust_np, list("pm_12"=as.matrix(pm_12[test,1:24]),"pm_28"=as.matrix(pm_28[test,1:24]), "pm_48"=as.matrix(pm_48[test,1:24]), "pm_80"=as.matrix(pm_80[test,1:24])))

general_train_np<-as.data.frame(critical[train])
general_test_np<-as.data.frame(critical[test])
general_train_robust_np<-as.data.frame(critical[train_robust])
general_test_robust_np<-as.data.frame(critical[test])

general_train_np$value<-train_result_np
general_test_np$value<-test_result_np
general_train_robust_np$value<-train_result_robust_np
general_test_robust_np$value<-test_result_robust_np

cm_base_train_np<-table(general_train_np)
accuracy_base_train_np<-trace.matrix(cm_base_train_np)/sum(cm_base_train_np)
cm_base_test_np<-table(general_test_np)
accuracy_base_test_np<-trace.matrix(cm_base_test_np)/sum(cm_base_test_np)
cm_robust_train_np<-table(general_train_robust_np)
accuracy_robust_train_np<-trace.matrix(cm_robust_train_np)/sum(cm_robust_train_np)
cm_robust_test_np<-table(general_test_robust_np)
accuracy_robust_test_np<-trace.matrix(cm_robust_test_np)/sum(cm_robust_test_np)

dd_knn<-classif.DD(critical7, data1, classif="knn")
test_result_knn<-predict(dd_knn, list("pm_12"=as.matrix(pm_12[test,1:24]),"pm_28"=as.matrix(pm_28[test,1:24]), "pm_48"=as.matrix(pm_48[test,1:24]), "pm_80"=as.matrix(pm_80[test,1:24])))
train_result_knn<-dd_knn$group.est

train_robust<-which(pm_12[train,]$out=="NO OUT")
data2=list("pm_12"=as.matrix(pm_12[train_robust,1:24]),"pm_28"=as.matrix(pm_28[train_robust,1:24]), "pm_48"=as.matrix(pm_48[train_robust,1:24]), "pm_80"=as.matrix(pm_80[train_robust,1:24]))
critical7_robust<-critical[train_robust]
dd_robust_knn<-classif.DD(critical7_robust, data2, classif="knn")
train_result_robust_knn<-dd_robust_knn$group.est
test_result_robust_knn<-predict(dd_robust_knn, list("pm_12"=as.matrix(pm_12[test,1:24]),"pm_28"=as.matrix(pm_28[test,1:24]), "pm_48"=as.matrix(pm_48[test,1:24]), "pm_80"=as.matrix(pm_80[test,1:24])))

general_train_knn<-as.data.frame(critical[train])
general_test_knn<-as.data.frame(critical[test])
general_train_robust_knn<-as.data.frame(critical[train_robust])
general_test_robust_knn<-as.data.frame(critical[test])

general_train_knn$value<-train_result_knn
general_test_knn$value<-test_result_knn
general_train_robust_knn$value<-train_result_robust_knn
general_test_robust_knn$value<-test_result_robust_knn

cm_base_train_knn<-table(general_train_knn)
accuracy_base_train_knn<-trace.matrix(cm_base_train_knn)/sum(cm_base_train_knn)
cm_base_test_knn<-table(general_test_knn)
accuracy_base_test_knn<-trace.matrix(cm_base_test_knn)/sum(cm_base_test_knn)
cm_robust_train_knn<-table(general_train_robust_knn)
accuracy_robust_train_knn<-trace.matrix(cm_robust_train_knn)/sum(cm_robust_train_knn)
cm_robust_test_knn<-table(general_test_robust_knn)
accuracy_robust_test_knn<-trace.matrix(cm_robust_test_knn)/sum(cm_robust_test_knn)


dd_glm<-classif.DD(critical7, data1, classif="glm")
test_result_glm<-predict(dd_glm, list("pm_12"=as.matrix(pm_12[test,1:24]),"pm_28"=as.matrix(pm_28[test,1:24]), "pm_48"=as.matrix(pm_48[test,1:24]), "pm_80"=as.matrix(pm_80[test,1:24])))
train_result_glm<-dd_glm$group.est

train_robust<-which(pm_12[train,]$out=="NO OUT")
data2=list("pm_12"=as.matrix(pm_12[train_robust,1:24]),"pm_28"=as.matrix(pm_28[train_robust,1:24]), "pm_48"=as.matrix(pm_48[train_robust,1:24]), "pm_80"=as.matrix(pm_80[train_robust,1:24]))
critical7_robust<-critical[train_robust]
dd_robust_glm<-classif.DD(critical7_robust, data2, classif="glm")
train_result_robust_glm<-dd_robust_glm$group.est
test_result_robust_glm<-predict(dd_robust_glm, list("pm_12"=as.matrix(pm_12[test,1:24]),"pm_28"=as.matrix(pm_28[test,1:24]), "pm_48"=as.matrix(pm_48[test,1:24]), "pm_80"=as.matrix(pm_80[test,1:24])))

general_train_glm<-as.data.frame(critical7)
general_test_glm<-as.data.frame(critical[test])
general_train_robust_glm<-as.data.frame(critical7_robust)
general_test_robust_glm<-as.data.frame(critical[test])

general_train_glm$value<-train_result_glm
general_test_glm$value<-test_result_glm
general_train_robust_glm$value<-train_result_robust_glm
general_test_robust_glm$value<-test_result_robust_glm

cm_base_train_glm<-table(general_train_glm)
accuracy_base_train_glm<-trace.matrix(cm_base_train_glm)/sum(cm_base_train_glm)
cm_base_test_glm<-table(general_test_glm)
accuracy_base_test_glm<-trace.matrix(cm_base_test_glm)/sum(cm_base_test_glm)
cm_robust_train_glm<-table(general_train_robust_glm)
accuracy_robust_train_glm<-trace.matrix(cm_robust_train_glm)/sum(cm_robust_train_glm)
cm_robust_test_glm<-table(general_test_robust_glm)
accuracy_robust_test_glm<-trace.matrix(cm_robust_test_glm)/sum(cm_robust_test_glm)


matrix_accuracy=data.frame(Models=c("knn", "glm", "np"), "Train complete"=c(accuracy_base_train_knn, accuracy_base_train_glm, accuracy_base_train_np), "Train robust"=c(accuracy_robust_train_knn, accuracy_robust_train_glm, accuracy_robust_train_np), "Test complete"=c(accuracy_base_test_knn, accuracy_base_test_glm, accuracy_base_test_np),"Test robust"=c(accuracy_robust_test_knn, accuracy_robust_test_glm, accuracy_robust_test_np))



print(xtable(matrix_accuracy, type = "latex"), file="tabla_accuracy.tex")

