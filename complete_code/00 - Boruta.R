######################### 00 - BORUTA #########################
library(Boruta)

bind_rose$TARGET <- as.factor(bind_rose$TARGET)
borutaMod <- Boruta(TARGET ~ ., data=bind_rose, doTrace=1)

bind_up$TARGET <- as.factor(bind_up$TARGET)
borutaMod_up <- Boruta(TARGET ~ ., data=bind_up, doTrace=1)

bind_smote$TARGET <- as.factor(bind_smote$TARGET)
borutaMod_smote <- Boruta(TARGET ~ ., data=bind_smote, doTrace=1)

bind_rose$TARGET <- as.factor(bind_rose$TARGET)
borutaMod_rose <- Boruta(TARGET ~ ., data=bind_rose, doTrace=1)


roughFixMod <- TentativeRoughFix(borutaMod)
boruta_signif <- getSelectedAttributes(roughFixMod, withTentative=TRUE)
boruta_signif
imps <- attStats(roughFixMod)
imps

pdf("outputboruta.pdf",width=8,height=5,paper='special') 
plot(roughFixMod, cex.axis=.7, las=2, xlab="", ylab="",
     main="", xaxt="n",outline=FALSE)
dev.off()



bind_down$TARGET <- TARGET_down
output_bind_down_logit <- recursive_logit(data.frame(bind_down), TARGET, bind_validation, TARGET_validation, 70)

bind_up$TARGET <- TARGET_up
output_bind_up_logit <- recursive_logit(data.frame(bind_up), TARGET, bind_validation, TARGET_validation, 70)

bind_smote$TARGET <- TARGET_smote
output_bind_smote_logit <- recursive_logit(data.frame(bind_smote), TARGET, bind_validation, TARGET_validation, 70)

bind_rose$TARGET <- TARGET_rose
output_bind_rose_logit <- recursive_logit(data.frame(bind_rose), TARGET, bind_validation, TARGET_validation, 70)
