mydata1 <-read.table(file="07-11.csv",header=T,sep=",",na.strings =" ")
mydata2 <-read.table(file="09-11.csv",header=T,sep=",",na.strings =" ")
mydata3 <-read.table(file="11-11.csv",header=T,sep=",",na.strings =" ")
mydata4 <-read.table(file="13-11.csv",header=T,sep=",",na.strings =" ")
mydata5 <-read.table(file="14-11.csv",header=T,sep=",",na.strings =" ")
mydata6 <-read.table(file="15-11.csv",header=T,sep=",",na.strings =" ")
mydata7 <-read.table(file="16-11.csv",header=T,sep=",",na.strings =" ")
mydata8 <-read.table(file="17-11.csv",header=T,sep=",",na.strings =" ")
mydata9 <-read.table(file="18-11.csv",header=T,sep=",",na.strings =" ")
mydata10<-read.table(file="19-11.csv",header=T,sep=",",na.strings =" ")
mydata11<-read.table(file="20-11.csv",header=T,sep=",",na.strings =" ")
mydata12<-read.table(file="21-11.csv",header=T,sep=",",na.strings =" ")
mydata13<-read.table(file="22-11.csv",header=T,sep=",",na.strings =" ")
mydata14<-read.table(file="23-11.csv",header=T,sep=",",na.strings =" ")
mydata15<-read.table(file="24-11.csv",header=T,sep=",",na.strings =" ")
mydata16<-read.table(file="25-11.csv",header=T,sep=",",na.strings =" ")
mydata17<-read.table(file="26-11.csv",header=T,sep=",",na.strings =" ")
mydata18<-read.table(file="27-11.csv",header=T,sep=",",na.strings =" ")
mydata19<-read.table(file="28-11.csv",header=T,sep=",",na.strings =" ")
mydata20<-read.table(file="29-11.csv",header=T,sep=",",na.strings =" ")
mydata21<-read.table(file="30-11.csv",header=T,sep=",",na.strings =" ")
mydata22<-read.table(file="01-12.csv",header=T,sep=",",na.strings =" ")
mydata23<-read.table(file="02-12.csv",header=T,sep=",",na.strings =" ")
mydata24<-read.table(file="03-12.csv",header=T,sep=",",na.strings =" ")
mydata25<-read.table(file="04-12.csv",header=T,sep=",",na.strings =" ")
mydata26<-read.table(file="05-12.csv",header=T,sep=",",na.strings =" ")
mydata27<-read.table(file="06-12.csv",header=T,sep=",",na.strings =" ")
mydata28<-read.table(file="07-12.csv",header=T,sep=",",na.strings =" ")
mydata29<-read.table(file="08-12.csv",header=T,sep=",",na.strings =" ")
mydata30<-read.table(file="09-12.csv",header=T,sep=",",na.strings =" ")
mydata31<-read.table(file="10-12.csv",header=T,sep=",",na.strings =" ")
mydata32<-read.table(file="11-12.csv",header=T,sep=",",na.strings =" ")
mydata33<-read.table(file="12-12.csv",header=T,sep=",",na.strings =" ")
mydata34<-read.table(file="13-12.csv",header=T,sep=",",na.strings =" ")
mydata35<-read.table(file="14-12.csv",header=T,sep=",",na.strings =" ")
mydata36<-read.table(file="15-12.csv",header=T,sep=",",na.strings =" ")
mydata37<-read.table(file="16-12.csv",header=T,sep=",",na.strings =" ")
mydata38<-read.table(file="17-12.csv",header=T,sep=",",na.strings =" ")
mydata39<-read.table(file="18-12.csv",header=T,sep=",",na.strings =" ")
mydata40<-read.table(file="19-12.csv",header=T,sep=",",na.strings =" ")
mydata41<-read.table(file="20-12.csv",header=T,sep=",",na.strings =" ")
mydata42<-read.table(file="21-12.csv",header=T,sep=",",na.strings =" ")
mydata43<-read.table(file="22-12.csv",header=T,sep=",",na.strings =" ")
mydata44<-read.table(file="23-12.csv",header=T,sep=",",na.strings =" ")
mydata45<-read.table(file="24-12.csv",header=T,sep=",",na.strings =" ")



library(dplyr)
pm01<-filter(mydata1,sensordatavalues__value_type=="P1") 
pm02<-filter(mydata2,sensordatavalues__value_type=="P1")
pm03<-filter(mydata3,sensordatavalues__value_type=="P1")
pm04<-filter(mydata4,sensordatavalues__value_type=="P1")
pm05<-filter(mydata5,sensordatavalues__value_type=="P1")
pm06<-filter(mydata6,sensordatavalues__value_type=="P1")
pm07<-filter(mydata7,sensordatavalues__value_type=="P1")
pm08<-filter(mydata8,sensordatavalues__value_type=="P1")
pm09<-filter(mydata9,sensordatavalues__value_type=="P1")
pm10<-filter(mydata10,sensordatavalues__value_type=="P1")
pm11<-filter(mydata11,�..sensordatavalues__value_type=="P1")
pm12<-filter(mydata12,sensordatavalues__value_type=="P1")
pm13<-filter(mydata13,sensordatavalues__value_type=="P1")
pm14<-filter(mydata14,�..sensordatavalues__value_type=="P1")
pm15<-filter(mydata15,sensordatavalues__value_type=="P1")
pm16<-filter(mydata16,sensordatavalues__value_type=="P1")
pm17<-filter(mydata17,sensordatavalues__value_type=="P1")
pm18<-filter(mydata18,�..sensordatavalues__value_type=="P1")
pm19<-filter(mydata19,sensordatavalues__value_type=="P1")
pm20<-filter(mydata20,sensordatavalues__value_type=="P1")
pm21<-filter(mydata21,sensordatavalues__value_type=="P1")
pm22<-filter(mydata22,sensordatavalues__value_type=="P1")
pm23<-filter(mydata23,sensordatavalues__value_type=="P1")
pm24<-filter(mydata24,sensordatavalues__value_type=="P1")
pm25<-filter(mydata25,sensordatavalues__value_type=="P1")
pm26<-filter(mydata26,sensordatavalues__value_type=="P1")
pm27<-filter(mydata27,sensordatavalues__value_type=="P1")
pm28<-filter(mydata28,sensordatavalues__value_type=="P1")
pm29<-filter(mydata29,sensordatavalues__value_type=="P1")
pm30<-filter(mydata30,sensordatavalues__value_type=="P1")
pm31<-filter(mydata31,sensordatavalues__value_type=="P1")
pm32<-filter(mydata32,sensordatavalues__value_type=="P1")
pm33<-filter(mydata33,sensordatavalues__value_type=="P1")
pm34<-filter(mydata34,sensordatavalues__value_type=="P1")
pm35<-filter(mydata35,sensordatavalues__value_type=="P1")
pm36<-filter(mydata36,sensordatavalues__value_type=="P1")
pm37<-filter(mydata37,sensordatavalues__value_type=="P1")
pm38<-filter(mydata38,sensordatavalues__value_type=="P1")
pm39<-filter(mydata39,sensordatavalues__value_type=="P1")
pm40<-filter(mydata40,sensordatavalues__value_type=="P1")
pm41<-filter(mydata41,sensordatavalues__value_type=="P1")
pm42<-filter(mydata42,sensordatavalues__value_type=="P1")
pm43<-filter(mydata43,sensordatavalues__value_type=="P1")
pm44<-filter(mydata44,sensordatavalues__value_type=="P1")
pm45<-filter(mydata45,sensordatavalues__value_type=="P1")
 
m1<-pm01$sensordatavalues__value
m2<-pm02$sensordatavalues__value
m3<- (pm03$sensordatavalues__value)
m4<- (pm04$sensordatavalues__value)
m5<- (pm05$sensordatavalues__value)
m6<- (pm06$sensordatavalues__value)
m7<- (pm07$�..sensordatavalues__value)
m8<- (pm08$sensordatavalues__value)
m9<- (pm09$sensordatavalues__value)
m10<- (pm10$sensordatavalues__value)
m11<- (pm11$sensordatavalues__value)
m12<- (pm12$sensordatavalues__value)
m13<- (pm13$sensordatavalues__value)
m14<- (pm14$sensordatavalues__value)
m15<- (pm15$sensordatavalues__value)
m16<- (pm16$sensordatavalues__value)
m17<- (pm17$sensordatavalues__value)
m18<- (pm18$sensordatavalues__value)
m19<- (pm19$sensordatavalues__value)
m20<- (pm20$sensordatavalues__value)
m21<- (pm21$sensordatavalues__value)
m22<- (pm22$sensordatavalues__value)
m23<- (pm23$sensordatavalues__value)
m24<- (pm24$sensordatavalues__value)
m25<- (pm25$sensordatavalues__value)
m26<- (pm26$sensordatavalues__value)
m27<- (pm27$sensordatavalues__value)
m28<- (pm28$sensordatavalues__value)
m29<- (pm29$sensordatavalues__value)
m30<- (pm30$sensordatavalues__value)
m31<- (pm31$sensordatavalues__value)
m32<- (pm32$sensordatavalues__value)
m33<- (pm33$sensordatavalues__value)
m34<- (pm34$sensordatavalues__value)
m35<- (pm35$sensordatavalues__value)
m36<- (pm36$sensordatavalues__value)
m37<- (pm37$sensordatavalues__value)
m38<- (pm38$sensordatavalues__value)
m39<- (pm39$sensordatavalues__value)
m40<- (pm40$sensordatavalues__value)
m41<- (pm41$sensordatavalues__value)
m42<- (pm42$sensordatavalues__value)
m43<- (pm43$sensordatavalues__value)
m44<- (pm44$sensordatavalues__value)
m45<- (pm45$sensordatavalues__value)
allpm10<-c(m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26,m27,m28,m29,m30,m31,m32,m33,m34,m35,m36,m37,m38,m39,m40,m41,m42,m43,m44,m45)
#################################

pm01<-filter(mydata1,sensordatavalues__value_type=="temperature") 
pm02<-filter(mydata2,sensordatavalues__value_type=="temperature")
pm03<-filter(mydata3,sensordatavalues__value_type=="temperature")
pm04<-filter(mydata4,sensordatavalues__value_type=="temperature")
pm05<-filter(mydata5,sensordatavalues__value_type=="temperature")
pm06<-filter(mydata6,sensordatavalues__value_type=="temperature")
pm07<-filter(mydata7,sensordatavalues__value_type=="temperature")
pm08<-filter(mydata8,sensordatavalues__value_type=="temperature")
pm09<-filter(mydata9,sensordatavalues__value_type=="temperature")
pm10<-filter(mydata10,sensordatavalues__value_type=="temperature")
pm11<-filter(mydata11,�..sensordatavalues__value_type=="temperature")
pm12<-filter(mydata12,sensordatavalues__value_type=="temperature")
pm13<-filter(mydata13,sensordatavalues__value_type=="temperature")
pm14<-filter(mydata14,�..sensordatavalues__value_type=="temperature")
pm15<-filter(mydata15,sensordatavalues__value_type=="temperature")
pm16<-filter(mydata16,sensordatavalues__value_type=="temperature")
pm17<-filter(mydata17,sensordatavalues__value_type=="temperature")
pm18<-filter(mydata18,�..sensordatavalues__value_type=="temperature")
pm19<-filter(mydata19,sensordatavalues__value_type=="temperature")
pm20<-filter(mydata20,sensordatavalues__value_type=="temperature")
pm21<-filter(mydata21,sensordatavalues__value_type=="temperature")
pm22<-filter(mydata22,sensordatavalues__value_type=="temperature")
pm23<-filter(mydata23,sensordatavalues__value_type=="temperature")
pm24<-filter(mydata24,sensordatavalues__value_type=="temperature")
pm25<-filter(mydata25,sensordatavalues__value_type=="temperature")
pm26<-filter(mydata26,sensordatavalues__value_type=="temperature")
pm27<-filter(mydata27,sensordatavalues__value_type=="temperature")
pm28<-filter(mydata28,sensordatavalues__value_type=="temperature")
pm29<-filter(mydata29,sensordatavalues__value_type=="temperature")
pm30<-filter(mydata30,sensordatavalues__value_type=="temperature")
pm31<-filter(mydata31,sensordatavalues__value_type=="temperature")
pm32<-filter(mydata32,sensordatavalues__value_type=="temperature")
pm33<-filter(mydata33,sensordatavalues__value_type=="temperature")
pm34<-filter(mydata34,sensordatavalues__value_type=="temperature")
pm35<-filter(mydata35,sensordatavalues__value_type=="temperature")
pm36<-filter(mydata36,sensordatavalues__value_type=="temperature")
pm37<-filter(mydata37,sensordatavalues__value_type=="temperature")
pm38<-filter(mydata38,sensordatavalues__value_type=="temperature")
pm39<-filter(mydata39,sensordatavalues__value_type=="temperature")
pm40<-filter(mydata40,sensordatavalues__value_type=="temperature")
pm41<-filter(mydata41,sensordatavalues__value_type=="temperature")
pm42<-filter(mydata42,sensordatavalues__value_type=="temperature")
pm43<-filter(mydata43,sensordatavalues__value_type=="temperature")
pm44<-filter(mydata44,sensordatavalues__value_type=="temperature")
pm45<-filter(mydata45,sensordatavalues__value_type=="temperature")


 
m1<- (pm01$sensordatavalues__value)
m2<- (pm02$sensordatavalues__value)
m3<- (pm03$sensordatavalues__value)
m4<- (pm04$sensordatavalues__value)
m5<- (pm05$sensordatavalues__value)
m6<- (pm06$sensordatavalues__value)
m7<- (pm07$�..sensordatavalues__value)
m8<- (pm08$sensordatavalues__value)
m9<- (pm09$sensordatavalues__value)
m10<- (pm10$sensordatavalues__value)
m11<- (pm11$sensordatavalues__value)
m12<- (pm12$sensordatavalues__value)
m13<- (pm13$sensordatavalues__value)
m14<- (pm14$sensordatavalues__value)
m15<- (pm15$sensordatavalues__value)
m16<- (pm16$sensordatavalues__value)
m17<- (pm17$sensordatavalues__value)
m18<- (pm18$sensordatavalues__value)
m19<- (pm19$sensordatavalues__value)
m20<- (pm20$sensordatavalues__value)
m21<- (pm21$sensordatavalues__value)
m22<- (pm22$sensordatavalues__value)
m23<- (pm23$sensordatavalues__value)
m24<- (pm24$sensordatavalues__value)
m25<- (pm25$sensordatavalues__value)
m26<- (pm26$sensordatavalues__value)
m27<- (pm27$sensordatavalues__value)
m28<- (pm28$sensordatavalues__value)
m29<- (pm29$sensordatavalues__value)
m30<- (pm30$sensordatavalues__value)
m31<- (pm31$sensordatavalues__value)
m32<- (pm32$sensordatavalues__value)
m33<- (pm33$sensordatavalues__value)
m34<- (pm34$sensordatavalues__value)
m35<- (pm35$sensordatavalues__value)
m36<- (pm36$sensordatavalues__value)
m37<- (pm37$sensordatavalues__value)
m38<- (pm38$sensordatavalues__value)
m39<- (pm39$sensordatavalues__value)
m40<- (pm40$sensordatavalues__value)
m41<- (pm41$sensordatavalues__value)
m42<- (pm42$sensordatavalues__value)
m43<- (pm43$sensordatavalues__value)
m44<- (pm44$sensordatavalues__value)
m45<- (pm45$sensordatavalues__value)


temp<-c(m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26,m27,m28,m29,m30,m31,m32,m33,m34,m35,m36,m37,m38,m39,m40,m41,m42,m43,m44,m45)
##################################################

pm01<-filter(mydata1,sensordatavalues__value_type=="humidity") 
pm02<-filter(mydata2,sensordatavalues__value_type=="humidity")
pm03<-filter(mydata3,sensordatavalues__value_type=="humidity")
pm04<-filter(mydata4,sensordatavalues__value_type=="humidity")
pm05<-filter(mydata5,sensordatavalues__value_type=="humidity")
pm06<-filter(mydata6,sensordatavalues__value_type=="humidity")
pm07<-filter(mydata7,sensordatavalues__value_type=="humidity")
pm08<-filter(mydata8,sensordatavalues__value_type=="humidity")
pm09<-filter(mydata9,sensordatavalues__value_type=="humidity")
pm10<-filter(mydata10,sensordatavalues__value_type=="humidity")
pm11<-filter(mydata11,�..sensordatavalues__value_type=="humidity")
pm12<-filter(mydata12,sensordatavalues__value_type=="humidity")
pm13<-filter(mydata13,sensordatavalues__value_type=="humidity")
pm14<-filter(mydata14,�..sensordatavalues__value_type=="humidity")
pm15<-filter(mydata15,sensordatavalues__value_type=="humidity")
pm16<-filter(mydata16,sensordatavalues__value_type=="humidity")
pm17<-filter(mydata17,sensordatavalues__value_type=="humidity")
pm18<-filter(mydata18,�..sensordatavalues__value_type=="humidity")
pm19<-filter(mydata19,sensordatavalues__value_type=="humidity")
pm20<-filter(mydata20,sensordatavalues__value_type=="humidity")
pm21<-filter(mydata21,sensordatavalues__value_type=="humidity")
pm22<-filter(mydata22,sensordatavalues__value_type=="humidity")
pm23<-filter(mydata23,sensordatavalues__value_type=="humidity")
pm24<-filter(mydata24,sensordatavalues__value_type=="humidity")
pm25<-filter(mydata25,sensordatavalues__value_type=="humidity")
pm26<-filter(mydata26,sensordatavalues__value_type=="humidity")
pm27<-filter(mydata27,sensordatavalues__value_type=="humidity")
pm28<-filter(mydata28,sensordatavalues__value_type=="humidity")
pm29<-filter(mydata29,sensordatavalues__value_type=="humidity")
pm30<-filter(mydata30,sensordatavalues__value_type=="humidity")
pm31<-filter(mydata31,sensordatavalues__value_type=="humidity")
pm32<-filter(mydata32,sensordatavalues__value_type=="humidity")
pm33<-filter(mydata33,sensordatavalues__value_type=="humidity")
pm34<-filter(mydata34,sensordatavalues__value_type=="humidity")
pm35<-filter(mydata35,sensordatavalues__value_type=="humidity")
pm36<-filter(mydata36,sensordatavalues__value_type=="humidity")
pm37<-filter(mydata37,sensordatavalues__value_type=="humidity")
pm38<-filter(mydata38,sensordatavalues__value_type=="humidity")
pm39<-filter(mydata39,sensordatavalues__value_type=="humidity")
pm40<-filter(mydata40,sensordatavalues__value_type=="humidity")
pm41<-filter(mydata41,sensordatavalues__value_type=="humidity")
pm42<-filter(mydata42,sensordatavalues__value_type=="humidity")
pm43<-filter(mydata43,sensordatavalues__value_type=="humidity")
pm44<-filter(mydata44,sensordatavalues__value_type=="humidity")
pm45<-filter(mydata45,sensordatavalues__value_type=="humidity")


m1<- (pm01$sensordatavalues__value)
m2<- (pm02$sensordatavalues__value)
m3<- (pm03$sensordatavalues__value)
m4<- (pm04$sensordatavalues__value)
m5<- (pm05$sensordatavalues__value)
m6<- (pm06$sensordatavalues__value)
m7<- (pm07$�..sensordatavalues__value)
m8<- (pm08$sensordatavalues__value)
m9<- (pm09$sensordatavalues__value)
m10<- (pm10$sensordatavalues__value)
m11<- (pm11$sensordatavalues__value)
m12<- (pm12$sensordatavalues__value)
m13<- (pm13$sensordatavalues__value)
m14<- (pm14$sensordatavalues__value)
m15<- (pm15$sensordatavalues__value)
m16<- (pm16$sensordatavalues__value)
m17<- (pm17$sensordatavalues__value)
m18<- (pm18$sensordatavalues__value)
m19<- (pm19$sensordatavalues__value)
m20<- (pm20$sensordatavalues__value)
m21<- (pm21$sensordatavalues__value)
m22<- (pm22$sensordatavalues__value)
m23<- (pm23$sensordatavalues__value)
m24<- (pm24$sensordatavalues__value)
m25<- (pm25$sensordatavalues__value)
m26<- (pm26$sensordatavalues__value)
m27<- (pm27$sensordatavalues__value)
m28<- (pm28$sensordatavalues__value)
m29<- (pm29$sensordatavalues__value)
m30<- (pm30$sensordatavalues__value)
m31<- (pm31$sensordatavalues__value)
m32<- (pm32$sensordatavalues__value)
m33<- (pm33$sensordatavalues__value)
m34<- (pm34$sensordatavalues__value)
m35<- (pm35$sensordatavalues__value)
m36<- (pm36$sensordatavalues__value)
m37<- (pm37$sensordatavalues__value)
m38<- (pm38$sensordatavalues__value)
m39<- (pm39$sensordatavalues__value)
m40<- (pm40$sensordatavalues__value)
m41<- (pm41$sensordatavalues__value)
m42<- (pm42$sensordatavalues__value)
m43<- (pm43$sensordatavalues__value)
m44<- (pm44$sensordatavalues__value)
m45<- (pm45$sensordatavalues__value)
 
humidity<-c(m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26,m27,m28,m29,m30,m31,m32,m33,m34,m35,m36,m37,m38,m39,m40,m41,m42,m43,m44,m45)
################################################################



pm01<-filter(mydata1,sensordatavalues__value_type=="P2") 
pm02<-filter(mydata2,sensordatavalues__value_type=="P2")
pm03<-filter(mydata3,sensordatavalues__value_type=="P2")
pm04<-filter(mydata4,sensordatavalues__value_type=="P2")
pm05<-filter(mydata5,sensordatavalues__value_type=="P2")
pm06<-filter(mydata6,sensordatavalues__value_type=="P2")
pm07<-filter(mydata7,sensordatavalues__value_type=="P2")
pm08<-filter(mydata8,sensordatavalues__value_type=="P2")
pm09<-filter(mydata9,sensordatavalues__value_type=="P2")
pm10<-filter(mydata10,sensordatavalues__value_type=="P2")
pm11<-filter(mydata11,�..sensordatavalues__value_type=="P2")
pm12<-filter(mydata12,sensordatavalues__value_type=="P2")
pm13<-filter(mydata13,sensordatavalues__value_type=="P2")
pm14<-filter(mydata14,�..sensordatavalues__value_type=="P2")
pm15<-filter(mydata15,sensordatavalues__value_type=="P2")
pm16<-filter(mydata16,sensordatavalues__value_type=="P2")
pm17<-filter(mydata17,sensordatavalues__value_type=="P2")
pm18<-filter(mydata18,�..sensordatavalues__value_type=="P2")
pm19<-filter(mydata19,sensordatavalues__value_type=="P2")
pm20<-filter(mydata20,sensordatavalues__value_type=="P2")
pm21<-filter(mydata21,sensordatavalues__value_type=="P2")
pm22<-filter(mydata22,sensordatavalues__value_type=="P2")
pm23<-filter(mydata23,sensordatavalues__value_type=="P2")
pm24<-filter(mydata24,sensordatavalues__value_type=="P2")
pm25<-filter(mydata25,sensordatavalues__value_type=="P2")
pm26<-filter(mydata26,sensordatavalues__value_type=="P2")
pm27<-filter(mydata27,sensordatavalues__value_type=="P2")
pm28<-filter(mydata28,sensordatavalues__value_type=="P2")
pm29<-filter(mydata29,sensordatavalues__value_type=="P2")
pm30<-filter(mydata30,sensordatavalues__value_type=="P2")
pm31<-filter(mydata31,sensordatavalues__value_type=="P2")
pm32<-filter(mydata32,sensordatavalues__value_type=="P2")
pm33<-filter(mydata33,sensordatavalues__value_type=="P2")
pm34<-filter(mydata34,sensordatavalues__value_type=="P2")
pm35<-filter(mydata35,sensordatavalues__value_type=="P2")
pm36<-filter(mydata36,sensordatavalues__value_type=="P2")
pm37<-filter(mydata37,sensordatavalues__value_type=="P2")
pm38<-filter(mydata38,sensordatavalues__value_type=="P2")
pm39<-filter(mydata39,sensordatavalues__value_type=="P2")
pm40<-filter(mydata40,sensordatavalues__value_type=="P2")
pm41<-filter(mydata41,sensordatavalues__value_type=="P2")
pm42<-filter(mydata42,sensordatavalues__value_type=="P2")
pm43<-filter(mydata43,sensordatavalues__value_type=="P2")
pm44<-filter(mydata44,sensordatavalues__value_type=="P2")
pm45<-filter(mydata45,sensordatavalues__value_type=="P2")


m1<- (pm01$sensordatavalues__value)
m2<- (pm02$sensordatavalues__value)
m3<- (pm03$sensordatavalues__value)
m4<- (pm04$sensordatavalues__value)
m5<- (pm05$sensordatavalues__value)
m6<- (pm06$sensordatavalues__value)
m7<- (pm07$�..sensordatavalues__value)
m8<- (pm08$sensordatavalues__value)
m9<- (pm09$sensordatavalues__value)
m10<- (pm10$sensordatavalues__value)
m11<- (pm11$sensordatavalues__value)
m12<- (pm12$sensordatavalues__value)
m13<- (pm13$sensordatavalues__value)
m14<- (pm14$sensordatavalues__value)
m15<- (pm15$sensordatavalues__value)
m16<- (pm16$sensordatavalues__value)
m17<- (pm17$sensordatavalues__value)
m18<- (pm18$sensordatavalues__value)
m19<- (pm19$sensordatavalues__value)
m20<- (pm20$sensordatavalues__value)
m21<- (pm21$sensordatavalues__value)
m22<- (pm22$sensordatavalues__value)
m23<- (pm23$sensordatavalues__value)
m24<- (pm24$sensordatavalues__value)
m25<- (pm25$sensordatavalues__value)
m26<- (pm26$sensordatavalues__value)
m27<- (pm27$sensordatavalues__value)
m28<- (pm28$sensordatavalues__value)
m29<- (pm29$sensordatavalues__value)
m30<- (pm30$sensordatavalues__value)
m31<- (pm31$sensordatavalues__value)
m32<- (pm32$sensordatavalues__value)
m33<- (pm33$sensordatavalues__value)
m34<- (pm34$sensordatavalues__value)
m35<- (pm35$sensordatavalues__value)
m36<- (pm36$sensordatavalues__value)
m37<- (pm37$sensordatavalues__value)
m38<- (pm38$sensordatavalues__value)
m39<- (pm39$sensordatavalues__value)
m40<- (pm40$sensordatavalues__value)
m41<- (pm41$sensordatavalues__value)
m42<- (pm42$sensordatavalues__value)
m43<- (pm43$sensordatavalues__value)
m44<- (pm44$sensordatavalues__value)
m45<- (pm45$sensordatavalues__value)
 

allpm25<-c(m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16,m17,m18,m19,m20,m21,m22,m23,m24,m25,m26,m27,m28,m29,m30,m31,m32,m33,m34,m35,m36,m37,m38,m39,m40,m41,m42,m43,m44,m45)


str(allpm10)
str(allpm25)
str(temp)
str(humidity)
qz<-cbind(allpm25,allpm10,temp,humidity)
colnames(qz)<-c("pm2.5","pm10","temp","humidty")
write.csv(file="meandataset4.csv",qz)
#################################################################################



