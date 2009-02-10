`NMRS` <-
function(){
 i=0
answer="yes"
datos.fid2<-list()
 round2<-function(x)
{
a<-round(x,3)
if(a<x)
a<-a+0.001
a<-round(a,3)
return(a)
}
round3<-function(x)
{
a<-round(x,3)
if(a>x)
a<-a-0.001
a<-round(a,3)
return(a)
}
Save.Data<-function(x)
{
  fileName<-tclvalue(tkgetSaveFile())
  write.table(x, file = fileName, append = FALSE, quote = FALSE, sep = "\t",
    eol = "\n", na = "NA", dec = ".", row.names = FALSE,
    col.names = FALSE, qmethod = c("escape", "double")) 
} 


while (answer=="yes")
  {
Met.FID()
i<-i+1
datos.fid2[[i]]<-list(datos=spect.fid)

if (nexfid=="yes")
{
answer<-tclvalue(tkmessageBox(message="Do you want to import another spectrum?",icon="question",type="yesno",default="yes"))
}
if (nexfid=="no")
{
answer<-"no"
}
}
f1<-c()
f2<-c()
for(j in 1:i)
{lim<-as.data.frame(datos.fid2[[j]])
f1[j]<-lim[1,1]
f2[j]<-lim[dim(lim)[1],1]
}
cm1<-which(f1==max(f1))[1]
cm2<-as.data.frame(datos.fid2[[cm1]])
cm2<-cm2[c(which(cm2[,1]==max(f1))[1]:which(round(cm2[,1],3)==round3(min(f2)))[1]),]
cm3<-dim(cm2)[1]
f1<<-f1
f2<<-f2
#p<-as.data.frame(datos.fid2[[1]])
for(j in 1:i)
{
p<-as.data.frame(datos.fid2[[j]])
p<-p[c(which(round(p[,1],3)==round2(max(f1)))[1]:(which(round(p[,1],3)==round2(max(f1)))[1]+cm3-100)),]
datos.fid2[[j]]<-p
}
datos<-as.data.frame(datos.fid2[[1]])
if (i>1)
{
for(j in 2:i)
{
p<-as.data.frame(datos.fid2[[j]])
datos<-cbind(datos,p[,2])
}
}
rownames(datos)<-c(1:dim(datos)[1])

#ans2<-tclvalue(tkmessageBox(title="Import Data",message="Select the info file",icon="info",type="ok"))
  
#fileName2<-tclvalue(tkgetOpenFile()) #open info file
  #if (!nchar(fileName2))
    #tkmessageBox(message="No file selected!")
 #else
    #tkmessageBox(message=paste("The selected file is",fileName2))
  #info=read.table(fileName2,sep="\t",header=TRUE)
#rm("nexfid","spect.fid","xPlotCoord")
ans1<-tclvalue(tkmessageBox(title="Manual cut",message="Remove spectral area?",icon="info",type="yesno"))
if(ans1=="yes")
datos<-Manual.cut(datos)
ans4<-tclvalue(tkmessageBox(title="Manual cut",message="the spectral area selected is correct ?",icon="info",type="yesno"))
if(ans4=="no")
datos<-Manual.cut(datos)

ans3<-tclvalue(tkmessageBox(title="Baseline Correction",message="Adjust the baseline?",icon="info",type="yesno"))
if(ans3=="yes")
Baseline.Correction(datos)
Spectra<<-datos
ans2<-tclvalue(tkmessageBox(title="Save Data",message="Save data as txt file?",icon="info",type="yesno"))
if(ans2=="yes")
Save.Data(Spectra)
return(Spectra)
}

