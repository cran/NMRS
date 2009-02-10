`Fid` <-
function(){
require(tcltk)
 file<-tclvalue(tkgetOpenFile())
S<-readBin(file, what="int",70000, size = 4, signed = T,
        endian ="swap")
td<-length(S)
si=2*td
rawR<-S[seq(from=1, to=td,by=2)]
fidRaw<-rawR
rawI<-S[seq(from=2, to=td,by=2)]
#plot(c(1:(td/2)),Re(rawR),"l")
mediar<-mean(as.integer(rawR[c((3*length(rawR)/4):length(rawR))]),na.rm = TRUE)
mediai<--mean(as.integer(rawI[c((3*length(rawR)/4):length(rawR))]),na.rm = TRUE)
rawR<-rawR-mediar
rawI<-rawI-mediai
raw<-complex(real=rawR,imaginary=rawI)
raw2<-raw
raw2[c(length(raw):si)]<-0
#plot(c(1:si),Re(raw2),"l")

t<-seq(from=1, to=td/2, length.out=si)
Met.text.radio <- function(entryWidth=7,returnValOnCancel="ID_CANCEL")
  {
entryWidth=7
tt <- tktoplevel()
  fontHeading <- tkfont.create(family="times",size=12,weight="bold")
  fontTextLabel <- tkfont.create(family="times",size=12)
  tkwm.deiconify(tt)
  tkgrab.set(tt)
  tkfocus(tt)
  tkwm.title(tt,"FID")
  tkgrid(tklabel(tt,text="    "))
  tkgrid(tklabel(tt,text="       Acquisition Parameters ",font=fontHeading),sticky="e")
  tkgrid(tklabel(tt,text="    "))
  
  textEntryVarTcl <- tclVar(paste("500.13"))
  textEntryVarTcl2 <- tclVar(paste("8333"))
  textEntryWidget <- tkentry(tt,width=paste(entryWidth),textvariable=textEntryVarTcl)
  textEntryWidget2 <- tkentry(tt,width=paste(entryWidth),textvariable=textEntryVarTcl2)

  tkgrid(tklabel(tt,text="       "))
  tkgrid(tklabel(tt,text="SF   "),textEntryWidget,tklabel(tt,text="       "),sticky="e")
  tkgrid(tklabel(tt,text="SWHz"),textEntryWidget2,tklabel(tt,text="       "),sticky="e")
  tkgrid(tklabel(tt,text="       "))
  tkgrid(tklabel(tt,text="       "))
  
  
   
  onOK <- function()
  {
    ReturnVal <<- list(a=as.numeric(tclvalue(textEntryVarTcl)),
b=as.numeric(tclvalue(textEntryVarTcl2)))
    tkgrab.release(tt)
tkdestroy(tt)
  }
  onCancel <- function()
  {
    ReturnVal <<- 0
     tkdestroy(tt)
   }
  OK.but<-tkbutton(tt,text="   OK   ",command=onOK)
  Cancel.but <-tkbutton(tt,text=" Cancel ",command=onCancel)

  tkgrid(tklabel(tt,text="    "),OK.but,Cancel.but,tklabel(tt,text="    "),sticky="w")
  tkgrid(tklabel(tt,text="    "))

  tkfocus(tt)
  tkraise(tt)
   tkwait.window(tt)
return(ReturnVal)
 }
Met.text.radio()
SF<-ReturnVal$a
SWHz<-ReturnVal$b
XScaleHz<-(SWHz*t/(td/2))
PPM<-XScaleHz/SF


TF<-fftshift(fft((raw2))) #Transformada de Fourier
#plot(fid.out$PPM,Re(TF),"l", xlim=c(fid.out$PPM[length(x$PPM)],0),xlab="PPM", ylab="")

fid.out<-list(fidRaw=fidRaw,fid=raw2,SF=SF, SWHz=SWHz,XScaleHZ=XScaleHz,
 PPM=PPM,spectrum=TF,t=t, si=si )
return(fid.out)

}

