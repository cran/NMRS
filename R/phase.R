`phase` <-
function(fid.out,phc0,phc1,pivot){
ref= pivot*fid.out$SWHz/fid.out$PPM[length(fid.out$PPM)]
phc0rad= 2*pi*phc0/360;
phc1rad= 2*pi*phc1/360;

refphase = (fid.out$t-ref)/fid.out$si;
a= phc0rad + refphase*phc1rad;
rawrA= Re(fid.out$fid)*cos(a) + Im(fid.out$fid)*sin(a);
rawiD= Im(fid.out$fid)*cos(a) - Re(fid.out$fid)*sin(a);
rawphase= complex(real=rawrA,imaginary=rawiD);
espec= fftshift(fft(rawphase)); 
#plot(PPM,Re(espec),"l",xlim=c(PPM[length(PPM)],0)) 
fid.out$spectrum=espec
return(fid.out)
}

