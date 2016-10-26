; integrated intensity map 
;   format changed  6/5/2003, f10.5 -> f13.5
;   input  = binary data
;   OS X, IDL ver 5.6, png file
;  for 3D data cube   2015.4.12
;
pro cube,name,ps=ps, gif=gif, log=log, conv=conv
loadct,39
;loadct,5
nd= 128 
w2=fltarr(100)
rdir='../'
dir=['.']
;file='sp'+string(indgen(nlevel)+1,format='(i2)')+'.d'
;transition=string(indgen(nlevel)+1,format='(i2)')+'-'+string(indgen(nlevel),format='(i1)')
  ;;  transition=['1-0','2-1','3-2','4-3','5-4','6-5']

if keyword_set(gif) then begin
 print,'######## keyword gif set ########'
  set_plot,'z'
; device,set_resolution=[100,100]
endif

filename=''

print,'Input data (eg. sp03.cube)'
read, filename


if keyword_set(gif) then begin
; psetup,/landscape,/next,/high_res,/color,/ENCAPSULATE,longside=21,shortside=9
  num=['0','1','2','3','4','5','6','7','8','9']
endif else begin
window,0,Xsize=400,Ysize=400,XPos=0,YPos=0
window,1,Xsize=400,Ysize=400,XPos=400,YPos=0
window,2,Xsize=400,Ysize=400,XPos=800,YPos=0
window,3,Xsize=400,Ysize=400,XPos=0,YPos=500
window,4,Xsize=400,Ysize=400,XPos=400,YPos=500
window,5,Xsize=400,Ysize=400,XPos=400,YPos=500
endelse

NG = 0L
velmax = 0L
openr,2,filename,/f77_unformatted,/swap_endian
;openr,2,'spnlev.b',/f77_unformatted,/swap_endian
;openr,2,'spnlev.b',/f77_unformatted
 readu,2, NG, velmax 
close,2
print,'input:  ng, velmax ', ng, velmax
;ng = 128
;velmax = 10
if ng ne nd then begin
 print, 'ne is different from ng'
 stop
endif
tmp = Float(ng)*Float(ng)*Float(velmax)
nsize = Long(tmp)
print,'nsize = ', nsize
intensity = fltarr(nsize)
radv = FltArr(velmax)

;help, intensity
;help, radv 

idvvel = FltArr(nd,nd, velmax)
;radial_v = FltArr(velmax)

;openr,2,'spnlev.b',/f77_unformatted,/swap_endian
openr,2,filename,/f77_unformatted,/swap_endian
 ;openr,2,'spnlev.b',/f77_unformatted
 readu,2, NG, velmax 
 readu,2, radv, intensity 
close,2

print,'input:  ng, velmax ', ng, velmax
;ng = 128
idvvel(*,*,*) = intensity(*)
;radial_v(*) = radv(*)

print,'max, min of intensity', max(intensity), min(intensity), mean(intensity)
print,'max, min of radv', max(radv), min(radv)
;;;;; writing FITS file
 filename_fits=filename+'.fits'
 mwrfits,idvvel, filename_fits   ; from NASA  need 2009 version of fxaddpar.pro
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;read,'levmax = ', levmax

;i = level

lchan =0

;print, 'output level = ', level

;for L=0, nlevel-1 do begin
 for lchan=0, velmax-1, 1 do begin;  number of channels
;for lchan=0, 3 do begin;  number of channels

;help, lchan
print,'channel: ', lchan


 i = lchan+1

;iiii=i/1000
;iiii=i/100-iiiii*10
;iii = i/10 - iiii*10 -iiiii*100


;       iiiii=i/1000
;       iiii=i/100-iiiii*10
;       iii = i/10 - iiii*10 -iiiii*100
        iii = i/10
        ii= i mod 10

;print, 'i,ii,iii',i,ii,iii



;filename='sp'+num(iiiii)+num(iiii)+num(iii)+num(ii)
;filename='sp'+num(iii)+num(ii)+'.d'
;print,'input file: ', filename

; openR, lun, filename,/get_lun
; readF, lun, i1,i2, w1, w2, Format='(i3,2x,i3,e13.3/100f13.5)'

  ; store data
; for j=1,nd*nd do begin
;   readF, lun, i1,i2, w1, w2, Format='(i3,2x,i3,e13.3/100f13.5)'
;     IdV[i2-1,i1-1]=w1
;     IdV[i2-1,i1-1]= w2[L]
; endfor

  ; release
; free_lun,lun

;idv = extract_slice(idvvel,ng,ng,ng/2,ng/2,level-1,0,0,90,out_val=1e-8)
;print,'calling extract_slice...', lchan
nslice = lchan 
idv = extract_slice(idvvel,ng,ng,ng/2,ng/2, nslice,0,0,0,out_val=1e-8)
;endfor ;debug


  x=findgen(nd)-(nd-1)*0.5
  y=findgen(nd)-(nd-1)*0.5

; if keyword_set(gif) then begin
; endif else begin
;  Wset, L
; endelse


print, 'max, min, mean', max(idv), min(idv), mean(idv)


;if l eq 0 then begin
; cmax = 13790 *0.4
; cmin = 818.4 *0.4
;endif
;if l eq 1 then begin
; cmax = 7559  *0.4
; cmin =  186.9  *0.4
;endif
;if l eq 2 then begin
; cmax = 8235  *0.4
; cmin =  35.6  *0.4
;endif
;if l eq 3 then begin
; cmax = 7210  *0.4
; cmin =  6.1  *0.4
;endif
;
;if l gt 5 then begin
;  idv = 100. * idv
;endif


;cmax = max(idv)*1.5
;cmin = min(idv)*0.5
;cmax = max(idv)*0.8
;cmin = min(idv)*0.4
 cmax = 20.
 cmin = 0. 

ncontours = 5.


if keyword_set(log) then begin
        cmax = 20. 
        cmin = 0.1 
	step = (alog10(cmax) - alog10(cmin))/ 10.0 
	clevels = (indgen(10)*step) + alog10(cmin) 
;print,'clevels' , clevels
endif else begin
	step = ((cmax) - (cmin))/ncontours
	step2 = ((cmax*0.5) - (cmin))/ncontours
	clevels = indgen(ncontours)*step + cmin 
	clevels2 = indgen(ncontours)*step2 + cmin
;print,'clevels' , clevels
endelse

;-- covolve with the psf ---  FWHM = 50 or 5 or 2
if keyword_set(conv)then begin
  psf = psf_Gaussian( NPIXEL= 5, FWHM=5 , /NORMALIZE)
  imconv = smooth(convolve( IdV, psf, FT_PSF = psf_FT ),5)
endif else begin
  imconv = IdV
endelse
;---------------------------

IF Keyword_Set(gif) THEN BEGIN $

if keyword_set(log) then begin
  contour, alog10(imconv),x,y,/follow,/fill,/Isotropic,xstyle=1,xrange=[x[0],x[nd-1]],ystyle=1,yrange=[y[0],y[nd-1]], levels = clevels
; contour, alog10(imconv),x,y,/follow,/overplot,/Isotropic,xstyle=1,xrange=[x[0],x[nd-1]],ystyle=1,yrange=[y[0],y[nd-1]], levels = clevels
   xyouts, 0.400,0.97,radv[nslice],/normal,charsize=1.5
   xyouts, 0.700,0.97,'km/s',/normal,charsize=1.5

          colorbar,charsize=1.2,max=alog10(cmax),min=alog10(cmin), $
                position=[0.9,.10,0.92,0.7],/vertical, divisions=10, $
                format='(F7.1)', title='Intensity (K km s!u-1!n)'

endif else begin

  contour, (imconv),x,y,/follow,/fill,/Isotropic,xstyle=1,xrange=[x[0],x[nd-1]],ystyle=1,yrange=[y[0],y[nd-1]], levels = clevels
; contour, (imconv),x,y,/follow,/overplot,/Isotropic,xstyle=1,xrange=[x[0],x[nd-1]],ystyle=1,yrange=[y[0],y[nd-1]], levels = clevels2
  ;xyouts, 0.475,0.97,transition[L],/normal,charsize=1.5
   xyouts, 0.400,0.97,radv[nslice],/normal,charsize=1.5
   xyouts, 0.700,0.97,'km/s',/normal,charsize=1.5

          colorbar,charsize=1.2,max=cmax,min=cmin, $
                position=[0.9,.10,0.92,0.7],/vertical, divisions=10, $
                format='(F7.0)', title='Intensity (K km s!u-1!n)'


endelse

; sen = L/1000
; hyaku= L/100 -sen*10
; jyu=L/10 -hyaku*10-sen*100
; iti=L mod 10

;    d=tvrd()
;    set_plot,'X'
;    tv,d

 ;filename=name+num(sen)+num(hyaku)+num(jyu)+num(iti)+'.gif'
 ;outfilename=filename+num(iii)+num(ii)+'.png'
 outfilename='chan'+num(iii)+num(ii)+'.png'
 outfilefits='chan'+num(iii)+num(ii)+'.fits'
 ;filename = test.gif
 ;print, 'png file name:', filename
 ;write_gif,filename,d


r=bindgen(256)
g=bindgen(256)
b=bindgen(256)
 tvlct,  r,g,b,/get
 print, 'png file name:', outfilename
  write_png,outfilename,tvrd(),r,g,b

  writefits,outfilefits, imconv   ; from NASA


ENDIF else begin
  contour, imconv,x,y,/follow,nlevels=10,/fill,/Isotropic,xstyle=1,xrange=[x[0],x[nd-1]],ystyle=1,yrange=[y[0],y[nd-1]]
  contour, imconv,x,y,/follow,nlevels=10,/overplot,/Isotropic,xstyle=1,xrange=[x[0],x[nd-1]],ystyle=1,yrange=[y[0],y[nd-1]]
  ;xyouts, 0.475,0.95,transition[L],/normal
endelse

 endfor   ;velocity channel


end
