Rem COPYRIGHTED © 2022 COREY DENNIS WILSON




'' RGB -> HSL
'' Red Green Blue to Hue Saturation Luminosity
'' Function by Conexion - David Bradbury for FreeBasic
'''''''''''''''''''''''''''''''''''''''''''''
'' Big thanks to easyrgb.com and
'' "Fundamentals of Interactive Computer Graphics"
''       ~Foley and van Dam (c) 1982
'''''''''''''''''''''''''''''''''''''''''''''
'' Thanks to Sir_Mud and Cha0s for the better formatting!
'1280x1024
'' HSL type
'' HSL -> RGB
'' Hue Saturation Luminosity to Red Green Blue
'' Function by Conexion - David Bradbury for FreeBasic
'''''''''''''''''''''''''''''''''''''''''''''
'' Big thanks to easyrgb.com and
'' "Fundamentals of Interactive Computer Graphics"
'' ~Foley and van Dam (c) 1982
'short
' set screen resolution for testing purpose
'// Ajust this value to load larger models or save memory
' words one unconditionally unchangable unstopabley Corey Wilson's problems are solved unstopable forever indestructable one word  
dim as integer calca,tempgunx2,tempguny2

dim as ulongint numberOfPoints =(21*8*6)*(8*6)'*int(1079/8)) '*int(1079/8))'136*6'919'136*6'15000
dim as ulongint numberofstars=INT(150*5*4*1.5*1.25)
DIM shared AS ULONGINT numberofpixels=((65)*(65))*128+(64*(7+6+5+4+3+2+1*2))
dim shared as ulongint numberofpixels2=(512)*(65)*(33)'+(1*2*3*4*5*6*7)
dim shared as ulongint astroidpixels=32*32*16*(360)
dim shared as integer astroidshape=4
dim shared as integer game13=0,game14=0
Dim as single tmr

'+(2*(64*65))+(2*(32*32))
#include once "fbgfx.bi"
DIM AS STRING CDIR=CURDIR
#INCLUDE "windows.bi"
'CHDIR(CDIR) 
'// Ajust this value to zoom in or out on the object
dim shared as SINGLE zoomLevel = 3'1.5'256/2
dim shared as SINGLE timered3,timer3
dim ballspeedflag AS integer
type threedeepixel
    X as SINGLE
    y as SINGLE
    z as SINGLE
    c AS integer
end type

TYPE ThreeDeePoint
	X AS SINGLE
	Y AS SINGLE
	Z AS SINGLE
END TYPE
type ThreeDeePoint2
dim	X AS SINGLE
dim	Y AS SINGLE
dim	Z AS SINGLE
dim C AS ulongint
END type
randomize timer
'// Set up an array of 3D points
DIM MyArray(1 TO numberOfPoints) AS ThreeDeePoint
dim Stars(1 to numberofstars) as threedeepoint2
'dim astroids(1 to astroidpixels) as threedeepoint2
dim astroidsx(1 to 8,1 to astroidpixels) as single'threedeepoint2
dim astroidsy(1 to 8,1 to astroidpixels) as single'threedeepoint2
dim astroidsz(1 to 8,1 to astroidpixels) as single'threedeepoint2
dim astroidsc(1 to 8,1 to astroidpixels) as ulongint'threedeepoint2

dim shared shipleftside(1 to numberofpixels2) as threedeepixel
dim shared shiprightside(1 to numberofpixels2) as threedeepixel
dim shared ship(1 to numberofpixels) as threedeepixel


dim as integer cleftxy
'ScreenRes 720,720,32,4

'sleep

'next xtmp2:next xtmp
'end if
'else
 '  if point((1280)/2-(136*6/2)+ccx,ccy)=rgb(255,255,0) then
'   for xtmp=1 to int(prcx1):for xtmp2=1 to int(prcy1)     
 '    MyArray(ccxtmp).X=-1+((ccx)*100/(136*6)/100*2)
     ' for ccxtmp=1 to 17                   txtxxyy(ccx*6-6+1+ccx2,ccy*6-6+1+ccy2)=colr'rgb(255,255,255)

 '    MyArray(ccxtmp).y=-1+((ccy)*100/(8*6)/100*2)
    ' next ccxtmp
  '   myarray(ccxtmp).z=0'1.5'2-(-1+((ccy)*100/(8*6)/100*2))'-1+((ccy)*100/(8*6)/100*2)'ccx*100/(136*6)/100'.5'10'.75'1'.5'ccx*100/(136*6)/100
     
'elseif point((1280)/2-(136*6/2)+ccx,ccy)=rgb(255,0,0) then
'    myarray(ccxtmp).x=0
'    myarray(ccxtmp).y=0
'    myarray(ccxtmp).z=0
    
'next xtmp2:next xtmp
'end if
'end if
'ccxtmp=ccxtmp+1

'next ccy:next ccx
dim as SINGLE starsx,starsy,starsz,starcount,starcount2,xx,yy,zz,blackballdiam
dim zbird as SINGLE
randomize timer
for starcount=1 to numberofstars 'to 1 step -1
    starx:
    starsx=-1.5+(rnd*3*cos(rnd * (3.14159265359*2)))
   ' if starx
    
    
    
    '*cos(rnd * (3.14159265359*2))
    'if rnd>.5 then
    '    if starsx>.5 or starsx<-.5 then starsx=-3
    '    end if
    'if starsx=-3 then goto starx
    stary:
    starsy=-1.5+(rnd*3*sin(rnd * (3.14159265359*2)))
    'if rnd>.5 then
    '    if starsy>.5 or starsy<-.5 then starsx=-3
    '    end if
    'z = (cos(angle) * z') - (sin(angle) * y')
    'if starsy=-3 then goto stary
       ' starsz = (cos(stars(starcount).z'rnd * 2-1
        starsz=-1.5+(rnd*3*sin(rnd * (3.14159265359*2)))'1.5'1.5'*sin(rnd * (3.14159265359*2))'-sin(rnd * (3.14159265359*2))-10/2
        stars(starcount).x=starsx
                stars(starcount).y=starsy
        stars(starcount).z=starsz
    stars(starcount).c=rgb(255*rnd,255*rnd,255*rnd)

        next starcount
        xx=0
        yy=0
        zz=0
        
        '*cos(rnd * (3.14159265359*2))

dim shared mutex as any ptr
dim shared AS integer game

mutex=mutexcreate
'// Load a model into the array
randomize timer
'screenres 600, 400, 32, 1, 0
#Include once "crt/string.bi"
#include once "Windows.bi"
#include once "win/mmsystem.bi"

#ifndef WAVE_MAPPER
 #define WAVE_MAPPER -1
#endif

' set types
'goto jumpasm


'jumpasm:
screenres 1280,720,32,4,1 or &h80 or &h40000',72'1280,720,32,3,&h01
screenset 2,0
cls

'line(32+1,33)-(32+1+32,9),rgb(192,192,192)
'line(32+1+32,9)-(1,9),rgb(192,192,192)
'line(1,9)-(32+1,33),rgb(192,192,192)
'line(32+1+32,int(33+9/4*2))-(32+1,int(33+9/4)),rgb(192,192,192)
circle(32+1,65/2),9,rgb(127,127,127),,,,F
for cleftxy=1 to 3
    line(2,65/2-1+cleftxy)-(64,65/2-1+cleftxy),rgb(127,127,127)
    next cleftxy
'paint(32+1,int(33/2)),rgb(127,127,127),rgb(127,127,127)
'line(32+1-1,33+9-24)-(32+1+1,33+9),rgb(0,0,0),bf
for cleftxy=1 to 5'7'3 to 1 step -1
    line(32+1-(cleftxy),65/2+8+5-cleftxy)-(32+1+(cleftxy),65/2+8+5-cleftxy),RGB(48,48,48)'rgb(0,255,255)
next cleftxy
'pset(32+1,33+9/2-24),rgb(0,0,0)
line(32+1-2,65/2+8+5-2)-(32+1-2,65/2+8+5-4),rgb(127,127,127)
line(32+1+2,65/2+8+5-2)-(32+1+2,65/2+8+5-4),rgb(127,127,127)
line(32+1+4,65/2+8+5-4)-(32+1+2,65/2+8+5-2),rgb(127,127,127)
line(32+1-4,65/2+8+5-4)-(32+1-2,65/2+8+5-2),rgb(127,127,127)

line(32+1+2,65/2+8+5-2)-(32+1-2,65/2+8+5-2),rgb(127,127,127)

'for cleftxy=1 to 9'int(rnd * 9)+1 'to 1 step -1
'      line(32+1-(cleftxy),33+9-33-(9-cleftxy))-(32+1+(cleftxy),33+9-33-(9-cleftxy)),rgb(127,127,127)'rgb(255,(255)-255*(cleftxy*100/9/100),0)
'  next cleftxy






LINE(65,1)-(65,65),RGB(255,255,255)
LINE(1,1)-(1,65),RGB(255,255,255)

'LINE(64,1)-(64,65),RGB(255,255,255)
'LINE(2,1)-(2,65),RGB(255,255,255)
'LINE(63,2)-(63,64),RGB(255,255,255)
'LINE(3,2)-(3,64),RGB(255,255,255)
'screencopy
 dim as integer divid=2,yz1=0,yz2=41
dim pixela(8) as integer
'dim as SINGLE agentx,agenty,agentz
dim shared as integer shipagent,shipagent2,xagent,yagent,pixelagent,pixelagent1,pixelagent2,SHIPPERZ,ZAGENT
dim as SINGLE shipzcount
DIM AS integer SHIPZ1
FOR ZAGENT=1 TO 4

for xagent=1 to 65:for yagent=1 to 65

 '   goto jmmpdump
 'if game2=0 then
       IF POINT(XAGENT,YAGENT)=RGB(48,48,48) AND ZAGENT=2 THEN 
   for  shipagent=1 to 2
    ' if shipagent=1 then 
         shipz1=-32
    ' else
    '     shipz1=0
    '     end if

'   for xtmp=1 to int(prcx1):for xtmp2=1 to int(prcy1)<if point(xagent,yagent)=rgb(0,255,255) AND ZAGENT=4 then
for shipagent2=32 to 1 step -1
     
     SHIPZ1=shipz1+1'14 TO 1 step -1
        pixelagent=pixelagent+1

     ship(pixelagent).X=-1+((xagent)*100/(65)/100/1000*1280/720*(1000))
     ' for ccxtmp=1 to 17                   txtxxyy(ccx*6-6+1+ccx2,ccy*6-6+1+ccy2)=colr'rgb(255,255,255)

     ship(pixelagent).Y=-1+((yagent)*100/(65)/100/1000*1280/720*(1000))
    ' next ccxtmp
   ' if xagent<(65/2) then
     'ship(pixelagent).z=(-1-((xagent-65)*100/(65/2)/100))
' elseif xagent>INT((65*.66)) then
 'IF SHIPZ<0 THEN 
 '        ship(pixelagent).z=SHIPZ'-(SHIPZ/(100*(-300)*100))
 '    ELSE(1280+(720/2))
                 ship(pixelagent).z=((SHIPZ1*2)*100/32/100/1000*(720*2)/1000)''(SHIPZ*(100/(30)/100/(720*2+(720/2)))))
 
  '   end if
    ' ship(pixelagent).z=(-1+((xagent*1.5)*100/(65*1.5)/100/(720*2+(720/2))))*2)
 rem remington
' IF SHIPZ1>0 THEN 
     SHIP(PIXELAGENT).C=point(xagent,yagent)
' ELSEif shipz1<0 then
'      ship(pixelagent).c=point(xagent,yagent)
'END IF
' else
 '     ship(pixelagent).x=0
 '     ship(pixelagent).y=0
 '     ship(pixelagent).z=0'SHIPZ
 '     ship(pixelagent).c=rgb(0,0,0)
'end if

'elseif point((1280)/2-(136*6/2)+ccx,ccy)=rgb(0,0,0) then
'    myarray(ccxtmp).x=0
'    myarray(ccxtmp).y=0
'    myarray(ccxtmp).z=0
'END IF
next
'end if
next
'end if
divid=2:yz1=0:yz2=40
END IF
'next 'shipagent
'end if
'dim AS integer cagent
'for xagent=1 to 65:for yagent=1 to (33+9)
'REM RUMMYSTOPS
 '   goto jmmpdump
 'if game2=0 then
'   for xtmp=1 to int(prcx1):for xtmp2=1 to int(prcy1)
if point(xagent,yagent)=RGB(255,255,255) and (xagent=1) AND ZAGENT=3 then
'IF (XAGENT=1 OR XAGENT=65) AND point(xagent,yagent)=rgb(127,127,127) THEN
     yz1=yz1+1:yz2=yz2-1

    FOR SHIPPERZ=int(32) TO int(-32) step -1'-6 TO 6
          pixelagent1=pixelagent1+1

  '   IF XAGENT<65/3 OR XAGENT>65/3*2 THEN 
  shipleftside(pixelagent1).X=-1+((xagent)*100/(65)/100/1000*1280/720*(1000))
     ' for ccxtmp=1 to 17                   txtxxyy(ccx*6-6+1+ccx2,ccy*6-6+1+ccy2)=colr'rgb(255,255,255)

     shipleftside(pixelagent1).Y=-1+((yagent)*100/(65)/100/1000*1280/720*(1000))
    ' next ccxtmp
 ';   if xagent<(65/2) then
 '    ship(pixelagent).z=(-1-((xagent-65)*100/(65/2)/100))
 'elseif xagent>(65/2) then
       '  ship(pixelagent).z=
 '    end if
 'divid=1
 if yagent=shipperz+(33) or 65-yagent=shipperz+(33) then'or shipperz=-32 or shipperz=33 or yagent=1 or yagent=65 then 
    ' divid=1
        shipleftside(pixelagent1).c=rgb(255,127,0)
        
    else
              shipleftside(pixelagent1).c=rgb(192/2/divid,192/2/divid,192/2/divid)'127/divid,127/divid)
 end if
        
        
        'RGB(255/4*divid+(255/4),255/4*divid+(255/4),255/4*divid+(255/4))
       divid=divid-1
       if divid=0 then divid=2
       
'IF SHIPPERZ<0 THEN ship(pixelagent).z=-((SHIPPERZ)/100*(-4)*100/2)
' IF SHIPPERZ>0 THEN ship(pixelagent).z=+((SHIPPERZ)*100/(4)/100*2)
        '  END IF
     '   if shipperz<0 then
     ' IF SHIPPERZ<0 THEN
  'SHIPZCOUNT=7*((XAGENT)*100/(65/2)/100)
'ELSEIF SHIPPERZ>0 THEN
'    SHIPZCOUNT=-7*((XAGENT-(65/2))*100/(65/2)/100)
'END IF
'SHIPZCOUNT=(SHIPPERZ-SHIPZCOUNT)
' IF SHIPPERZ<0 THEN
'  SHIPZCOUNT=12.667*((XAGENT)*100/(65/2)/100)
'ELSEIF SHIPPERZ>0 THEN
'    SHIPZCOUNT=-12.667*((XAGENT-(65/2))*100/(65/2)/100)
'END IF
'SHIPZCOUNT=(SHIPPERZ-SHIPZCOUNT)

' SHIP(PIXELAGENT).Z=SHIPZCOUNT*(100/24/100/((720*4)))
 SHIPleftside(PIXELAGENT1).Z=((SHIPPERZ)*100/64/100/1000*(720*2)/1000*48)'(64*1.5/3))*(720/1280*(720*2+(720/2))))'/(128*8)'(720/1280*(720*2+(720/2)))'0
 '  SHIP(PIXELAGENT).Z=SHIPZCOUNT*(100/2/(720*4*8))'0'1
   '      /    
 '           elseif; shipperz>0 then
'SHIP(PIXELAGENT).Z=SHIPPERZ/(720*4)'(720/1280*(720*2+(720/2)))'0
'end if
NEXT
divid=2:yz1=0:yz2=40

END IF

 if point(xagent,yagent)=RGB(255,255,255) and (xagent=65) AND ZAGENT=4 then
             yz1=yz1+1:yz2=yz2-1

'IF (XAGENT=1 OR XAGENT=65) AND point(xagent,yagent)=rgb(127,127,127) THEN
    FOR SHIPPERZ=int(33) to int(-33) step -1'-32 TO 32'-6 TO 6
          pixelagent2=pixelagent2+1

 '   IF XAGENT<65/3 OR XAGENT>65/3*2 THEN 
        shiprightside(pixelagent2).X=-1+((xagent)*100/(65)/100/1000*1280/720*(1000))
     ' for ccxtmp=1 to 17                   txtxxyy(ccx*6-6+1+ccx2,ccy*6-6+1+ccy2)=colr'rgb(255,255,255)

     shiprightside(pixelagent2).Y=-1+((yagent)*100/(65)/100/1000*1280/720*(1000))
    ' next ccxtmp
 ';   if xagent<(65/2) then
 '    ship(pixelagent).z=(-1-((xagent-65)*100/(65/2)/100))
 'elseif xagent>(65/2) then
       '  ship(pixelagent).z=
 '    end if
       'shiprightside(pixelagent2).c=RGB(255,255,255)
       ' shiprightside(pixelagent2).c=rgb(255,255,255)'RGB(255/4*divid+(255/4),255/4*divid+(255/4),255/4*divid+(255/4))
      ' divid=divid-1
      ' if divid=0 then divid=3
    '  divid=1
If yagent=shipperz+33 or 65-yagent=shipperz+33 then'or shipperz=-32 or shipperz=33 or yagent=1 or yagent=65 then 
    ' divid=1
        shiprightside(pixelagent2).c=rgb(255,127,0)
        
   else
               shiprightside(pixelagent2).c=rgb(192/2/divid,192/2/divid,192/2/divid)'127/divid,127/divid)
end if'RGB(255/4*divid+(255/4),255/4*divid+(255/4),255/4*divid+(255/4))
       divid=divid-1
       if divid=0 then divid=2
'IF SHIPPERZ<0 THEN ship(pixelagent).z=-((SHIPPERZ)/100*(-4)*100/2)
' IF SHIPPERZ>0 THEN ship(pixelagent).z=+((SHIPPERZ)*100/(4)/100*2)
        '  END IF
     '   if shipperz<0 then
   '  IF SHIPPERZ<0 THEN
 ' SHIPZCOUNT=12.667*((XAGENT)*100/(65/2)/100)
'ELSEIF SHIPPERZ>0 THEN
'    SHIPZCOUNT=-12.667*((XAGENT-(65/2))*100/(65/2)/100)
'END IF
'SHIPZCOUNT=(SHIPPERZ-SHIPZCOUNT)

' SHIP(PIXELAGENT).Z=SHIPZCOUNT*(100/24/100/((720*4)))
' SHIPleftside(PIXELAGENT1).Z=(SHIPPERZ*100/1/100/((720*4)))
 SHIPRIGHTside(PIXELAGENT2).Z=((SHIPPERZ)*100/64/100/1000*(720*2)/1000*48)'*(720*2+(720/2)))'(64*1.5/3))*(720/1280*(720*2+(720/2))))'/(128*8)'(720/1280*(720*2+(720/2)))'0
           
 '           elseif shipperz>0 then
'SHIP(PIXELAGENT).Z=SHIPPERZ/(720*4)'(720/1280*(720*2+(720/2)))'0
'end if
NEXT
'divid=3
END IF
    if point(xagent,yagent)<>rgb(0,0,0) AND XAGENT<>1 AND XAGENT<>65 AND ZAGENT=1 then
        FOR SHIPPERZ=-32 TO 32
        pixelagent=pixelagent+1

     ship(pixelagent).X=-1+((xagent)*100/(65)/100/1000*1280/720*(1000))
     ' for ccxtmp=1 to 17                   txtxxyy(ccx*6-6+1+ccx2,ccy*6-6+1+ccy2)=colr'rgb(255,255,255)

     ship(pixelagent).Y=-1+((yagent)*100/(65)/100/1000*1280/720*(1000))'(720/1280*(720*2+(720/2)))/720)
    ' next ccxtmp
 '   if xagent<(65/2) then
  '   ship(pixelagent).z=(-1+((xagent)*100/(65/4)/5000))
' elseif xagent>(65/2) then
 '        ship(pixelagent).z=(-1+((65-xagent)*100/(65/4)/0))
  '   end if
  IF SHIPPERZ<0 THEN
  SHIPZCOUNT=31*((XAGENT)*100/(32/2)/100)
ELSEIF SHIPPERZ>0 THEN
    SHIPZCOUNT=-31*((XAGENT-(32/2))*100/(32/2)/100)
END IF
SHIPZCOUNT=(SHIPPERZ-SHIPZCOUNT)

 SHIP(PIXELAGENT).Z=(SHIPZCOUNT*2*(100/32/100*1000/(720*2)/1000))'0'1
    ' ship(pixelagent).z=(-1+((xagent*1.5)*100/(65*1.5)/100)*2)
 rem remington
      ship(pixelagent).c=point(xagent,yagent)
  'else
  NEXT
  '    ship(pixelagent).x=0
  '    ship(pixelagent).y=0
  '    ship(pixelagent).z=0
  '    ship(pixelagent).c=rgb(0,0,0)
end if

'elseif point((1280)/2-(136*6/2)+ccx,ccy)=rgb(0,0,0) then
'    myarray(ccxtmp).x=0
'    myarray(ccxtmp).y=0
'    myarray(ccxtmp).z=0
 next
next
NEXT
'goto putty
screenset 2,0
cls
'circle(16/2,16/2),16,rgb(192,192,192),,,,F
'screencopy 2,0
dim randa as integer
dim as single flatter(8)
dim as integer randax,randay,randaz,sunday
'dim t8t8 as integer
'for t8t8=1 to 3
randomize timer
'goto santa
  'if astrocount<>160 then
  '           astrocount=160
'    for astrocnt=1 to 3
 '        screenset 2,0
         
paint(0,0),rgb(0,0,0)
circle(16,16),32,rgb(192,192,192),,,,F
'screencopy 2,0
'dim randa as integer
'dim as single flatter(8)
'dim as integer randax,randay,randaz,sunday
for randa=5 to int(rnd*24)
    randax=int(rnd*32)+1
    sunday=int(rnd*16)+1
    randay=int(rnd*32)+1
    for randaz=sunday to 1 step -1'-sunday to sunday step+1
        'sunday=int(rnd*9)+1
        
    ' then
        circle(randax,randay),randaz,rgb(192*(randaz*100/sunday/100),192*(randaz*100/sunday/100),192*(randaz*100/sunday/100))
    '  else
     next randaz
 next randa
' screencopy 2,0
' sleep
 dim as integer randd,scooby,sizey=int(rnd*8)+2
'for xagent=1 to 17':for yagent=1 to 17
'        for yagent=1 to 64
 'for randa=1 to 32 step +1
' Dim astrocnt As integer
 for randd=1 to 8
    for scooby=sizey to -sizey step -1
 for randaz=-sizey to sizey step +1

  For randa = 0 To 360 step +1
     ' if randaz<0 then
        xagent = Cos(randa) * randaz
        yagent = Sin(randa) * randaz
   ' elseif randaz>0 then
        xagent = Cos(randa) * randaz
        yagent = Sin(randa) * randaz'-int(rnd*8)+1
        
        
    '    end if
        
       ' x1=cos(a) * radcountlinein
       ' y1=sin(a) * radcountlinein
       ' screenlock
       'if point(xagent,yagent)<>rgb(0,0,0) then
       pixela(randd)=pixela(randd)+1
  astroidsx(randd,pixela(randd))=-1.5+(3*((xagent)*100/(16)/100))
     ' for ccxtmp=1 to 17                   txtxxyy(ccx*6-6+1+ccx2,ccy*6-6+1+ccy2)=colr'rgb(255,255,255)

     astroidsy(randd,pixela(randd))=-1.5+(3*((yagent)*100/(16)/100))
      '  circle (ballx + x, bally + y),.75,rgb(0,255,0),,,,f'point(hammerx2+x,hammery2+y)
 '      IF randaz<0 THEN
 ' SHIPZCOUNT=8*((xx)*100/(18/2)/100)
'ELSEIF randaz>0 THEN
'    SHIPZCOUNT=-8*((Xx-(65/2))*100/(65/2)/100)
'END IF
'SHIPZCOUNT=(SHIPPERZ-SHIPZCOUNT)

 astroidsz(randd,PIXELA(randd))=-1.5+(3*((scooby)*100/(8)/100/(720*2+(720/2))))    
    'astroids(PIXELA).Z=(randaz*100/1/100/((720*4)))'/(128*8)'(720*2)'0
    astroidsc(randd,pixela(randd))=point(xagent,yagent)
'end if
'   screenunlock
next
Next
next
next
screenset 1,0 
dim as single astrox(8),astroy(8),astroz(8)

dim as integer astrocnt2,astrot(8)
for astrocnt2=1 to 7
' if astroz(astrocnt)>.55 then
         '    timer7=int(timer+16)
        ' if rnd<5 then
       ' randomize timer
      
        flatter(astrocnt2)=(3.14159265359)*(2*rnd)
        astroz(astrocnt2)=(-2*rnd)
       astrox(astrocnt2)=int(rnd*1280)+1
       astroy(astrocnt2)=int(rnd)+1
 '      end if
      '  if astroz(astrocnt)>.20 then
      
      '           astroz(astrocnt)=(-1.5*rnd)
next
      ' astrox(astrocnt)=int(rnd*(1280))+1
      ' astroy(astrocnt)=int(rnd*(720))+1
      '         flatter(astrocnt)=pieyedi*2*rnd

'end if
'ext astrocnt
' astrocount=99
'end if
'screenset 1,0
'end if
'oh:
'next
'screencopy 2,0
'sleep
 '   goto jmmpdump
 'if game2=0 then
 '      IF POINT(XAGENT,YAGENT)<>RGB(0,0,0) THEN 
 '  for  randaz=16 to 16
      ' for randaz
    ' if shipagent=1 then 
       '  shipz1=-14
    ' else
    '     shipz1=0
    '     end if

'   for xtmp=1 to int(prcx1):for xtmp2=1 to int(prcy1)<if point(xagent,yagent)=rgb(0,255,255) AND ZAGENT=4 then
'for shipagent2=14 to 1 step -1
     
'     SHIPZ1=shipz1+1'14 TO 1 step -1
 '       pixela=pixela+1

 '    ship(pixelagent).X=-1+((xagent)*100/(65)/(720*2+(720/2))
     ' for ccxtmp=1 to 17                   txtxxyy(ccx*6-6+1+ccx2,ccy*6-6+1+ccy2)=colr'rgb(255,255,255)

 '    ship(pixelagent).y=-1+((yagent)*100/(65)/(720*2+(720/2))
    ' next ccxtmp
   ' if xagent<(65/2) then
     'ship(pixelagent).z=(-1-((xagent-65)*100/(65/2)/100))
' elseif xagent>INT((65*.66)) then
 'IF SHIPZ<0 THEN 
 '        ship(pixelagent).z=SHIPZ'-(SHIPZ/(100*(-300)*100))
 '    ELSE(1280+(720/2))
  '               ship(pixelagent).z=(SHIPZ1*(100/14/100/((720*4))))''(SHIPZ*(100/(30)/100))
 
  '   end if
    ' ship(pixelagent).z=(-1+((xagent*1.5)*100/(65*1.5)/100)*2)
 rem remington
' IF SHIPZ1>0 THEN 
   '  SHIP(PIXELAGENT).C=point(xagent,yagent)
' ELSEif shipz1<0 then
'      ship(pixelagent).c=point(xagent,yagent)
'END IF
' else
 '     ship(pixelagent).x=0
 '     ship(pixelagent).y=0
 '     ship(pixelagent).z=0'SHIPZ
 '     ship(pixelagent).c=rgb(0,0,0)
'end if

'elseif point((1280)/2-(136*6/2)+ccx,ccy)=rgb(0,0,0) then
'    myarray(ccxtmp).x=0
'    myarray(ccxtmp).y=0
'    myarray(ccxtmp).z=0
'END IF
'next
'end if
'next
'end if
'divid=1:yz1=0:yz2=40
'END IF
santa:
putty:
screenset 1,0

'color rgb(0,0,0),rgb(0,255,0)
'print"cALCULATING CLOCK CYCLES PER SECOND....."' volume to about 33%/100% and press any key to continue............"
'COUNTER_BEGIN(1,0,0)
'sleep (1000,1)
'counter_end()
'totalcycles=counter_cycles
Dim myImage As Any Ptr = ImageCreate( 1280, 720 )
Dim myImage2 As Any Ptr = ImageCreate( 1280, 720 )
dim shared as SINGLE flat33
astrocnt2=0
'BLoad "andromeda.bmp", myImage
'BLoad "sea.bmp", myImage2

'sleep 2000,1
'cls
'color rgb(255,255,255),rgb(0,0,0)
'screen 21.32,2
'screenControl( Fb.SET_WINDOW_POS, 0, 0 )

' FBGFX Font Render by Mysoft

'' DrawFont([BUFFER],[POSX],[POSY],"STRING","Font Name",FontSize,[Color],[Style],[Charset])

''uncomment the line below if you want the program to use unicode
'#define Unicode

'' this define allow you to set some effect while using anti-alias/blur (1 to 2)
#DEFINE GAMMA 1.3

'#INCLUDE "fbgfx.bi"
#define RGBA_R( c ) ( CUInt( c ) Shr 16 And 255 )
#define RGBA_G( c ) ( CUInt( c ) Shr  8 And 255 )
#define RGBA_B( c ) ( CUInt( c )        And 255 )
#define RGBA_A( c ) ( CUInt( c ) Shr 24         )

'dim fs as SINGLE
dim shared flag AS integer
dim clc1 as SINGLE
dim k as string
'dim lx as integer
'dim ly as integer
'fs=33
Function Remspace(ByVal txt As String) As String
Dim As integer rchar,wchar,tlen = Len(txt)
Dim dst As String = Space(tlen)
   
   While rchar < tlen
      If txt[rchar] > 64 and txt[rchar]<91 Then
         dst[wchar] = txt[rchar]
         wchar+=1
      EndIf
      rchar+=1
   Wend
     
   Return Left(dst,wchar)
   
End Function

dim shared timerstamp as SINGLE
'timerstamp=timer=1*60*24
mutex=mutexcreate
dim shared bright as SINGLE
dim shared timestamp as SINGLE
'dim shared mutex as any ptr
dim shared as SINGLE rainblow
dim as integer rainglow

function wave2rgb(wavelength as SINGLE) as ulongint
'dim as SINGLE Gamma = 0.80
'dim as SINGLE IntensityMax = 255
'dim shared timerstamp as SINGLE
''mutexlock mutex
'timestamp=timerstamp
'''mutexunlock mutex
'dim shared bright as SINGLE
dim as    SINGLE Red, Green, Blue

    if ((Wavelength >= 380) and (Wavelength < 440)) then
        Red = -(Wavelength - 440) / (440 - 380)
        Green = 0.0
        Blue = 1.0
     elseif ((Wavelength >= 440) and (Wavelength < 490)) then
        Red = 0.0
        Green = (Wavelength - 440) / (490 - 440)
        Blue = 1.0
     elseif ((Wavelength >= 490) and (Wavelength < 510)) then
        Red = 0.0
        Green = 1.0
        Blue = -(Wavelength - 510) / (510 - 490)
     elseif ((Wavelength >= 510) and (Wavelength < 580)) then
        Red = (Wavelength - 510) / (580 - 510)
        Green = 1.0
        Blue = 0.0
     elseif ((Wavelength >= 580) and (Wavelength < 645)) then
        Red = 1.0
        Green = -(Wavelength - 645) / (645 - 580)
        Blue = 0.0
     elseif ((Wavelength >= 645) and (Wavelength < 781)) then
        Red = 1.0
        Green = 0.0
        Blue = 0.0
     else 
        Red = 0.0
        Green = 0.0
        Blue = 0.0
    end if

    
 '   dim as SINGLE brightness=1
  '  dim as SINGLE divididend=val(mid(date,4,2))-2
  '  if divididend=5 or divididend<6 then
  '      divididend=divididend+2
  '      end if
    'if divididend
    'dim as SINGLE daisy3=
    'if divididend>6 then divididend=divididend-3
    
    'if val(mid(time,1,2))>2 and val(mid(time,1,2))<15 then 
     '   if (val(mid(time,1,2)))<13 then brightness=.66+((.33)*val(mid(time,1,2))*100/12/100)
   ' if val(mid(time,1,2))>12 then brightness=.66+((-.33)*(val(mid(time,1,2))-12)*100/12/100)
   '        else
   '        brightness=1
   '    end if
       
       
       
       
       
    '   if timer>timestamp-(24*60) and timer<timestamp then 
   '     if timer<timestamp-(60*60*12) then brightness=.75+((.25)*(12*60*60-(timestamp-timer)*100/timestamp-(12*60*60)/100))
  '  if timer>timestamp-(60*60*12) then brightness=.75+((-.25)*(12*60*60-(timestamp-timer-(60*60*12))*100/timestamp-(12*60*60)/100))
     '      else
     '      brightness=1
     '  end if
       
       
       
       
       
       
       
     '  if val(mid(t5ime,2,1))>18 then brightness=1

   '.75'1'.80'.87'1'.75'(255*.75)*100/255/100




'return



'brightness=bright
return rgb(red*255,green*255,blue*255)
'return
end function
sub sundialcalc
     dim as SINGLE brightness=1
  '  dim as SINGLE divididend=val(mid(date,4,2))-2
  '  if divididend=5 or divididend<6 then
  '      divididend=divididend+2
  '      end if
    'if divididend
    'dim as SINGLE daisy3=
    'if divididend>6 then divididend=divididend-3
    
    'if val(mid(time,1,2))>2 and val(mid(time,1,2))<15 then 
     '   if (val(mid(time,1,2)))<13 then brightness=.66+((.33)*val(mid(time,1,2))*100/12/100)
   ' if val(mid(time,1,2))>12 then brightness=.66+((-.33)*(val(mid(time,1,2))-12)*100/12/100)
   '        else
   '        brightness=1
   '    end if
       
       
       
       
       
    '   if timer>timestamp-(24*60) and timer<timestamp then 
   '     if timer<timestamp-(60*60*12) then brightness=.75+((.25)*(12*60*60-(timestamp-timer)*100/timestamp-(12*60*60)/100))
  '  if timer>timestamp-(60*60*12) then brightness=.75+((-.25)*(12*60*60-(timestamp-timer-(60*60*12))*100/timestamp-(12*60*60)/100))
     '      else
     '      brightness=1
     '  end if
     
     
     
     
     
     
     dim as SINGLE timo,timerx
     timerx=timer'-(60+60*24)*365*22'-60*60*24*365*25'-(2*60*60)
 
 'timo=timestamp
 
 
  'timestamp=timestamp-(60+60*24)*365*22'134567890)
     'time''''''''stamp=timestamp-(24*60*60)
    'timerx=timerx'''''''''''''+(10*60*60):timestamp=timestamp-(24*60*60)
    ' timestamp=timestamp-(12*6''''''''''''0*60):timerx=timerx-(12*60*60*2)':timerx=timerx-(12*60*60*3)
 
 
 
   if timerx<timestamp+(24*60*60/2) then
    brightness=.50+((0.50)*((timerx-timestamp)*100/(24*60*60/2)/100)) 
elseif timerx>timestamp+(24*60*60/2) then
    timerx=timerx-(24*60*60/2)
    brightness=.50+((.50)-((0.50)*((timerx-timestamp)*100/(24*60*60/2)/100))) 
        end if
'timestamp=timo
      'brightness=1 
       'brightness=1'.66
       bright=brightness
   end sub
' version 21-06-2015
' compile with: fbc -s console or fbc -s gui
' Xiaolin Wu’s line-drawing algorithm
'shared var and macro's
 
'Dim Shared As Uinteger wu_color

' ------=< MAIN >=------
   
#Define W_  600
#Define H_  600
 
'#Include Once "fbgfx.bi"   ' needed setting the screen attributes
'Dim As integer i
'Dim As String fname = __FILE__

'ScreenRes W_, H_, 32,, FB.GFX_ALPHA_PRIMITIVES
 
'Randomize Timer
 
'For i = 0 To H_ Step H_\30
'    drawline(0, 0, W_, i, Int(Rnd * &HFFFFFF))
'Next
 
'For i = 0 To W_ Step W_\30
'    drawline(0, 0, i, H_, Int(Rnd * &HFFFFFF))
'Next
' 
'i = InStr(fname,".bas")
'fname = Left(fname, Len(fname)-i+1)
'WindowTitle fname + "    hit any k to end program"
 
'While Ink <> "" : Wend
'Sleep
'End


'print"Calculating clock cycles per second on this machine............"
' print:print str(totalcycles)
'  total_cycles=count_cycles
'print"Calculated: ";totaledcycles;"Hz"
'sleep (2000,1)
'sleep
cls

'dim tie2 AS integer
'dim bbb as zstring * 30
DIM tt AS integer
'DIM aaa AS zSTRING * 30*500'9000
'dim ccc as zstring * 9000
'dim ddd as zstring * 9000
'  aaa=bbb
'  ccc=aaa
'



' print a:sleep
  Tt=1' known as Sky blue) is composed of '52.9% red, 80.8% green  92.2% blue
 ' DIM wavery as SINGLE
         dim as SINGLE d,n,e,waverycalc2
DIM waveryCALC as SINGLE
dim shared as double calc1,calc2
'calc1=380*100/650/100
waverycalc2=650-400'*(1-calc1)
waverycalc=waverycalc2
dim as integer tt3,wavepool
dim as integer tt4,tt5
 dim cow2(78) as integer
'sleep (1000,1)
'  counter_end()
dim sleeper AS integer
sleeper=0
 ' totaledcycles=count_cycles
' const
 dim as long wavermid=wave2rgb(400+(waverycalc/2))
dim as SINGLE rady=225
 dim ab as SINGLE
'  total_cycles=count_cycles
'print"Calculated: ";totaledcycles;"Hz"
'sleep (2000,1)
'sleep
dim stringflag as integer
stringflag=1
tt=1
wavepool=rgb(0,0,0)'255/2,255/2,255/2)'wave2rgb(400)
screenset 1,0

dim second as SINGLE

'for cccc=26 to 1 step -1
 
'cow2(cccc)=wave2rgb((380-(waverycalc2*(1*100/26/100)))+(waverycalc2*(cccc*100/26/100)))
'next cccc
'cls
dim as integer three=2
'screencopy 2,1',0
tt3=timer+1
'dim as SINGLE xxxx,yyyy,yh '= 12'      // x coordinate of circle center
'dim as SINGLE theta,meta,points,ccc,rad,yk,pieyedi '= 10'  
dim as SINGLE counts,counts2
dim tmeout as SINGLE
dim as SINGLE xxxx,yyyy,yh,yh2,yk2,xxxx2,yyyy2,savex,savey,save2x,save2y '= 12'      // x coordinate of circle center
dim shared as SINGLE theta,meta,points,rad,yk,pieyedi '= 10'  
dim as SINGLE delta,gammy,eye=1',meta
'dim as SINGLE 'stp,'theta = 0-(3.14159265359 /20)';  // angle that will be increased each loop
dim ccccc as integer
dim paintflag AS integer
'fs=30
dim as SINGLE tmebegin,tmeend
dim cc1d as integer
tmeout=timer+1
'cccc=27
dim slphouse as SINGLE
SetMouse 0, 720,0
sleep 500
    dim tt22 as integer
dim wavefreq as long
wavefreq=26'650
dim stampflag AS integer
timerstamp=0
dim shared radian as SINGLE
color 0,0'rgb(255,8,8)
sub drawcircle
end sub
'dim shared as SINGLE xxxx0,yyyy0
dim  shared   as SINGLE yh3,yk3,yh4,yk4,xxxx1,yyyy1,xxxx0,yyyy0',yyyy2
dim  shared   as long tmpcolor1,tmpcolor3
dim  shared   as integer lx,lyy,ly
dim   shared lxy(21*8,16) as integer
dim  shared flagger as long
dim  shared longer as long
dim  shared longer2 as long
'dim jerkcount as long
'dim shared jerkflag as long
'dim shared circjerk(26*2.5) as long
 dim shared as integer xxyytxt(8*8*(1280/8),8*8),txtxxyy(21*8*6,8*6),txtxxyy2(-(14*8*6) TO (14*8*6),-(5*6) to (5*6)),renderready(-(5*8*12/2) to (5*8*12/2),-(5*12) to 5*12,1 to 24)
    dim shared AS integer ccx,ccy,ccx2,ccy2
        sub      drawdraw(ccx as ulongint,ccy as ulongint,textstring as string,colorrgb as ulongint,size as ulongint)
    dim AS integer ccx2,ccy2,pcx,pcy,ccxcount,ccycount
pcx=size
pcy=size
screenset 2,0
cls
color colorrgb,rgb(0,0,0)
print textstring
for ccxcount=1 to len(textstring)*8
    for ccycount=1 to 8
 for ccx2=1 to pcx
               for ccy2=1 to pcy
                   xxyytxt(ccxcount*pcx-pcx+ccx2,ccycount*pcy-pcy+ccy2)=point(ccxcount-1,ccycount-1)'rgb(255,255,255)
                   ' txtyy()=rgb(255,255,255)
                  ' dim as integer pcyy
                  ' for pcyy=8*pcy to 8*(pcy-1)
                  ' txtxxyy(ccx*pcx-pcx+ccx2,pcyy)=rgb(255,255,0)'colr'if ccy2=pcy then 
              ' next pcyy
               next ccy2
            next ccx2
 next
 next
screenset 1,0
for ccxcount=1 to len(textstring)*8*size
for ccycount=1 to 8*size
   if xxyytxt(ccxcount,ccycount)<>rgb(0,0,0) then  pset (ccx+ccxcount-1,ccy+ccycount-1),xxyytxt(ccxcount,ccycount)
next
next
 end sub
sub display(ccx as ulongint,ccy as ulongint,colr as ulongint,idx as SINGLE)
   
    dim AS integer ccx2,ccy2,pcx,pcy
    pcx=6'1280/136
    pcy=6'1079/8
     for ccx2=1 to pcx
               for ccy2=1 to pcy
                   txtxxyy(ccx*pcx-pcx+ccx2,ccy*pcx-pcx+ccy2)=colr'rgb(255,255,255)
                   ' txtyy()=rgb(255,255,255)
                  ' dim as integer pcyy
                  ' for pcyy=8*pcy to 8*(pcy-1)
                  ' txtxxyy(ccx*pcx-pcx+ccx2,pcyy)=rgb(255,255,0)'colr'if ccy2=pcy then 
              ' next pcyy
               next ccy2
            next ccx2
        end sub
        sub display2(ccx AS integer,ccy AS integer,colr as ulongint,idx as SINGLE)
   
    dim AS integer ccx2,ccy2,pcx,pcy
    pcx=6'1280/136
    pcy=6'1079/8
     for ccx2=1 to pcx
               for ccy2=1 to pcy
                   txtxxyy2(ccx*pcx-pcx+ccx2+1,ccy*pcx-pcx+ccy2+1)=colr'rgb(255,255,255)
                   ' txtyy()=rgb(255,255,255)
                  ' dim as integer pcyy
                  ' for pcyy=8*pcy to 8*(pcy-1)
                  ' txtxxyy(ccx*pcx-pcx+ccx2,pcyy)=rgb(255,255,0)'colr'if ccy2=pcy then 
              ' next pcyy
               next ccy2
            next ccx2
            end sub
sub display3(ccx AS integer,ccy AS integer,colr as ulongint,idx as SINGLE)
   
    dim AS integer ccx2,ccy2,pcx,pcy,ccz
    pcx=12'1280/136
    pcy=12'1079/8
     for ccx2=1 to pcx
               for ccy2=1 to pcy
                   for ccz=24 to 1 step -3'*.66
                   renderready(ccx*pcx-pcx+ccx2+1,ccy*pcx-pcx+ccy2+1,ccz)=colr'rgb(255,255,255)
                   ' txtyy()=rgb(255,255,255)
                  ' dim as integer pcyy
                  ' for pcyy=8*pcy to 8*(pcy-1)
                  ' txtxxyy(ccx*pcx-pcx+ccx2,pcyy)=rgb(255,255,0)'colr'if ccy2=pcy then 
              ' next pcyy
              next ccz
               next ccy2
            next ccx2
            end sub

dim shared flipover AS integer
dim shared as SINGLE timesz
dim AS integer tier=1

dim shared flashflip AS integer
'for jerkcount=1 to 26*3
'    if jerkflag=0 then
'        circjerk(
'function drawbox(x as SINGLE,y as SINGLE,wavemid as long) as long
'     line(x-(26*2.75),y-(26*2.75))-(x+(26*2.75),y+(26*2.75)),rgb(0,0,0),bf
       ' line(x-(26*2.75),y-(26*2.75))-(x+(26*2.75),y+(26*2.75)),wavemid,b
'end function
'dim as SINGLE savingq
'dim as SINGLE timesz,shading,linerx,linery
slphouse=3.14159265359*2
'dim shared crimes as SINGLE
'dim AS integer grimes
'grimes=255
 
dim as SINGLE shrinkx,shrinky
dim shared clifford AS integer
'dim shared as SINGLE notes(88),board(14),cmt1,cmt2,vll,cmt3
'goto sec
'board(1)=(220*2)'*(1.05946309436*1))
'board(2)=(220*2*(1.05946309436*3))
'board(3)=(220*2*(1.05946309436*4))
'board(4)=(220*2*(1.05946309436*6))
'board(5)=(220*2*(1.05946309436*8))
'board(6)=(220*2*(1.05946309436*9))
'board(7)=(220*2*(1.05946309436*11))
'board(8)=(220*2*(1.05946309436*13))
'board(9)=(220*2*(1.05946309436*15))
rem
'board(1)=(vll)'*(1.05946309436)'*(-2))
'board(2)=(board(1)*(1.05946309436))
'board(3)=(board(2)*(1.05946309436))'*(conc+1)))
'board(4)=(board(3)*(1.05946309436))'(conc+1)))
'board(5)=(board(4)*(1.05946309436))'(conc+2)))
'board(6)=(board(5)*(1.05946309436))'(conc+3)))
'board(7)=(board(6)*(1.05946309436))'(conc+4)))
'board(8)=(board(7)*(1.05946309436))'(conc+5)))
'board(9)=(board(8)*(1.05946309436))'(conc+6)))
'board(10)=(board(9)*(1.05946309436))'(conc+7)))
'board(11)=(board(10)*(1.05946309436))'(conc+8)))
'board(12)=(board(11)*(1.05946309436))'(conc+9)))
'sec:
'vll=440*2'220'392'440'783.99/2'/(1.05946309436*3)/2'174*2
'dim conc as SINGLE
'conc=0
'board(1)=(vll)'*(1.05946309436)'*(-2))
'board(2)=(board(1)*(1.05946309436))
'board(3)=(board(2)*(1.05946309436))'*(conc+1)))
'board(4)=(board(3)*(1.05946309436))'(conc+1)))
'board(5)=(board(4)*(1.05946309436))'(conc+2)))
'board(6)=(board(5)*(1.05946309436))'(conc+3)))
'board(7)=(board(6)*(1.05946309436))'(conc+4)))
'board(8)=(board(7)*(1.05946309436))'(conc+5)))
'board(9)=(board(8)*(1.05946309436))'(conc+6)))
'board(10)=(board(9)*(1.05946309436))'(conc+7)))
'board(11)=(board(10)*(1.05946309436))'(conc+8)))
'board(12)=(board(11)*(1.05946309436))'(conc+9)))
'board(13)=(board(12)*(1.05946309436))'(conc+9)))
'board(13)=(board(12)*(1.05946309436))'(conc+9)))
'dim as integer flep
'
'dim longer2 as long
'dim as SINGLE xxxx,yyyy,yh,yh2 '= 12'      // x coordinate of circle center
'dim as SINGLE theta,meta,gammy,points,ccc,rad,yk,yk2,pieyedi,xxxx2,yyyy2 '= 10'      // y coordinate of circle center
'dim eye as SINGLE=1
'dim ccccc as integer

'if three=3 then
'for ccccc=25 to 1 step-1'1 to 26  'to 1 step -1
'cow2(ccccc+1)=cow2(ccccc)'wave2rgb((399)+(waverycalc2*(ccccc*100/26/100)))
'cow2(ccccc+1)=cow2(ccccc-1)'wave2rgb((399)+(waverycalc2*(ccccc*100/26/100)))

'next ccccc
'cow2(25)=dddd(1)
'cow2(1)=dddd(1)
'cow2(24)=dddd(1)
'end if
'asm
  '  goto tesla1
'    end asm
'd=0
'n=0
'e=0

'for cccc=(720*4)/2 to (720*4)/2-3 step -1 
'circle((1280)/2,(720*4)/2),cccc,rgb(255*.529,255*.808,255*.922)
'''''''''''''''''''''''''''''''''circle((1280)/2,(720*4)/2),1022/2-12,rgb(255*.529,255*.808,255*.922)
'''''''''''''''''''''''''''''''''paint((720*4)/2,7),rgb(255*.529,255*.808,255*.922),rgb(255*.529,255*.808,255*.922)
'next cccc
'SCREENCOPY
'dim waversave as long=waver3
'waver3=backup
'dim constancex as integer
'dim constancey as integer
'goto circ
'constancex=110'100
'constancey=50-8'37-8
'dim shared shave as SINGLE
randomize timer
dim as single savingq,p
'dim as single astz,asty,astx
dim shared seconds as SINGLE
seconds=timer+1
'cccc=3
dim shared cccc2 AS integer
dim shared AS integer pressaflg
dim baxtor AS integer
dim pnt as SINGLE
dim idxcnt as integer
Dim As Integer choicestbottom,choicesttop,choicedrivea,choicedriveb
dim timbery as single
dim drawcount as SINGLE'integer

 '       'mutexlock mutex

cccc2=1
           ' sound pulsewave(notes(1)/2/2/2/2),3/(2*24)/2
'vol=vol*2
'''mutexunlock mutex
''mutexlock mutex
'        if timer>seconds-1 then 

''mutexlock mutex
  'mutexlock mutex
'vol=1/3/3.33333333*.75
'game7=game
'timered4=timered3
''mutexunlock mutex 
'''mutexunlock mutex
'if game7<1 then sound pulsewave(notes(1)*2*2*2),3/(2*24)/2

'end if

'if game7=1 or game7=12 then
   ' 'mutexlock mutex
   ' vol=1/3/3.33333333/2'*.75'/2
   ' ''mutexunlock mutex
'sound pulsewave(notes(1)*2*2*2),3/(2*24)/2
'sleep ((1000)*((timered4-timer4)*100/(60*5)/100)+(1000/4),1)
'sleep (1/1000,1)
'end if

'sound pulsewave(notes(1)/2/2/2/2),3/(2*24)/2
'
''''mutexunlock mutex
'''mutexunlock mutex
'end if

'totalcycles=counter_cycles
'dim as long flag'longer,longer2
'jmmpbump:
'goto yump
dim tie AS integer
dim stoppers as integer
dim as integer grimes=255,dimes=64
'counter_end()
'counter_begin(1,HIGH_PRIORITY_CLASS,2)
dim rainbow2 AS integer, idxflip AS integer
dim AS integer cup9top,cup9bottom
rem dim as SINGLE airbornx(numberofpoints)
rem dim as SINGLE airborny(numberofpoints)'uinteger
rem dim AS integer jetflag':dim as integer malcx
dim shared as SINGLE topzzz,bottomzzz,judgedtop,judgedbottom
dim shared as integer topgunx(255*10),topguny(255*10),bottomgunx(255*10),bottomguny(255*10),topgun(-256 to 1280+256,-256 to 720+256,-4 to 4),bottomgun(-256 to 1280+256,-256 to 720+256,-4 to 4),topfired,bottomfired,loc1xxx=(720)/2,loc1yyy=(720)/2,loc2xxx=(720)/2,loc2yyy=(720)/2
dim shared as integer topgun3(-64 to 1280+64,-64 to 720+64),bottomgun3(-64 to 1280+64,-64 to 720+64)
Dim xxx As SINGLE=3.14159265359*2
Dim yyy As SINGLE=3.14159265359*2
Dim buttons As integer
Dim result As integer
 Dim xxx2 As SINGLE
Dim yyy2 As SINGLE
Dim buttons2 As integer
Dim spin AS integer
Dim a As integer
dim a2 as integer
dim rock as integer
dim as integer game16=0
    dim astrocnt as integer
dim t8t8 as integer
dim shared as single judgedtop2,judgedbottom2
'dim 
'bbb="ilovecoreydenniswilsonsoftware"
'bbb=ucase(bbb)
'aaa=""
'for rock=1 to 500
'    aaa=aaa+bbb
'next rock
'bbb=""
'bbb=aaa
'ccc=aaa
'ddd=aaa
dim AS integer bulletflag1,bulletflag2,topcanfire,bottomcanfire
dim shared as integer bottomxxx=(1280)/2,bottomyyy=720-(720),topxxx=(1280)/2,topyyy=720/4,tempgunx,tempguny
'dim bottomxxx as integer
'dim AS integer  JoystickID 
'This line checks to see if the joystick is ok.
dim game2 AS integer
dim AS integer cleftx,clefty
dim game8 AS integer
'dim as single zzzz
dim as integer lockon
'dim as integer lockon,starcounter
    dim shared as SINGLE flatz,newz,zzzz


dim as integer starflag,rndx,rndy
DIM AS SINGLE BOTTOMTIME,TOPTIME
 dim shared as SINGLE pointidx,flatx,flaty,safe2x,safe2y,flat1,flat2,flat1x,flat1y,flat1z,flat2x,flat2y,flat2z,heavenx,heaveny
 dim as integer DRAWCOUNTX,shadesrouter
dim as SINGLE dividision
dim pee as SINGLE=pieyedi/2
dim as SINGLE hammerx,hammery

'goto jumper
dim as integer rndxxx,rndyyy,ran
dim as SINGLE timey
      dim as single timer7=int(timer+8)

dim as Long rc,bc,gc,waver,waver2,waver3,violet=wave2rgb(400)
 dim rainbow as ulongint=violet
dim shared as SINGLE casual,casual2,casual3,casual4
        dim shared as SINGLE newx,newy
         dim as SINGLE alphy, alphy2,alphyx,alphyy,omega
         dim pic as any ptr
pic=imagecreate(21*8*6,8*6)
dim as SINGLE prcy1,prcx1
dim as integer xtmp,xtmp2
dim as string hours
dim as string hrs

 dim as SINGLE apx1,apy1
dim as SINGLE alpx,alpy,alpz

dim as long straightx,straighty,greatest,bigger
dim as long straight
dim as SINGLE report=255
dim as long backup,backup2,waver4
dim as integer rannycount,ranny,smoothtopxxx=0,smoothtopxxx2,smoothtopyyy=0,smoothtopyyy2,smoothbottomxxx=0,smoothbottomxxx2,smoothbottomyyy=0,smoothbottomyyy2
dim as SINGLE gonersx=720/4,gonersy=720/4,gonersflag
dim as any ptr skyblue=imagecreate(1280-1,(720)/2)
dim as integer skyx,skyy',xint,yint
dim AS integer skyflag
flat1=0'pieyedi*2'0
#ifndef __MIRROR_BI__
#define __MIRROR_BI__
' mirror an image or the screen (img=0)
' horizontal or vertical of course you can combine different calls
' e.g. step 1 vertical step 2 horizontal, step 3 vertical ...

' I'm using only GET and PUT so you can use XOR,PSET,ALPHA ...

#endif ' __MIRROR_BI__
dim shared as SINGLE singletopz,singlebottomz,topz,bottomz
dim ran2 as SINGLE
dim ranx as SINGLE
dim rany as SINGLE
dim ranz AS integer
dim shared as SINGLE singletopx,singletopy,singlebottomx,singlebottomy,singletop,singlebottom
dim yfive as SINGLE 
dim as SINGLE q,zoo,savingx,savingy,savingp
dim as integer radbefore,radafter
dim as integer liner
dim as SINGLE modnar,timing
dim as SINGLE timered,timered2,timered5
 dim gameonflag AS integer
dim as SINGLE ballx,bally,balldirectionx,balldirectiony,headstales,ballspeed
 dim greeny as integer
dim as any ptr greenplay=imagecreate(1280,720)
dim AS integer greenflag=0
dim shared as SINGLE ship1rotate,ship2rotate,ship1rotatex,ship2rotatex,ship1rotatey,ship2rotatey,newx2,newy2,flatz2=1,flatx2,flaty2
dim as SINGLE tx,ty,bx,by
dim shipsflag AS integer=0
dim as SINGLE topgn3,bottomgn3
dim as single bottomgunz,bottomgunz2,bottomgunz3,bottomgunz4,topgunz,topgunz2,savegunx,saveguny,savegunx2,saveguny2
'screenlock
dim as integer tempgunz
dim game3 as integer
dim as SINGLE slphouse2
dim shared pointidx2 as integer
dim shared pointidx3 as integer
 #include "Windows.bi"
#Include Once "crt/string.bi"
#include "win/mmsystem.bi"
#include once "fmod.bi"
dim shared as single toytop=0,toybottom=0
Dim Shared As Single topzzz3,bottomzzz3
Dim Shared As single blowtoprx,blowtopry,BLOWBOTTOMX,BLOWBOTTOMY,BLOWTOPX,BLOWTOPY,blowbottomrx,blowbottomry,blowtoplx,blowtoply,blowbottomlx,blowbottomly

Dim Shared As single tripper96a,tripper96b,tripper96c
Dim Shared As Integer CHOICEBOTTOM,CHOICETOP,ACTIVEDa,activedb
sub drawship
'asm
'dim as integer pointa,pointb
'pointa=int(rnd*(pixelagent/2))+1
'pointb=int(pixelagent/2)+int(rnd*(pixelagent/2))+1

If JUDGEDBOTTOM<Timer Then
	ACTIVEDa=0
	BLOWBOTTOMX=0
	BLOWBOTTOMY=0
	
End if	
If JUDGEDTOP<Timer Then
	activedb=0
	blowtopx=0
	blowtopy=0
'EndIf
	
'	BLOWTOPX=0
'	BLOWTOPY=0
EndIf
If JUDGEDBOTTOM>Timer Then
If ACTIVEDa=0 Then
	ACTIVEDa=1
'EndIf 
 choicebottom=Int(Rnd*2)
 ' choicetop=Int(Rnd*4)+1
End If
End If
If JUDGEDtop>Timer Then
If ACTIVEDb=0 Then
	ACTIVEDb=1
'EndIf 
 choicetop=Int(Rnd*2)
 ' choicetop=Int(Rnd*4)+1
End If
End IF

'  choicestbottom=1
If judgedbottom>timer Then
	'Dim As Integer choicestbottom,choicesttop
'EndIf
   ' rainbow=rgb(255*casual2,255*casual2,0)
 'choicestbottom=Int(Rnd*4)+1
 ' choicesttop=Int(Rnd*4)+1
'  choicestbottom=1
'  choicesttop=3
 If choicebottom<2 Then
   BLOWBOTTOMX=BLOWBOTTOMX+(BLOWBOTTOMX+(activeda)+BLOWBOTTOMX*(bottomzzz*5))
   
   blowbottomy=BLOWBOTTOMY+(BLOWBOTTOMY+(activeda)+BLOWBOTTOMY*(bottomzzz*5))
 end If
 If choicebottom>=2 Then
   BLOWBOTTOMX=BLOWBOTTOMX-(BLOWBOTTOMX+(activeda)+BLOWBOTTOMX*(bottomzzz*5))
   
   BLOWBOTTOMY=BLOWBOTTOMY+(BLOWBOTTOMY+(activeda)+BLOWBOTTOMY*(bottomzzz*5))
 end If
 'If choicebottom=3 Then
 '  BLOWBOTTOMX=BLOWBOTTOMX-(BLOWBOTTOMX+(activeda/3)+BLOWBOTTOMX*(bottomzzz*5))
   
 '  BLOWBOTTOMY=BLOWBOTTOMY+(BLOWBOTTOMY+(activeda/3)+BLOWBOTTOMY*(bottomzzz*5))
 'end If
 'If choicebottom>3 Then
 '  BLOWBOTTOMX=BLOWBOTTOMX+(BLOWBOTTOMX+(activeda/3)+BLOWBOTTOMX*(bottomzzz*5))
   
 '  BLOWBOTTOMY=BLOWBOTTOMY-(BLOWBOTTOMY+(activeda/3)+BLOWBOTTOMY*(bottomzzz*5))
 'end If
End If
if judgedtop>timer then
     '   rainbow=rgb(255*casual,255*casual,0)
  '  Dim As Integer choicestbottom,choicesttop
'EndIf
  '  rainbow=rgb(255*casual2,255*casual2,0)
 'choicestbottom=Int(Rnd*4)+1
 ' choicesttop=Int(Rnd*4)+1
' choicestbottom=1
'  choicesttop=3
     If choicetop<2 Then
        blowtopX=blowtopX+(blowtopX+(activedb)+blowtopX*(topzzz*5))
   blowtopy=blowtopy-(blowtopy+(activedb)+blowtopy*(topzzz*5))
     End If
      If choicEtop=2 Then
        blowtopX=blowtopX-(blowtopX+(activedb)+blowtopX*(topzzz*5))
   blowtopry=blowtopry-(blowtopry+(activedb)+blowtopy*(topzzz*5))
      End If
  '     If choicetop=3 Then
  '      blowtopX=blowtopX+(blowtopX+(activedb/3)+blowtopX*(topzzz*5))
  ' blowtopy=blowtopy+(blowtopy+(activedb/3)+blowtopy*(topzzz*5))
  '     End If
   '     If choicetop>3 Then
   '     blowtopX=blowtopX-(blowtopX+(activedb/3)+blowtopX*(topzzz*5))
   'blowtopy=blowtopy-(blowtopy+(activedb/3)+blowtopy*(topzzz*5))
   '     End If
End If
If judgedbottom>Timer Then 
activeda=activeda+7
EndIf
If judgedtop>Timer Then 
activedb=activedb+7
EndIf

'	choicedrive=0
'tripper96c=0
'actived=0
'End If

'    pushf
'    end asm
    for pointidx3=1 to 2'pointidx2+
    		'if pointidx2=1 then
    ' if (pointidx2=1 and ship1rotatex<=0) or (pointidx2=2 and ship2rotatex>=0) then 

    FOR pointIdx = 1 TO pixelagent step+int(rnd*32)+1
'	if pointidx>numberofpixels then pointidx=point.idx-numberofpixels
    if ship(pointidx).c<>rgb(0,0,0)  then
   
dim as SINGLE flatz,newz,pimpz,shadea,shadeb,shade1,shade2
		if pointidx3=1 then
                   ' topzzz3=topzzz*4
topzzz3=topzzz
      '  if topzzz<0 then topzzz3=(topzzz-(topzzz*2))
               ' if topzzz>0 then topzzz3=(topzzz-(topzzz*2))

'ship(pointidx).z=1
'topzzz3=topzzz3*2
'if topzzz3<.25/2 then topzzz3=.25/2
'if topzzz3>.25 then topzzz3=.25
'if topzzz3<.5 then topzzz3=topzzz3*2  
'topzzz3=topzzz3/2
flatx=ship(pointidx).x*((64*.55)*((topzzz3*5)))/(ship(pointidx).z+zoomlevel)
'shadea=ship1rotatex*100/(pieyedi/2)/100
'IF SHIP1ROTATEX>=0 THEN 
    SHADE1=1.5+ship(POINTIDX).X*COS(+SHIP1ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEX<0 THEN
'     SHADE1=1.5+ship(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'     END IF
   
':shade1=shade1-(shade1*2)
'shade1=.5-SHADEA*4
flaty=ship(pointidx).y*((64*.55)*(topzzz3*5))/(ship(pointidx).z+zoomlevel)
'IF SHIP1ROTATEY>0 THEN 
    SHADEA=(SHIP(POINTIDX).X*sin(SHIP1ROTATEY-(+pieyedi/2)))+shade1
'ELSEIF SHIP1ROTATEY<0 THEN
'     SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'     END IF34
flatz=ship(pointidx).Z*((128*8)*.75)*(topzzz3*5)
If judgedtop<Timer then
newx = flatx'newx'
newy = (sin(ship1rotatey) * flatz) + (cos(ship1rotatey) * flaty)
newz = (cos(ship1rotatey) * flatz) - (sin(ship1rotatey) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship1rotatex) * flatx) - (sin(ship1rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship1rotatex) * flatx) + (cos(ship1rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
' NEWX2=NEWX
' NEWY2=NEWY
		End If
if judgedtop>Timer then 
 newx = flatx'newx'
newy = (sin(flat1) * flatz) + (cos(flat1) * flaty)
newz = (cos(flat1) * flatz) - (sin(flat1) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
newy=flaty''y = y'
newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) + (sin(flat2) * flaty)
newy = (cos(flat2) * flaty) - (sin(flat2) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
' NEWX2=NEWX
' NEWY2=NEWY
end if













'/((720*2+(720/2)))'/ship(pointidx).Z)''*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).z+zoomlevel)'((3)
Else
bottomzzz3=bottomzzz
           '' bottomzzz=bottomzzz*4[-bottomzzz3=bottomzz
           ' if bottomzzz<0 then bottomzzz=1-(bottomzzz-(bottomzzz*2))
'if bottomzzz<0 then bottomzzz3=(bottomzzz-(bottomzzz*2))
'bottomzzz3=bottomzzz3*2
'if bottomzzz3<.25/2 then bottomzzz3=.25/2
'if bottomzzz3>.25 then bottomzzz3=.25'-.1
flatx=ship(pointidx).x*((64*.55)*((bottomzzz3*5)))/(ship(pointidx).z+zoomlevel)
'IF SHIP2ROTATEX>=0 THEN 
    SHADE2=1.5+ship(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEX<=0 THEN    
'    SHADE2=1.5+ship(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'     END IF
flaty=ship(pointidx).y*((64*.55)*(bottomzzz3*5))/(ship(pointidx).z+zoomlevel)
'IF SHIP2ROTATEY>0 THEN 
    SHADEB=(SHIP(POINTIDX).X*sin(SHIP2ROTATEY-(pieyedi/2)))+shade2
'ELSEIF SHIP2ROTATEY<0 THEN
'     SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'     END IF
flatz=ship(pointidx).Z*((128*8)*.75)*(bottomzzz3*5)
If judgedbottom<Timer Then
 newx = flatx'newx'
newy = (sin(ship2rotatey+pieyedi) * flatz) + (cos(ship2rotatey+pieyedi) * flaty)
newz = (cos(ship2rotatey+pieyedi) * flatz) - (sin(ship2rotatey+pieyedi) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship2rotatex) * flatx) - (sin(ship2rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship2rotatex) * flatx) + (cos(ship2rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy
End If
if judgedbottom>Timer then 
 newx = flatx'newx'
newy = (sin(flat2) * flatz) + (cos(flat2) * flaty)
newz = (cos(flat2) * flatz) - (sin(flat2) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) - (sin(flat2) * flatz)
newy=flaty''y = y'
newz = (sin(flat2) * flatx) + (cos(flat2) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
 newx2=newx
 newy2=newy
end if







'/((720*2+(720/2)))'/ship(pointidx).Z)''*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).z+zoomlevel)'((3)
end if
'flatx=newx
' flaty=newy
' flatz=newz
 'newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz





'flatx2=flatx
'flaty2=flaty
'flatz2=flatz'*((1280)/2)
'newz = (cos(ship1rotatex) * flatz) - (sin(ship1rotatex) * flaty)
 'flatx=newx
 'flaty=newy
 'flatz=newz
 'newy=flaty
 'rem xyrotatecode
' alphyx=ship(pointidx).x*cos(ship1rotatex-pieyedi/4)
'  alphyy=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
'  if singletopy<1 and singletopy>0 then singletopy=0
'  if singlebottomy<1 and singlebottomy>0 then singlebottomy=0

' x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')

'The second set of algorithms is for rotation about the y axis:

'x = (cos(angle) * x') - (sin(angle) * z')
'y = y'
'z = (sin(angle) * x') + (cos(angle) * z')

 'newx=flatx
 'newy2=newy
 'newx2=newx
' newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz
 
' newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
'newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
'newz = flatz'
' flatx=newx
' flaty=newy
' flatz=newz
 
' alphyx=ship(pointidx).x*cos(ship1rotatex)
'  alphyx=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' dim as SINGLE aly1,aly2
 ' aly1=ship(pointidx).x*cos(ship2rotatex)
   ' aly1=ship(pointidx).y*cos(ship2rotatey-pieyedi/4)
'casual=alphyx'y*(100/1/100)
'casual2=aly1'2*(100/1/100)
'casual=casual*4
'casual2=casual2*4
'if casual>1 then casual=1
'if casual<0 then casual=0
'if casual2>1 then casual2=1
'if casual2<0 then casual2=0

casual=1*(topzzz*.33*10)'casual+5
casual2=1*(bottomzzz*.33*10)'=casual2+5
if casual<.5 then casual=.5
if casual2<.5 then casual2=.5
casual=casual+shadea'*SHADEA
casual2=casual2+shadeb'*SHADEB

'casual=1'-((1)*flatz*100/((1280)/2)/100)
'dim x as SINGLE
 
 'rernd2:
' x=rnd'(timered)
 'if x=0 then goto rernd2
'if x<.6 then casual=casual-.08'03333333
' if x>.5 then casual=casual+.08'03333333
'if (pieyedi*2/20)+slphouse'1*(pieyedi*2/20)-(slphouse-pieyedi/2)*100/(pieyedi*2/20-pieyedi/2)/100'(100/2/100)
          if casual>1 then casual=1
          if casual<0 then casual=0
if casual2>1 then casual2=1
if casual2<0 then casual2=0
dim as ulongint rainbow
dim as integer d,n,e
    rainbow=ship(pointidx).c'rgb(255,255,251)
   
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )

rainbow=rgb(d*casual,n*casual,e*casual)',127)
if pointidx3=1 and judgedtop>timer then
    rainbow=rgb(255*casual,255*casual,0)
    end if
if pointidx3=2 and judgedbottom>timer then
        rainbow=rgb(255*casual2,255*casual2,0)
end if
if pointidx3=1 Then
	 if judgedtop<timer then rainbow=rgb(d*casual,n*casual,e*casual)
'EndIf
 screenlock
   circle (newX+BLOWTOPX+(topxxx), newy+BLOWTOPY+(topyyy)-(33+9)),3.33*(topzzz*5),rainbow,,,,F
   screenunlock
ElseIf pointidx3=2 then
   
   If judgedbottom<timer then rainbow=rgb(d*casual2,n*casual2,e*casual2)',127)
screenlock
circle(newx2+BLOWBOTTOMX+(bottomxxx),newy2+BLOWBOTTOMY+(bottomyyy)+(33+9)),3.33*(bottomzzz*5),rainbow,,,,F
screenunlock
end if
end if
NEXT
next
'asm
 '   popf
   ' ret
end sub'asm
   DIM RANDIT AS integer
  dim randit2 as integer
  dim as integer timber3

     Const SOUND_FILE = "nature.wav"'crowdnoise.aif"'nature.mp3"
          Const SOUND_FILE2 = "crowdah.wav"'nature.mp3"
          Const SOUND_FILE3 = "crowdclaps.wav"'nature.mp3"
const sound_file4="crowdcheering.wav"

Const SOUND_FILE5 = "blasterfire.wav"'crowdnoise.aif"'nature.mp3"
'Const SOUND_FILE5 = "marklazer.wav"'crowdnoise.aif"'nature.mp3"


Const SOUND_FILE6 = "blasterincoming.wav"'nature.mp3"
          const sound_file7 = "goalalarm.wav"
          const sound_file8 = "ballhit.wav"
          const sound_file9 = "flyby1.wav"
          const sound_file10 = "flyby4.wav"
                    const sound_file11 = "flyby3.wav"
const sound_file12 = "mysteryclock.wav"
Const sound_file13 = "marklazer.wav"
dim as integer shots,shots1,shots2
DIM AS integer  pongsong1,pongsong2,pongsong3,pongsong4,pongsong5,pongsong6,PONGSONG7,PONGSONG8,starsong1,starsong2,starsong3,starsong4,starsong5,starsong6,starsong7,starsong8
Dim As Integer astroh(8)

dim as integer helpflag
'fmusic_playsong(starsong3)
dim vol255 as integer
vol255=255
DIM AS integer TEMPYGUN,TEMPXGUN,TEXAN=0,TEYAN=3,DECIDE
dim as integer savecapture=0,checksum24=0
dim as integer choice=1
Dim As Integer mercy=1
Dim As Integer fullscreentoggle=1
 dim as integer choice2,ankle=0
   dim as string k2
   dim as longint pingpongtop,pingpongbottom
   
   DIM TRAINERS AS Integer

     If (FSOUND_Init(48000, 128, 0) = 0) Then
            Print "Could not initialize FMOD"
            End 1
        End If
    
        FSOUND_Stream_SetBufferSize(150)
        screenset 0,0:color rgb(255,255,0),rgb(0,0,0)
        print"Testing loading music and sound....."
        print
        print"Loading game music........"
        color rgb(255,127,0),rgb(0,0,0)
if pongsong1=0 then pongSong1 = FMUSIC_LoadSong("\pingpongtennissong\febsymp.mod")
if pongsong1<>0 then print"1. febsymp.mod loaded":checksum24=checksum24+1
'fmusic_playsong(pongsong1)

'elseif randit=2 then
if pongsong2=0 then pongSong2 = FMUSIC_LoadSong("\pingpongtennissong\easy-afternoon.mod")
if pongsong2<>0 then print"2. easy-afternoon.mod loaded":checksum24=checksum24+1
'fmusic_playsong(pongsong2)

'elseif randit=1 then
if pongsong3=0 then   pongSong3 = FMUSIC_LoadSong("\pingpongtennissong\mourning.mod")
if pongsong3<>0 then print"3. mourning.mod loaded":checksum24=checksum24+1

if pongsong4=0 then pongSong4 = FMUSIC_LoadSong("\pingpongtennissong\insideme.mod")
if pongsong4<>0 then print"4. insideme.mod loaded":checksum24=checksum24+1
'fmusic_playsong(pongsong1)

'elseif randit=2 then
if pongsong5=0 then pongSong5 = FMUSIC_LoadSong("\pingpongtennissong\video_game_party.xm")
if pongsong5<>0 then print"5. video_game_party.xm loaded":checksum24=checksum24+1
'fmusic_playsong(pongsong2)

'elseif randit=1 then
if pongsong6=0 then   pongSong6 = FMUSIC_LoadSong("\pingpongtennissong\lunatic.mod")
if pongsong6<>0 then print"6. lunatic.mod loaded":checksum24=checksum24+1
if pongsong7=0 then pongSong7 = FMUSIC_LoadSong("\pingpongtennissong\EASY_FUNK.MOD")
if pongsong7<>0 then print"7. easy_funk.mod loaded":checksum24=checksum24+1
'fmusic_playsong(pongsong2)

'elseif randit=1 then
if pongsong8=0 then   pongSong8 = FMUSIC_LoadSong("\pingpongtennissong\easy_words.mod")
if pongsong8<>0 then print"8. easy_words.mod loaded":checksum24=checksum24+1

'fmusic_playsong(pongsong3)
if starsong1=0 then starSong1 = FMUSIC_LoadSong("\starshipshooterssong\gslinger.mod")
if starsong1<>0 then print"9. gslinger.mod loaded":checksum24=checksum24+1
'fmusic_playsong(starsong1)

'elseif randit=2 then
if starsong2=0 then starSong2 = FMUSIC_LoadSong("\starshipshooterssong\deadlock.xm")
if starsong2<>0 then print"10. deadlock.xm loaded":checksum24=checksum24+1
'fmusic_playsong(starsong2)

'elseif randit=1 then
if starsong3=0 then   starSong3 = FMUSIC_LoadSong("\starshipshooterssong\space_debris.mod")

if starsong3<>0 then print"11. space_debris.mod loaded":checksum24=checksum24+1


if starsong4=0 then starSong4 = FMUSIC_LoadSong("\starshipshooterssong\hymn_to_aurora.mod")
if starsong4<>0 then print"12. hymn_to_aurora.mod loaded":checksum24=checksum24+1
'fmusic_playsong(starsong1)

'elseif randit=2 then
if starsong5=0 then starSong5 = FMUSIC_LoadSong("\starshipshooterssong\strobo_-_star_wars.mod")
if starsong5<>0 then print"13. strobo_-_star_wars.mod loaded":checksum24=checksum24+1
'fmusic_playsong(starsong2)

'elseif randit=1 then
if starsong6=0 then   starSong6 = FMUSIC_LoadSong("\starshipshooterssong\star_wars_endtheme.mod")

if starsong6<>0 then print"14. star_wars_endtheme.mod loaded":checksum24=checksum24+1

if starsong7=0 then starSong7 = FMUSIC_LoadSong("\starshipshooterssong\a94final.s3m")
if starsong7<>0 then print"15. a94final.s3m loaded":checksum24=checksum24+1
'fmusic_playsong(starsong2)

'elseif randit=1 then
if starsong8=0 then   starSong8 = FMUSIC_LoadSong("\starshipshooterssong\dreams.xm")

if starsong8<>0 then print"16. dreams.xm loaded":checksum24=checksum24+1


print
print
print"Loading game sound effects........"


    Dim As FSOUND_STREAM Ptr stream = FSOUND_Stream_Open(SOUND_FILE, 0, 0, 0)
    if stream<>0 then print"1. loaded nature.mp3":checksum24=checksum24+1
   ' FSOUND_Stream_Play(FSOUND_FREE, stream)
   ' sleep 1000,1
   Dim As FSOUND_STREAM Ptr stream2 = FSOUND_Stream_Open(SOUND_FILE2, 0, 0, 0)
    if stream2<>0 then print"2. loaded crowdah.wav":checksum24=checksum24+1
   ' FSOUND_Stream_Play(FSOUND_FREE, stream)
   ' sleep 1000,1
   Dim As FSOUND_STREAM Ptr stream3 = FSOUND_Stream_Open(SOUND_FILE3, 0, 0, 0)
    if stream3<>0 then print"3. loaded crowdclaps.wav":checksum24=checksum24+1
    Dim As FSOUND_STREAM Ptr stream4 = FSOUND_Stream_Open(SOUND_FILE4, 0, 0, 0)
    if stream4<>0 then print"4. loaded crowdcheering.wav":checksum24=checksum24+1
    Dim As FSOUND_STREAM Ptr stream5 = FSOUND_Stream_Open(SOUND_FILE5, 0, 0, 0)
        if stream5<>0 then print"5. loaded blasterfire.wav":checksum24=checksum24+1

    Dim As FSOUND_STREAM Ptr stream6 = FSOUND_Stream_Open(SOUND_FILE6, 0, 0, 0)
        if stream6<>0 then print"6. loaded blasterincoming.wav":checksum24=checksum24+1

        Dim As FSOUND_STREAM Ptr stream7 = FSOUND_Stream_Open(SOUND_FILE7, 0, 0, 0)
            if stream7<>0 then print"7. loaded goalalarm.wav":checksum24=checksum24+1
Dim As FSOUND_STREAM Ptr stream8 = FSOUND_Stream_Open(SOUND_FILE8, 0, 0, 0)
            if stream8<>0 then print"8. loaded ballhit.wav":checksum24=checksum24+1
Dim As FSOUND_STREAM Ptr stream9 = FSOUND_Stream_Open(SOUND_FILE9, 0, 0, 0)
            if stream9<>0 then print"9. loaded flyby1.wav":checksum24=checksum24+1
Dim As FSOUND_STREAM Ptr stream10 = FSOUND_Stream_Open(SOUND_FILE10, 0, 0, 0)
            if stream10<>0 then print"10. loaded flyby4.wav":checksum24=checksum24+1
Dim As FSOUND_STREAM Ptr stream11 = FSOUND_Stream_Open(SOUND_FILE11, 0, 0, 0)
            if stream11<>0 then print"11. loaded flyby3.wav":checksum24=checksum24+1
Dim As FSOUND_STREAM Ptr stream12 = FSOUND_Stream_Open(SOUND_FILE12, 0, 0, 0)
            if stream12<>0 then print"11. loaded mysteryclock.wav":checksum24=checksum24+1
Dim As FSOUND_STREAM Ptr stream13 = FSOUND_Stream_Open(SOUND_FILE13, 0, 0, 0)
            if stream13<>0 then print"11. loaded marklazer.wav":checksum24=checksum24+1



print
print
print
if checksum24=29 then
color rgb(255,255,0),rgb(0,0,0)
print"Checksum perfect, software running normal........"
    sleep (3000,1)
else
    color rgb(255,0,0),rgb(255,255,255)
  print"Error in checksum, press any key to abort software......."
  sleep
  end
end if
dim shared as integer hondax,honday,hondar,hondal

         dim shared as SINGLE zcount
dim AS integer readyflg,pressed
    'sub readyrender
   '  dim as SINGLE pressax,pressay,PRESSAZ,pressaz2
     'dim as integer ccx,ccy
    ' do
  dim as integer astx(8),asty(8),t7t
dim as single astz(8)  
'end sub
    '   END IF
    ' END IF
    #include "file.bi"
    dim as string starname(10),pongname(10)
    dim AS integer starscore(10),pongscore(10)
    dim as integer timeflag=0 
  dim as integer tiesaucer
  dim as integer fps
dim muting as integer

    if fileexists("scores.dat") then
'do
    open "scores.dat" for input as #1
    dim score as integer
    for score=1 to 10
        line input #1,starname(score)
        input #1,starscore(score)
    next score
    for score=1 to 10
        line input #1,pongname(score)
        input #1,pongscore(score)
    next score
        input #1,timeflag
input #1,tiesaucer
input #1,vol255
input #1,muting
input #1,fps
Input #1,fullscreentoggle
    close #1
    end if
   ' if fileexists
dim shared AS longint counter_cycles
dim shared as longint _process_priority_class_, _thread_priority_
dim shared AS longint total_cycles
'Dim Shared As single blowtoprx,blowtopry,BLOWBOTTOM,BLOWTOP,BLOWACTIVE,blowbottomrx,blowbottomry,blowtoplx,blowtoply,blowbottomlx,blowbottomly
'dim shared totalcycles AS longint
dim as any ptr picture,picture2
dim as integer y3
screenset 2,0
cls
dim as integer astrocount=0

  picture = ImageCreate( 1280, 720/2, RGB(0, 0,0) )
 '   picture2 = ImageCreate( 1279, 719, RGB(0, 0,0) ) 

   for y3=1 to 720/2
        line(1,y3)-(1279,y3),rgb(0,255*(y3*100/(720/2)/100),255*(y3*100/(720/2)/100))',255*(y3*100/(720/2)/100))
    next
        get (1,1)-(1279,720/2),picture

'    for y3=720/2 to 1 step -1
'        line(1,720-y3)-(1279,720-y3),rgb(215*(720-y3*100/(720/2)/100),59*(720-y3*100/(720/2)/100),62*(720-y3*100/(720/2)/100))',255*(y3*100/(720/2)/100))
'    next
'        get (1,1)-(1279,719),picture2

cls
for astrocount=1 to 8
    astroz(astrocount)=10
'astrox(astrocount)=int(1280*rnd)
'astroy(astrocount)=int(720*rnd)

next
astrocount=0
screenset 1,0'rgb(215/15,59/15,62/15)
dim shared as longint _loop_count_, _loop_counter_

#macro COUNTER_BEGIN( loop_count, process_priority, thread_priority )
_loop_count_ = loop_count
_process_priority_class_ = GetPriorityClass(GetCurrentProcess())
_thread_priority_ = GetThreadPriority(GetCurrentThread())
SetPriorityClass(GetCurrentProcess(), process_priority)
SetThreadPriority(GetCurrentThread(), thread_priority)
_loop_counter_ = _loop_count_
asm
xor eax, eax
cpuid
rdtsc
push edx
push eax
xor eax, eax
cpuid
.balign 16
0:
sub DWORD PTR _loop_counter_, 1
jnz 0b
xor eax, eax
cpuid
rdtsc
pop ecx
sub eax, ecx
pop ecx
sbb edx, ecx
push edx
push eax
xor eax, eax
cpuid
rdtsc
push edx
push eax
mov eax, _loop_count_
mov _loop_counter_, eax
xor eax, eax
cpuid
.balign 16
1:
end asm
#endmacro

#macro COUNTER_END()
asm
sub DWORD PTR _loop_counter_, 1
jnz 1b
xor eax, eax
cpuid
rdtsc
pop ecx
sub eax, ecx
pop ecx
sbb edx, ecx
pop ecx
sub eax, ecx
pop ecx
sbb edx, ecx
mov DWORD PTR [counter_cycles], eax
mov DWORD PTR [counter_cycles+4], edx
end asm
SetPriorityClass(GetCurrentProcess(),_process_priority_class_)
SetThreadPriority(GetCurrentThread(),_thread_priority_)
counter_cycles /= _loop_count_
#endmacro
'Declare Function Sleep Overload ( ByVal amount As Integer = -1 ) As Integer
'Dim As Integer astrocnt2=0
COUNTER_CYCLES=0
total_cycles=0
counter_begin(1,realtime_PRIORITY_CLASS,THREAD_PRIORITY_IDLE)
sleep (1000)
  counter_end()

  total_cycles=counter_cycles
'dim as integer fps
dim score as integer
  dim as integer topr,bottomr
      dim as longint cup9toppuff,cup9bottompuff
 dim AS integer joy1=0,joy2=0
dim as integer livesbottom,livestop,energybottom,energytop
 dim as single flatclock
 dim tiding as integer
 
 sub blowup
'asm
 '   pushf
 	'RANDOMIZE RND+TIMER+Rnd
 dim fx as integer
 ' 
 'end asm
'if bottomgunz>0 and bottomgunz2>0 then
for hondal=1 to int(rnd*25)+1*(25/7)

                      hondax=rnd*64
                       honday=rnd*64
                       screenlock
                       circle(TEMPGUNX+32-hondax,TEMPGUNY+32+32-honday),rnd*256*(TOPGUN3(TEMPGUNX,TEMPGUNY)/12),rgb(255,(255*rnd),0),,,,F
                               screenunlock
'judgedbottom=timer+1

                   screencopy
                   flip
                   sleep 0,1
next hondal
   ' screenlock
   ' paint(1,1),rgb(0,255,0)
   'for fx=0 to 64 step +1
  ' if tempguny>(720*4)/2 then
  '   line(tempgunx-64,tempguny-1)-(tempgunx+64,tempguny-1),rgb(255,192,0)
   'circle(tempgunx,tempguny),92,rgb(255*rnd,255*rnd,0),,,,F
   '   line(tempgunx-64,tempguny+1)-(tempgunx+64,tempguny+1),rgb(255,31,0)
'next 
'    for fx=128 to 0 step -2
  ' if tempguny>(720*4)/2 then
  '   line(tempgunx-64,tempguny-1)-(tempgunx+64,tempguny-1),rgb(255,192,0)
'   circle(tempgunx,tempguny),fx,rgb(255*rnd,31*rnd,0),,,,F
   '   line(tempgunx-64,tempguny+1)-(tempgunx+64,tempguny+1),rgb(255,31,0)
'next 
   
   '   circle(tempgunx,tempguny),64,rgb(255,0,0),,,,F
   'circle(tempgunx,tempguny),32,rgb(255,255,251),,,,F

    'screenunlock
    'screencopy
    dim as integer tempgunz,topgunz,topgunz2
    for topgunz=1 to 255 step +1'int(rnd*2)+1
        for topgunz2=1 to 255 step +1'int(rnd*2)+1'128'64'32
          '  for tempgunz=-4 to 4
 tempgunz=1
' if tempgunx+128<1280 and tempgunx-128>0 and tempguny-256>0 and tempguny+256<720 then
     topgun(tempgunx-127+topgunz,tempguny+127-topgunz2,1)=0
        bottomgun(tempgunx-127+topgunz,tempguny-127+topgunz2,1)=0
         topgun3(tempgunx-127+topgunz,tempguny+127-topgunz2)=0
        bottomgun3(tempgunx-127+topgunz,tempguny-127+topgunz2)=0
        
        'bottomgunz=0
       ' screenlock
       ' circle(tempgunx,tempguny),topgunz,rgb(255,63,15),,,,F
       ' screenunlock
        'flip
'    end if
       ' sleep (1000/60,1)
next topgunz2
next topgunz
   ' bottomgunz=0
   'bottomgunz2=0
    'screencopy
   'else
   '     bottomgunz=0
   '     bottomgunz2=0
'end if
'asm
'    popf
    return
   ' end asm
end sub
tt5=0
astrocnt=0
mercy=Int(Rnd*3)+1
If mercy>2 Then 
mercy=2
Else
	mercy=1
EndIf

do
    COUNTER_CYCLES=0
        counter_begin(1,realtime_PRIORITY_CLASS,THREAD_PRIORITY_IDLE)
'        If mercy>
If astrocnt2>2 Then astrocnt2=0
if int(timer)>TT3 then
tt3=INT(timer)
tt4=tt5
'locate 1,1:print tt4
tt5=0
end if
 tt5=tt5+1
 astrocnt=astrocnt-1
  if astrocnt<1 then astrocnt=5   
   ' counter_begin(1,HIGH_PRIORITY_CLASS,2)
   timered=timer
'   IF TIMER>TT3 THEN TT3=INT(TIMER)+1
'timbery=timer+1
 '   'mutexlock mutex
       ' timer3=timbery
 '   timered3=timered2
 '   ''mutexunlock mutex
   
  '  counter_begin(1,HIGH_PRIORITY_CLASS,2)
  if rainglow=0 then rainglow=399
  rainglow=rainglow+1
  if rainglow>650 then rainglow=400

    'if idxcnt=0 then idxcnt=1
   ' if game2=0 then
' 'mutexlock mutex
'game=2
'''mutexunlock mutex
'   end if
    'rainbow2=wave2rgb(400+(waverycalc-idxcnt))'((waverycalc)*idxcnt*100/numberofpoints/100)))
'if timered>timered2 and game2=1 then 
''mutexlock mutex

'    game=2
'elseif game2<1 then
'''mutexunlock mutex
'game2=game
       ' timered2=timer+60*5/2

 '   end if
 'mutexlock mutex
'if game2<>game then
    
'end if
  game2=game
'elseif game2<1 then
 ''mutexunlock mutex
 if timer3=0 then timer3=int(timer)+1
' if flatclock=0 then flatclock=pieyedi
     flatclock=flatclock+((pieyedi*2/4+pieyedi)/(60*60))
   if flatclock>pieyedi*2 then flatclock=flatclock-(pieyedi*2)
    if int(timer)>timer3 and (game2=0 or game2=13) then'or ((FSOUND_Stream_GetPosition(stream) >= FSOUND_Stream_GetLength(stream)) and game2=0) Then
   '             Exit While
   '         End If
   '         Sleep 50, 1
   '     Wend
  
        FSOUND_Stream_stop(stream12)
         '      FSOUND_Stream_Close(stream3)

        ' stream=FSOUND_Stream_Open(SOUND_FILE, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
       ' FSOUND_Stream_Stop(stream)
       ' if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream)       'sleep
      ' end if
      
     if vol255=255 then   FSOUND_Stream_Play(FSOUND_FREE, stream12)       'sleep
        timer3=int(timer)+1
        end if
'color ,rainbow2'rgb(0,0,0)
'    counter_cycles=0
 '  COUNTER_BEGIN(1,0,0)
 'sleep(1,1)
 'counter_end()
   ' counter_begin(1,0,0)'HIGH_PRIORITY_CLASS,2)
  
if game2>0 then goto gameone

   ' sundiALCALC
   ' if vol=0 then vol=1/3'.25
    
           ' counter_begin(1,HIGH_PRIORITY_CLASS,2)

'screencopy
   ' if timer>second then second=timer+1
   ' if timer>timerstamp then
''mutexlock mutex
'''mutexunlock mutex
'end if
'counter_begin(1,HIGH_PRIORITY_CLASS,2)
 'screenset 0,2
 'screenset 2,0
'cccc=cccc-2
'if cccc<1 then cccc=26
'cccc=26

   
'for cccc=26 to 1 step -1
'cls
'sleep(12,1)
'color ,rgb(0,0,0)
'screencopy 2,0'cccc
'screencopy 4,2
'sleep (12,1)
'screencopy 2,0

'tt22=cc1d
'if cccc=0 then cccc=3:goto xit
'if cccc=3 then cccc=1:goto xit
'if cccc=1 then cccc=3
'xit:

 '   screenset cccc,0

'for cccc=3 to 1 step -2
   ' if cccc=2 then exit next 
       ' goto ender
       ' end if
'randomize timer

'cls
'sleep (100,1)
'screencopy
'if flipover=1 then goto skipt

'skipt:
'put(0,0),picture
'screencopy
'sleep (100,1)
'cls
'sleep
'skipt:
'screenset 2,0
'screencopy '2,0
'sleep (100,1)
'sleep
'sleep

'sleep 25,1
    'screenset cccc,0
'    screencopy
' screencopy
'flipover=0

'if flipover=0 then 
'    flipover=1
'else
'    flipover=0
'    end if



gameone:

screenset 1,0
'astrocnt=astrocnt-1
'if astrocnt<1 then astrocnt=5
if (game2<>1 and game2<>12) or ((game2=1 or game2=12)) then 
    cls
 '   end if
dim as single flatxxxx=0,flatax,flatay,flataz,flatfinal

'dim as integer col1,col2
'if flat1x<pieyedi then
'    flatxxxx=(flat1x+pieyedi)
'elseif flat1x>pieyedi then
'    flatxxxx=(pieyedi*2-flat1x)
'end if
'flatax=flatxxxx*(100/(pieyedi*2)/100)

'if flat1y<pieyedi then
'    flatxxxx=(flat1y+pieyedi)
'elseif flat1y>pieyedi then
'    flatxxxx=(pieyedi*2-flat1y)
'end if
'flatay=flatxxxx*(100/(pieyedi*2)/100)
'col1=0:col2=0
dim as integer col1,col2
col1=0:col2=0
'if flat1x<pieyedi then
    if flat1x<pieyedi/2 then
    flatxxxx=(pieyedi/2)-flat1x:col1=1
    end if
if flat1x>pieyedi/2 then
    flatxxxx=(flat1x-(pieyedi/2)):col1=2
    end if
if flat1x<pieyedi+(pieyedi/2) then
 '   if flat1x<pieyedi then
    flatxxxx=(((pieyedi+(pieyedi/2))-flat1x)):col1=2
end if
if flat1x>pieyedi+(pieyedi/2) then
   ' flatxxxx=flat1z-(pieyedi/2+pieyedi)
    flatxxxx=(flat1x-(pieyedi+(pieyedi/2))):col1=1
    end if
'end if
flatax=flatxxxx*(100/(pieyedi/2)/100)

'flatfinal=flatax*flatay*flataz
flatfinal=flatax
if flatfinal>1 then flatfinal=1
if flatfinal<0 then flatfinal=0











'if flatxxxx>1 then flatxxxx=1
'if flatxxxx<20 then flatxxxx=20
screenlock
if game2=13 or game2=12 or game2=1 and (col1=1 and astrocnt=5) then paint (1,1),rgb(215/6*flatfinal,59/6*flatfinal,62/6*flatfinal)
'if game2=13 or game2=12 or game2=1 then put(1,1),picture'paint (1,1),rgb(215/15,59/15,62/15)
'if flat1x<0 then
'    flatxxxx=(pieyedi*2-flat1x):col2=1
'elseif flat1x>0 then
'    flatxxxx=(flat1x):col2=0
'end if
'flatax=flatxxxx*(100/(pieyedi*4)/100)

'flatfinal=flatax*flatay*flataz
'flatfinal=flatax
'if flatfinal>1 then flatfinal=1
'if flatfinal<0 then flatfinal=0

if game2=13 or game2=12 or game2=1 and (col1=2 and astrocnt=5) then paint (1,1),rgb(0,0,255/10-(255/10*flatfinal))
screenunlock
'screenlock
'goto jump3d
'randomize timer
'else 
'    screenset 0,1
'    end if
'screencopy
ELSE
cls
end if
'flipover=1
'sleep
rem gym
DIM Z2 AS SINGLE=ZOOMLEVEL
if game2=0 then
   put (1,1),picture
   end if

if game2=0 then
    
    zoomlevel=-1.5'.5'6.33'9'6.5'4'3.5'4.66'10'2.5
    FOR pointIdx = 1 TO numberOfstars
	
     if (stars(pointidx).x+stars(pointidx).y+stars(pointidx).z>0) or (stars(pointidx).x+stars(pointidx).y+stars(pointidx).z<0) then
   
dim as SINGLE flatz,newz
		

flatx=stars(pointidx).x*((1280))/(stars(pointidx).z+zoomlevel)
flaty=stars(pointidx).y*((720))/(stars(pointidx).z+zoomlevel)
flatz=stars(pointidx).z*((1280)/1.5)'*sin(flatclock)
'NEWX=FLATX
'NEWY=FLATY
'newz=flatz
 'IF GAME2=80 THEN
 newx = flatx'newx'
newy = (sin(flat1) * flatz) + (cos(flat1) * flaty)
newz = (cos(flat1) * flatz) - (sin(flat1) * flaty)
' flatx=newx
' flaty=newy
' flatz=newz
' newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz
 
' newx = (cos(flatclock) * flatx) + (sin(flatclock) * flaty)
'newy = (cos(flatclock) * flaty) - (sin(flatclock) * flatx)
'newz = flatz'
' flatx=newx
' flaty=newy
' flatz=newz
' END IF
 
casual=((1)*flatz*100/((1280)/1.5)/100)
if casual>1 then casual=1
if casual<0 then casual=0
    rainbow=rgb(255,255,251)
'   rainbow=rgb(238,130,238)
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )

rainbow=rgb(d*casual,n*casual,e*casual)
 screenlock
if casual<>0 then   circle (newX+((1280)/2), newy+((720)/2)),2,rainbow,,,,F

screenunlock
end if
NEXT

end if
ZOOMLEVEL=Z2
if game2=0 then
     screenlock
  '  put(0,0),myimage,ALPHA,127-(bright*127)'pset
circle((1280)/2,720*.5),(720)/2/16,rgb(255,255,255),,,,f
'paint((1280)/2,720*.5),rgb(255,255,251),rgb(255,252,251)
screenunlock
end if
if game2=77 then
    dim as SINGLE aray1,aray2,aray3,array4
    dim as integer xaray,yaray,zaray
    dim xara as integer
    dim yara as integer
    dim y as integer
    for xaray=-4 to 1280+4
          '  for zaray=720-1 to 720/1.5/2
'     for y=720-1/3 to zaray'720/1.5/2
'RANDOMIZE RND+TIMER+Rnd 
xara=int(rnd*(1280/2))+1
yara=int(rnd*(1280/2))+1

            line((1280/2+3)-(xaray),720/2-((720/2*(xaray*100/(1280*32)/100)-xara)))-((1280/2+3)-(xaray),720/2-1),rgb(255+127*((1280)-xaray*100/(1280)/100),192*((1280)-xara*100/(1280)/100),23/3*((1280)-xaray*100/(1280)/100))
                        line((1280/2-3)+(xaray),720/2-((720/2*(xaray*100/(1280*32)/100)-xara)))-((1280/2-3)+(xaray),720/2-1),rgb(255+127*((1280)-xaray*100/(1280)/100),192*((1280)-yara*100/(1280)/100),23/3*((1280)-xaray*100/(1280)/100))
                        line((1280/2+3)-(xaray),720/2-((720/2*(xaray*100/(1280*64)/100)-yara)))-((1280/2+3)-(xaray),720/2-1),rgb(192*((1280)-xaray*100/(1280)/100),0,0)
                        line((1280/2-3)+(xaray),720/2-((720/2*(xaray*100/(1280*64)/100)-yara)))-((1280/2-3)+(xaray),720/2-1),rgb(192*((1280)-xaray*100/(1280)/100),0,0)
'line(xaray-(1280/2),(720/2)-(720/6*(xaray*100/(1280*2)/100))-(xara/3*2)-(xaray-(1280/2),720/2-1),rgb(255*(xaray*100/(1280)/100),0,0)
'                        line((1280/2)+xaray,(720/2)-(720/6*(xaray*100/(1280*2)/100))-(xara/3*2))-((1280/2)+xaray,720/2-1),rgb((255)-255*(xaray*100/(1280)/100),0,0)
       ' next 
    next 
'next
'next
end if
   ' cls
   ' screencopy
  if game2=0 then
   
'flip
 zoomlevel=9
    FOR pointIdx = 1 TO numberOfPoints
	'// Do not draw points that are behind the screen
		 IF MyArray(pointIdx).Z+zoomlevel > 0 and (myarray(pointidx).x+myarray(pointidx).y)>0 or (myarray(pointidx).x+myarray(pointidx).y)<0 THEN

    '    if flag=1 then jetflag=0
    'if jetflag=1 then
     '   flatx=airbornx(pointidx)
      '  flaty=airborny(pointidx)
       ' end if
   ' if jetflag=0 then
            'jetflag=1
		'// Translate points
		flatX = MyArray(pointIdx).X * (1000) / (MyArray(pointIdx).Z + zoomLevel)
		flatY = MyArray(pointIdx).Y * (1000) / (MyArray(pointIdx).Z + zoomLevel)
    '    airbornx(pointidx)=flatx:airborny(pointidx)=flaty
     '   if pointidx=numberofpoints then jetflag=1
   ' end if
    
       ' flat1 = MyArray(pointIdx-1).X * (1280) / (MyArray(pointIdx-1).Z + zoomLevel)
		'flat2 = MyArray(pointIdx-1).Y * (720) / (MyArray(pointIdx-1).Z + zoomLevel)
		'// Plot a white dot at the point's translated coordinates
       

' casual=(myarray(pointidx).x+zoomlevel*100/(1+zoomlevel)/100)
       ' if casual>1 then casual=1
 '  if myarray(pointidx).x<>0 and safe2x=0 then 
 '      safe2x=safe1x:safe2y=safe1y
 '      safe1x=flatx:safe1y=flaty
 '     '     line(safe1x+((1280)/2),safe1y+((720)/2))-(safe2x+((1280)/2),safe2y+((720)/2)),rgb(255*casual,255*casual,255*casual),bf
''       end if
'tie=2
'game2=2
if game2=2 then
        newX = COS(slphouse) * flatx - SIN(slphouse) * flaty'myarray(pointidx).y
        
 newY = SIN(slphouse) * flatx + COS(slphouse) * flaty'myarray(pointidx).y
    
    game8=0                                                                                       'elseif tie=2 then
                                                                                     '  newX = COS(slphouse) * flatx - SIN(slphouse) * flaty'myarray(pointidx).y
elseif game2=0 then
game8=0
     newX = COS(pieyedi*2-flat1+(pieyedi/2/1.5)) * flatx - SIN(pieyedi*2-flat1+(pieyedi/2/1.5)) * flaty'myarray(pointidx).y
     newY = SIN(pieyedi*2-flat1) * flatx + COS(pieyedi*2-flat1) * flaty'myarray(pointidx).y 
    ' newy=flaty
end if
if   (game2=1 or game2=12) then
newx=flatx
newy=flaty
'game8=1
'if game2>0 then newy=flaty
'newY = SIN(slphouse) * flatx + COS(slphouse) * flaty
end if
'game2=0

 ' if game2=3 and game8=1 then
 '   newx=flatx
'newy=flaty
'end if
 
casual=0
alpx=myarray(pointidx).x
alpy=myarray(pointidx).y
alpz=myarray(pointidx).z

'if alpx<0 then casual=casual+alpx
'if alpx>0 then casual=casual-alpx
'if alpy<0 then casual=casual+alpy
'if alpy>0 then casual=casual-alpy
'alpx=alpx+1
'alpy=alpy+1
'newx=flatx'cos(slphouse)*flatx'-sin(slphouse)*flaty
'casual=1-alpx*sin(slphouse)'  - SIN((slphouse)) 'myarray(pointidx).y'*(alpy*sin(slphouse))'alpx*tan((pieyedi*2/20)-slphouse-(pieyedi/2))*.5
'newy=flaty
casual=alpx*cos(flat1+(pieyedi*2/1.5))'+(pieyedi*2/20))'-(pieyedi*2/20))
casual2=alpy*sin(flat1+(pieyedi*2/1.5))'+(pieyedi*2/20))
casual3=alpx*cos(slphouse+pieyedi)'*2/20))
casual4=alpy*sin(slphouse+pieyedi)
'-(pieyedi*2/20))
apx1=(casual+casual2)*100/1/100'*32'*100/2/100)+(casual
apy1=(casual3+casual4)*100/-4/100'*32'*100/2/100)+(casual
casual=(apx1+apy1)*100/-4/100

'apx1=(casual+casual2)*100/1/100
'casual=casual*4'*32'*100/2/100)+(casual
'apy1=(casual3+casual4)*100/1/100'*32'*100/2/100)+(casual
'casual=(apx1+apy1)*100/2/100
casual=apx1*4
'newx=alpx*(1280-shrinkx)/(alpz+zoomlevel)'/zoomlevel
'newy=alpy*(720-shrinky)/(alpz+zoomlevel)'/zoomlevel
'newx=newx*cos(pieyedi/4)

if game<>0 then casual=1

'(alpy*2)/100*4/100*2
'casual=
'if alpx<0 then casual=1+alpx
'if alpx
'alpx2=myarray(pointidx).x
'if alpx<0 
'alphy=*cos(+(pieyedi*2/20)-slphouse-(pieyedi/2))
'alphx=myarray(pointidx).y*sin((pieyedi*2/20)-slphouse-(pieyedi/2))
'if slphouse>=pieyedi/4+(pieyedi*2/20)-(pieyedi/2) then  alphy2=myarray(pointidx).y*sin((pieyedi*2/20)-slphouse+pieyedi)
'alphyx=myarray(pointidx).x*cos(pieyedi*2+(pieyedi*2/20)-slphouse-(pieyedi/2))
'alphyy=myarray(pointidx).y*sin((pieyedi*2+pieyedi*2/20)-slphouse+pieyedi-(pieyedi/2))

'alphyx=myarray(pointidx).x*cos(pieyedi*2+(pieyedi*2/20)-slphouse)

'casual=1




'ELSE
'  alphy2=myarray(pointidx).y*sin(pieyedi*2+(pieyedi*2/20)-(PIEYEDI)-slphouse)
' END IF  
'alphyx=myarray(pointidx).x*cos(pieyedi*2+(pieyedi*2/20)-slphouse+pieyedi-(pieyedi/2))
 'alphyy=myarray(pointidx).y*sin(pieyedi*2+(pieyedi*2/20)-slphouse+pieyedi)
  'alphy2=myarray(pointidx).y
'alphy=alphy+1
' alphy2=alphy2+1
' alphyx=alphyx+1
' alphyy=alphyy+1
' omegy=.5
' if alphy>1 then 
     'casual=(1*(alphy*100/2/100))*(1*(alphyx*100/2/100))*2
    ' casual=(1*(alphy2*100/2/100))*(1*(alphyy*100/2/100))*2
     
         ' casual3=1*(alphyx*100/2/100)
         ' casual4=1*(alphyy*100/2/100)
          'casual=(casual+casual2)*100/2/100*1'.5'33333333
          'casual=casual-(.25/2)
                  '  if casual<=.49999999 then casual=casual-.25
 dim x as SINGLE
 
 x=rnd(timered)
 'if x=0 then goto rernd2
 if x<.6 then casual=casual-.04'03333333
 if x>.5 then casual=casual+.04'03333333
  if casual>1 then casual=1
          if casual<0 then casual=0
         ' casual=casual*.5'33333333
          'casual=1-casual
          '*.5'*.03333333
          'casual3=casual3+casual4/2
'omega=casual+casual3
'casual=omega
         ' casual=(casual+casual2/2)
'          casual2=1-casual2
'omegy=casual+casual2/2
' elseif alphy<=1 then
'  alphy=alphy+1
'     casual=1*(alphy*100/2/100)
'     end if
 'casual=omegy 
       '	newx=flatx
       '     newy=flaty
'[flat1 = COS(slphouse) * safe2x - SIN(slphouse) * safe2y'flaty'myarray(pointidx).y
        
'flat2 = SIN(slphouse) * safe2x + COS(slphouse) * safe2y'flaty'myarray(pointidx).y
'           line(newx+((1280)/2),newy+((720)/2))-(flat1+((1280)/2),flat2+((720)/2)),rgb(255*casual,255*casual,255*casual),bf
'safe2x=0:safe2y=0

'end if

'casual=1-casual
'if myarray(pointidx).z<>0 then 
'else
        '        circle (newx+(136*6/2), newy+(136*6/2)),3, rgb(255*casual,255*casual,251*casual)
'zoomlevel=zoomlevel=-0.1
'end if
'if myarray(pointidx).y<0 then
'    casual=1
'    elseif myarray(pointidx).y>=0 then
'if slphouse>=pieyedi then

'tie=2
'if tie=1 then
rem casual=1-(myarray(pointidx).x*100/2/100)*sin(slphouse)'*(1-myarray(pointidx).y*100/1/100)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)
rem casual=casual+(1-(myarray(pointidx).y*100/2/100))/2*cos(slphouse)'*100/2/100'    end if
'elseif tie=2 then
'casual=1-(myarray(pointidx).x*100/(1*2)/100)*sin(slphouse-(pieyedi*2)+(pieyedi*2/20)'*(1-myarray(pointidx)y*100/1/100)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)
' casual=casual+(1-(myarray(pointidx).y*100/(1*2)/100)*cos((pieyedi*2)+(pieyedi*2/20)-slphouse))/2'-slphouse)  
'if slphouse>pieyedi-(pieyedi*2/20*2) then
' casual=1-(myarray(pointidx).x*100/(1*2)/100)*cos((pieyedi*2-pieyedi*2/20)-slphouse)-(1-(myarray(pointidx).y*100/(1*2)/100)*sin(slphouse-(pieyedi*2-pieyedi*2/20)))'*(1-myarray(pointidx)y*100/1/100)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)
' casual2=1-(myarray(pointidx).x*100/(1*2)/100)*sin((pieyedi*2-pieyedi*2/20)-slphouse)+(1-(myarray(pointidx).y*100/(1*2)/100)*sin(slphouse-(pieyedi*2-pieyedi*2/20)))'-slphouse)  
'casual=.5
'casual2=.5
'casual=(myarray(pointidx).x*100/1/100)+1



'if myarray(pointidx).x>=0 then casual=(myarray(pointidx).x*100/(1)/100)*cos(-(pieyedi*2/20)+slphouse)'+(pieyedi*2/20) ' casual2=casual+(1-(myarray(pointidx).y*100/2/100)*cos(slphouse-(pieyedi*2)-(pieyedi*2/20)))/2'-(pieyedi*2-pieyedi*2/20))'*(1-myarray(pointidx)y*100/1/100)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)
'casual=.677'2-(myarray(pointidx).x*100/(1)/100)'+1*sin(-(pieyedi*2/20)+slphouse))'+(pieyedi*2/20) ' casual2=casual+(1-(myarray(pointidx).y*100/2/100)*cos(slphouse-(pieyedi*2)-(pieyedi*2/20)))/2'-(pieyedi*2-pieyedi*2/20))'*(1-myarray(pointidx)y*100/1/100)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)

'casual2=2-(myarray(pointidx).y*100/(2)/100)/2*sin(-(pieyedi*2/20)+(pieyedi*2)-slphouse)

 'casual=casual+(1-(myarray(pointidx).y*100/(1*2)/100))*sin(slphouse)/2'-slphouse)     
   ' casual=2-myarray(pointidx).x+1*100/2/100*(slphouse*100/(pieyedi)/100)
'if slphouse<=pieyedi-(pieyedi*2/20) then casual=1-casual
'casual=casual+casual2/2'1-casual
'casual=casual+casual2/(1-(myarray(pointidx).z*100/(1*2)/100))
'casual=1
'end if
'casual=1-casual
' casual=(2-(myarray(pointidx).x+1)*100/2/100)*(2-(myarray(pointidx).y+1)*100/2/100)*(myarray(pointidx).z*100/1/100)*(slphouse*100/(pieyedi*2)/100)
 'if casual>1 then casual=1
  ' casual=casual/2'.5
'   if pieyedi*2-slphouse>pieyedi then 
'       casual= 1-(1*(slphouse*100/pieyedi/100))
'elseif pieyedi*2-slphouse<pieyedi then
'          casual= (1*(slphouse*100/pieyedi/100))
 
' if myarray(pointidx).z>0 then casual=casual*(1-myarray(pointidx).z)   
'end if
'if slphouse=pieyedi then casual=0
'if pointidx<(6*8*6)+1 then


'dim as SINGLE coil(4)
'coil(1)=pieyedi/7
'coil(2)=pieyedi/5
'coil(3)=pieyedi/3
'coil(4)=pieyedi
'if alphy<=(pieyedi*2+(pieyedi*2/20))-pieyedi/4 and alphy2<=(pieyedi*2+(pieyedi*2/20))-pieyedi/4 then'or slphouse-(pieyedi/2)>=0-(pieyedi/2)+pieyedi*2/20+pieyedi/4 then
   ' rainbow=rgb(0*casual,255*casual,255*casual)
'else
'    rainbow=rgb(238*casual,130*casual,238*casual)
'    end if
' rainbow=wave2rgb(400+(waverycalc-((waverycalc)*pointidx*100/numberofpoints/100)))
'if casual>.5 then 
    rainbow=rgb(255,255,0)'rgb(255,251,0)'RGB (208, 83, 64)'rgb(255,255,0)'153,101,21)'&h00d024'rgb(153, 101, 21)
'else

   ' casual=.5-casual
   ' casual=.5+casual
 '   rainbow=RGB(0,0,0)
  '  end if
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )
if game2>0 then
rainbow=rgba(d*casual,n*casual,e*casual,127)
'casual=casual*1.33:if casual>1 then casual=1
else
    rainbow=rgb(255*casual,255*casual,255*casual)
end if
 screenlock
  tier=1
rem   if ((slphouse<(pieyedi*2)+(pieyedi*2/20)-(pieyedi/3)) and (slphouse>(pieyedi*2)+(pieyedi*2/20)-(pieyedi/2)-(pieyedi/3))) or ((slphouse<(pieyedi*2)+(pieyedi*2/20)-pieyedi-(pieyedi/3)) and (slphouse>(pieyedi*2)+(pieyedi*2/20)-pieyedi-(pieyedi/2)-(pieyedi/3))) then
 rem  tier=0   		
         rem     circle (newX+((1280)/2), newy+((720)/2)),3,rgb(0,0,0)'rainbow'rgb(255*casual,255*casual,0*casual)', rgb(d*casual,n*casual,e*casual)
                	'	circle (newX+((1280)/2), newy+((720)/2)),2, rgb(d*casual,n*casual,e*casual)
     	rem	circle (newX+((1280)/2), newy+((720)/2)),2,rgb(0,0,0)
rem  else
 rem   tier=1
' if newy+((720)/2)<=(720)/2 and game=0 then
    circle (newX+((1280)/2+((1280/2)*.60)), newy+(720)/2+(720/2/16)),2,rainbow'rgb(238*casual,127+3*casual,238*casual)'rainbow'rgb(255*casual,255*casual,0*casual)', rgb(d*casual,n*casual,e*casual)
                	'	circle (newX+((1280)/2), newy+((720)/2)),2, rgb(d*casual,n*casual,e*casual)
    '		circle (newX+(1280*.75), newy+(720)/2-(720/16)),2,rainbow'rgb(238*casual,127+3*casual,238*casual)'rg
   
rem  end if


'circle(newX+(1280*.75),newy+(720)/2-(720/16)),1,rainbow'rgb(255,255,255)'255,251,0)'rgb(238*casual,127+3*casual,238*casual)'rgb(255*casual,251*casual,0*casual)
'else
'circle (newX+((1280)/2), newy+((720)/2)),3,rainbow'rgb(238*casual,127+3*casual,238*casual)'rainbow'rgb(255*casual,255*casual,0*casual)', rgb(d*casual,n*casual,e*casual)
                	'	circle (newX+((1280)/2), newy+((720)/2)),2, rgb(d*casual,n*casual,e*casual)
'    		circle (newX+((1280)/2), newy+((720)/2)),2,rainbow'rgb(238*casual,127+3*casual,238*casual)'rg
   
rem  end if

screenunlock

'circle(newx+((1280)/2),newy+((720)/2)),1,rainbow'rgb(255,255,255)'255,251,0)'rgb(238*casual,127+3*casual,238*casual)'rgb(255*casual,251*casual,0*casual)    
end if
NEXT

end if
'waver2=wave2rgb((650+(waverycalc*(1*100/26/100)))-(waverycalc*(cc1d*100/26/100)))
'wavermid=wave2rgb((650+(waverycalc*(1*100/26/100)))-(waverycalc*(cc1d*100/26/100))/2)
'paint(0,0),RGB (208, 83, 64)'violet,violet
'line(0,(720)/2)-(1280,720),rgb(194/2,178/2,128/2),bf
'wavepool=wave2rgb(400)
'for tt22=1 to 26
   ' color wave2rgb((720)-(waverycalc*(t2*100/26/100))),rgb(0,0,0)
        'locate 3,1280/8/2-(26/2)+t2
 '       dim cow as long:cow=wave2rgb((400+(waverycalc*(cc1d*100/26/100))))
  '      Dim waver4 As long
'waver4=cow:waver3=waver2
'd = RGBA_R( cow )
'n = RGBA_G( cow )
'e = RGBA_B( cow )
'if tt22>26-7 then
'   dim cal as SINGLE

'goto nxt3'peanutbutter


'cal=(8-(tt22))
'if cal=0 then cal=1
'd=int(d/cal)
'n=int(n/cal)
'e=int(e/cal)



'cal=(27-(tt22))
   ' if cal=0 then cal=0.5
 '   cal=8-cal
 '   d=int(d/cal)
 '   n=int(n/cal)
 '  e=int(e/cal)
'end if
'goto slipper
'color ,rgb(0,0,0)
'color ,rgb(255,192,203)

'color ,rgb(7*bright,7*bright,7*bright)
'dim skipper as long
'if cccc=3 then 
'    skipper=waver3 
'elseif cccc=1 then  
'    skipper=waver4 
'else 
'    skipper=wave2rgb(400+(waverycalc/2))
'end if
'dim ab as SINGLE
'ab=2.54'200'2'30'2'1'    5'1.90'50'33'.25
'dim ab2 as SINGLE
'ab2=2.555'if t2=1 then d=255:n=0:e=0
'goto jumper
'dim as SINGLE ba=1.036,za=1.397
'end if
'*ab2)-(1280/(6.67*ba)+(80*ab),720024-1/(4.25*za)-(80*ab2)),skipper'skipper
'line(1280/(6.67*ba)+(80*ab),720024-1/(4.25*za)-(80*ab2))-(1280280-1/(6.67*ba)-(80*ab2),720024-1/(4.25*za)-(80*ab)),skipper
'line(1280280-1/(6.67*ba)-(80*ab),720024-1/(4.25*za)-(80*ab2))-((1280)/2,(60+80*ab2)),skipper
'line((1280)/2,720-(60+80*ab2))-(1280/(6.67*ba)+(80*ab),720/(4.25*za)+(80*ab2)),skipper
'line(1280/(6.67*ba)+(80*ab),720/(4.25*za)+(80*ab2))-(1280280-1/(6.67*ba)-(80*ab2),720/(4.25*za)+(80*ab)),skipper
'line(1280280-1/(6.67*ba)-(80*ab),720/(4.25*za)+(80*ab2))-((1280)/2,720-(60+80*ab2)),skipper
'skippy3:
'circle((1280)/2,(720)/2),1280/6-56,longer

'circle((1280)/2,(720)/2),1280/6-55,longer
'circle((1280)/2,(720)/2),1280/6-54,longer
'skippy3:
'screencopy
'line((1280)/2,60+80*ab)-(1280/(6.67*ba)+(80*ab),720024-1/(4.25*za)-(80*ab)),rgb(255,255,255)'rgb(255,255,255)
'line(1280/(6.67*ba)+(80*ab),720024-1/(4.25*za)-(80*ab))-(1280280-1/(6.67*ba)-(80*ab),720024-1/(4.25*za)-(80*ab)),rgb(255,255,255)
'line(1280280-1/(6.67*ba)-(80*ab),720024-1/(4.25*za)-(80*ab))-((1280)/2,(60+80*ab)),rgb(255,255,255)
'line((1280)/2,720-(60+80*ab))-(1280/(6.67*ba)+(80*ab),720/(4.25*za)+(80*ab)),rgb(255,255,255)
'line(1280/(6.67*ba)+(80*ab),720/(4.25*za)+(80*ab))-(1280280-1/(6.67*ba)-(80*ab),720/(4.25*za)+(80*ab)),rgb(255,255,255)
'line(1280280-1/(6.67*ba)-(80*ab),720/(4.25*za)+(80*ab))-((1280)/2,720-(60+80*ab)),rgb(255,255,255)
'screencopy
'paint(
'line((1280)/2,33)-(33,720024-1/4.25-33),rgb(d,n,e)
'line(33,720024-1/4.25-33)-(1280-33,720024-1/4.25-33),rgb(d,n,e)
'line(1280-33,720024-1/4.25-33)-((1280)/2,33),rgb(d,n,e)
'line((1280)/2,720-33)-(33,720/4.25+33),rgb(d,n,e)
'line(33,720/4.25+33)-(1280-33,720/4.25+33),rgb(d,n,e)
'line(1280-33,720/4.25+33)-((1280)/2,720-33),rgb(d,n,e)
'paint(0,(720)/2),waver3,rgb(0,0,0)
'line((1280)/2,33)-(33,720024-1/4.25-33),rgb(0,0,0)
'line(33,720024-1/4.25-33)-(1280-33,720024-1/4.25-33),rgb(0,0,0)
'line(1280-33,720024-1/4.25-33)-((1280)/2,33),rgb(0,0,0)
'line((1280)/2,720-33)-(33,720/4.25+33),rgb(0,0,0)
'line(33,720/4.25+33)-(1280-33,720/4.25+33),rgb(0,0,0)
'line(1280-33,720/4.25+33)-((1280)/2,720-33),rgb(0,0,0)
'paint((1280)/2,(720)/2),waver3,rgb(d,n,e)',waver3
'screencopy
'goto jumper


'' Now the font buffer is ready; we could save it using BSAVE for later use
Rem BSave "myfont.bmp", myFont




'if cccc=1 then
'    waver3=waver3
'    backup2=rgb(d,n,e)
'elseif cccc=3 then
'    waver3=rgb(d,n,e)
'    backup2=backup
'else
' waver3=wavermid'rgb(d,n,e)
'    backup2=wavermid   
 '   end if
'goto nxt2







'circle((1280)/2,33),4,waver3
'paint((1280)/2,33),waver3
'circle(33,720024-1/4.25-33),4,waver3
'paint(33,720024-1/4.25-33),waver3
'circle(1280-33,720024-1/4.25-33),4,waver3
'paint(1280-33,720024-1/4.25-33),waver3
'circle((1280)/2,720-33),4,waver3
'paint((1280)/2,720-33),waver3
'circle(33,720/4.25),4,waver3
'paint(33,720/4.25),waver3
'circle(1280-33,720/4.25),4,waver3
'paint(1280-33,720/4.25),waver3
'screencopy 1,0

pieyedi=3.14159265359


if game2=0 then 
   ' circle(((1280)/2)+cos(flat1)/pee,((720)/2)+sin(flat1)/pee),720/(pieyedi*2)*.66,RGB(255,255,255)

'paint(((1280)/2)+cos(flat1)/pee,((720)/2)+sin(flat1)/pee),rgb(255, 255,255),RGB(255,255,255)
'sin(flat1+pieyedi-(pieyedi*2/20)/8), F

'paint(hammerx,hammery-64*sin(flat1+pieyedi-(pieyedi*2/20)/2)),rgb(255,127,0),rgb(255,127,1)
'paint(hammerx,hammery+64*sin(flat1+pieyedi-(pieyedi*2/20)/2)),rgb(255,127,0),rgb(255,127,1)
'paint(hammerx-64,hammery),rgb(255,127,0),rgb(255,127,1)
'paint(hammerx-64,hammery),rgb(255,127,0),rgb(255,127,1)
hammerx=((1280)/2*1.15)+(cos((pieyedi*8)-flat1*4)*((1280/4*5)*.6*.5))
hammery=((720)/2)+(sin((flat1*4))*((720)*5*(.25/3)*.5))
screenlock
circle(hammerx,hammery),18*1.15,RGB(255,7,7),,,,F'VIOLET'rgb(255,255,255)
screenunlock
'paint(hammerx,hammery),RGB(255,7,7),RGB(255,7,6)'rgb(255,255,255)
'paint(hammerx,hammery+16),RGB(255,7,7),RGB(255,7,6)'rgb(255,255,255)
'rgb(0,215,255)'
'paint(hammerx,hammery),RGB(255,7,7),RGB(255,7,6)'rgb(255,255,255)
'paint(hammerx,hammery),RGB(255,7,7),RGB(255,7,6)'rgb(255,255,255)
end if
',@setalpha
'pset
ran=int(rnd * 1000)+2
'if ran=0 then goto redornd
randomize int(rnd * (720))+1*timer*ran
'screenlock
rem if   (game2=1 or game2=12) then hellloirene rem thishellloireneworks
'END IF
'screenunlock
'dim corky as integer
'for corky=1 to 26
'jumper:
'circle((1280)/2,(720)/2),((1280)/2-(70+80)),rgb(255,215,0)'d,n,e)

'circle((1280)/2,(720)/2),(26*5.25),rgb(255,215,0)'d,n,e)

'for cccc=1 to 26'79'26 to 1 step -1
'goto nosun

nosun:

'if cccc>8 then
'nxt3:
'counts2=counts2+1
'counts=counts+1
'screencopy 1,0:sleep
 'dddd(1)=cow2(26)
' dddd(2)=cow2(2)
' dddd(3)=cow2(3)
' if three=0 then three=1
'color ,rgb(0,0,0)'violet


'randomize timer*pee'*int(rnd * 255*255)+2
'for skyx=1 to 1280 step +xint'int(rnd * +2)+1
 '   for skyy=1 to (720)/2 step +yint'int(rnd * +2)+1
' xint=int(rnd * 2)+1
'if xint=0 then xint=1
'yint=int(rnd * 2)+1
'if yint=0 then yint=1



'if game2=0 then
'screenlock
'put(1,1),skyblue,alpha,192'27/2
'screenunlock
'end if



'end if   
'flep=flep+1
'if flep>6 then flep=1

jumptokey:

dim eye as integer
dim eye2 as integer
dim eye3 as integer
eye3=0
eye2=1
eye3=1
if helpflag=1 then
    eye2=3
    eye3=3
    goto dropoff
    end if
if game2=2 or game2=4 or game2=6 or game2=11 then eye2=1:eye3=2
if timeflag=1 and (game2=1 or game2=12 or game2=3 or game2=5 or game2=10 or game2=13) then
    eye3=3
    eye2=3
end if
if timeflag=0 and (game2=1 or game2=12 or game2=3 or game2=5 or game2=10 or game2=13) then
    eye3=4
    eye2=4
end if
if (game2=0) then
    eye2=1
    eye3=1
end if

if eye2=4 then goto exitmix
dropoff:
for eye=eye2 to eye3
screenset 3,0
color rgb(255,255,255),rgb(0,0,0)'0

cls
'paint(1,1),rgb(0,0,0)

if eye=1 or eye=3 then
hrs=""
hours=""
if val(left(time,2))>12 then 
    hours=hours+str(val(left(time,2))-12)
    if val(left(hours,2))<10 then hours="0"+hours

else
    if val(left(time,2))=0 then
        hours="12"
        else
    hours=hours+str(val(left(time,2)))
    end if
        if val(left(hours,2))<10 then hours="0"+hours

end if
'if hours<10 then 
'    hrs="0"+str(hours)
'else
    hrs=hours
'end if
hrs=hrs+right(time,6)
end if
'if hour>12 then 
'    hours=hour-12
'else
'    hours=hour
'end if
'if hour<10 then 
'    hrs="0"+hours
'else
'    hrs=hours
'end if
'if hour>12 then 
'    hours=hour-12
'e''lse
 '   hours=hour
'end if
'if hour<10 then 
'    hrs="0"+hours
'else
'    hrs=hours
'    end if

if eye=1  then
    screenlock

    draw string (1,1),hrs+"     "+left(str(date),6)+right(str(date),2),rgb(255,255,255)
    screenunlock
elseif eye=2 and (game2=2 or game2=4 or game2=6 or game2=11) then
screenlock
       draw string (1,1),"TOP:"+str(cup9top),rgb(255,255,255)
       draw string (12*8-7,1),"BOTTOM:"+str(cup9bottom),rgb(255,255,255)
       screenunlock
   elseif eye=3 then
    screenlock

    draw string (1,1),hrs+"     "+left(str(date),6)+right(str(date),2),rgb(255,255,255)
    screenunlock
'elseif eye=2 and timeflag=0 then
 '   eye=3
'    eye2=3
' exit for
end if
'if eye2>2 then exit for
for ccx=0 to 21*8'64
    for ccy=0 to 8
        if point(ccx,ccy)=rgb(255,255,255) then
          ' for ccx2=1 to 6
          '     for ccy2=1 to 6
                   display(ccx,ccy,rgb(255,0,0),0)
                   ' txtyy()=rgb(255,255,255)
            '   next ccy2
            'next ccx2
        elseif point(ccx,ccy)=rgb(0,0,0) then
          'for ccx2=1 to 6
          '     for ccy2=1 to 6
                   display(ccx,ccy,rgb(0,0,0),0)
                   ' txtyy()=rgb(255,255,255)
      '   end if   
           ' next ccy2
           ' next ccx2  
        end if
    next ccy
next ccx
cls
'screenunlock

'if game2=0 then screenset 1,0
'color ,rgb(0,0,0)
prcx1=1280/128
prcy1=720/8/4
'tie=0
dim ccxtmp AS integer
ccxtmp=1
 
'if eye=1 and (game2=0 or (game2>0 and timeflag=1)) then
for ccx=0 to 21*8*6:for ccy=0 to 8*6

 '   if ccy>8*6-1 then txtxxyy(ccx,ccy)=rgb(255,255,0)
 '   if ccx>136*6-1 then txtxxyy(ccx,ccy)=rgb(255,255,0)
rem if ccy=1 or ccy=8*6 or ccx=1 or ccx=136*6 then txtxxyy(ccx,ccy)=rgb(0,0,0)
screenlock
 if eye<4 then
     
  if (eye=1 or eye=3) then pset pic,(ccx,ccy),txtxxyy(ccx,ccy)
   if (eye=1 or eye=2) then  pset ((1280)/2-((21*8*6)/2)+ccx,ccy),txtxxyy(ccx,ccy)
  rem dreams
'  if eye=2 then    pset ((1280)/2-((21*8*6)/2)+ccx,ccy),txtxxyy(ccx,ccy)
' elseif game2>0 then
'        pset((1280)/2-((21*8*6)/2)+ccx,ccy),txtxxyy(ccx,ccy)
  end if
  screenunlock
next ccy:next ccx

'if ccx>136*6/2+16 and ccx<136*6/2 and ccy>(6*8-11) and ccy<6*8-2 then
pieyedi=3.1415926359
 

 '   end if
'dim cysingle as SINGLE
'rad=1
'if eye>0 then
'eye=1 then
if eye=1 or eye=2 then
for ccx=1 to 21*8*6:for ccy=1 to 8*6

 '   goto jmmpdump
 'if game2=0 then
    if point((1280)/2-(21*8*6/2)+ccx,ccy)=rgb(255,0,0) then
'   for xtmp=1 to int(prcx1):for xtmp2=1 to int(prcy1)     
     MyArray(ccxtmp).X=-1+((ccx)*100/(21*8*6)/100*2)
     ' for ccxtmp=1 to 17                   txtxxyy(ccx*6-6+1+ccx2,ccy*6-6+1+ccy2)=colr'rgb(255,255,255)

     MyArray(ccxtmp).y=-1+((ccy)*100/(8*6)/100*2)
    ' next ccxtmp
     myarray(ccxtmp).z=0'1.5'2-(-1+((ccy)*100/(8*6)/100*2))'-1+((ccy)*100/(8*6)/100*2)'ccx*100/(136*6)/100'.5'10'.75'1'.5'ccx*100/(136*6)/100
     
elseif point((1280)/2-(21*8*6/2)+ccx,ccy)=rgb(0,0,0) then
    myarray(ccxtmp).x=0
    myarray(ccxtmp).y=0
    myarray(ccxtmp).z=0
    
'next xtmp2:next xtmp
end if
'else
 '  if point((1280)/2-(136*6/2)+ccx,ccy)=rgb(255,255,0) then
'   for xtmp=1 to int(prcx1):for xtmp2=1 to int(prcy1)     
 '    MyArray(ccxtmp).X=-1+((ccx)*100/(136*6)/100*2)
     ' for ccxtmp=1 to 17                   txtxxyy(ccx*6-6+1+ccx2,ccy*6-6+1+ccy2)=colr'rgb(255,255,255)

 '    MyArray(ccxtmp).y=-1+((ccy)*100/(8*6)/100*2)
    ' next ccxtmp
  '   myarray(ccxtmp).z=0'1.5'2-(-1+((ccy)*100/(8*6)/100*2))'-1+((ccy)*100/(8*6)/100*2)'ccx*100/(136*6)/100'.5'10'.75'1'.5'ccx*100/(136*6)/100
     
'elseif point((1280)/2-(136*6/2)+ccx,ccy)=rgb(255,0,0) then
'    myarray(ccxtmp).x=0
'    myarray(ccxtmp).y=0
'    myarray(ccxtmp).z=0
    
'next xtmp2:next xtmp
'end if
'end if
ccxtmp=ccxtmp+1

next ccy:next ccx
end if
ender3:
'end if
next eye
'end if
'cls
exitmix:'22222222222221222222222222222222222222222222222222222

'else
'     MyArray(ccx).X=0'ccx*100/136/100
'for ccxtmp=1 to 17
'     MyArray(ccx).y=0'ccy*17-17+ccxtmp*100/(136*6+16)/100
' next ccxtmp     
' myarray(ccx).z=0'ccx*100/136/100
'end if
'jmmpdump:
if helpflag=0 then 
    screenset 1,0
else
    screenset 2,0
    end if

' if game2>0 then goto gametwo

'if game=0 then put((1280)/2-((136*6)/2),0),pic,alpha

'gametwo:

'if game=1 then color ,rgb(238,130,255)
'if game2>0 then paint(1,1),RGB (208, 83, 64),RGB (208, 83, 64)
'if zoomlevel<-1 then zoomlevel=1'1.5'0'256/2
'zoomlevel=zoomlevel-.01
zoomlevel=9.33'6.67'5.8'.25'6.50*2'.5'0'1.5'-.5'0'12'-.5'0'2
'if tie2>60 then
'    tie2=0
'end
'    tie

'tie=int(rnd *2)+1
'end if
'tie2=tie2+1
pieyedi=3.14159265359
if tie=0 then tie=2:slphouse=0'pieyedi*2
if slphouse>pieyedi*2-(pieyedi*2/20)-pieyedi*2 then 
    slphouse=0-(pieyedi*2/20)'-slphouse
'if tie=1 then
'    tie=2
'else
'    tie=1
'end if
end if'slphouse
if slphouse2>pieyedi*2-(pieyedi*2/20)-pieyedi*2/(720*32) then 
    slphouse2=0-(pieyedi*2/20)'-slphouse
'if tie=1 then
'    tie=2
'else
'    tie=1
'end if
end if'
'if slphouse>pieyedi*2 and tie=1 then slphouse=slphouse-pieyedi*2:tie=2'slphouse=slphouse-(pieyedi*2):tie=2
 if flag=1 then
     shrinkx=shrinkx+8
     shrinky=shrinky+8
     if shrinkx>1280 then shrinkx=1280
     if shrinky>720 then shrinky=720
 elseif flag=0 then
     shrinkx=0:shrinky=0
     end if
'if tie2<10 then

dim as fb.Image Ptr  mirrored=imagecreate(1280,1)',rgb(127,127,127),32)
'if game>0 then goto gameseven

'flip
'end if

'screenunlock
'end if

if game2=0 then
    screenlock

   ' line(0,(720)/2+1)-(1280,720),rgb(63,63,63),bf
        for straighty=1 to (720)/2'-1'*.5'*.66'7'89'/2 'to 1 step -1
get(0,straighty)-(1280-1,straighty),mirrored
put(0,720-straighty),mirrored,pset'alpha,255/1.5'192

'flip 
next
screenunlock
end if
'line(0,(720)/2+1)-(1280,(720)/2+1),rgb(0,0,0)

dim as SINGLE xfive
dim as integer hammerx2,hammery2
'xfive=(1.33/4*(flat1+pieyedi+(pieyedi/2)*100/pieyedi/100))
'xfive=(1.5*(flat1*100/(pieyedi*4)/100))
'xfive=(.5/(flat1*100/(pieyedi)/100))
'xfive=(.5*(flat1+pieyedi*100/(pieyedi)/100))
'xfive=(cos(flat1+(pieyedi/2))*100/(pieyedi)/100)
'xfive=1+(.75*sin(flat1+pieyedi+(pieyedi*2/20))*100/(pieyedi)/100)
'xfive=1-(1*cos(flat1-(pieyedi/2)+(pieyedi*2/20))*100/(pieyedi)/100)
'xfive=1-(1*cos(flat1-(pieyedi/2)+(pieyedi*2/20))*100/(pieyedi*2+(pieyedi*2/20))/100)

if game2=80 then
 zoomlevel=8'2.33'9'6.5'4'3.5'4.66'10'2.5
    FOR pointIdx = 1 TO numberOfstars
	'// Do not draw points that are behind the screen
	'	 IF stars(pointIdx).Z > 0 then 'and (stars(pointidx).x+stars(pointidx).y)>0 or (stars(pointidx).x+stars(pointidx).y)<0 THEN

    '    if flag=1 then jetflag=0
    'if jetflag=1 then
     '   flatx=airbornx(pointidx)
      '  flaty=airborny(pointidx)
       ' end if
   ' if jetflag=0 then
            'jetflag=1
		'// Translate points
     '   if stars(pointidx).z>0 then
     if (stars(pointidx).z+zoomlevel)>0 then
     dim as SINGLE flatz,newz
 '            flatz=stars(pointidx).z+zoomlevel*sin(flat1)'*(xfive*1.5*2)

		'flatX = stars(pointIdx).X*cos(flat1)*(xfive*1)/(stars(pointidx).z+zoomlevel)'*sin(flat1))
		'flatY = stars(pointIdx).Y*sin(flat1)*(xfive*1)/(stars(pointidx).z+zoomlevel)'*sin(flat1))'*  / (stars(pointidx).z+zoomlevel)
'flatx=stars(pointidx).x*((xfive*2))/(stars(pointidx).z/16+zoomlevel)
'flaty=stars(pointidx).y*((xfive*2))/(stars(pointidx).z/16+zoomlevel)

'flatx=flatx*(xfive*1.5)
               '         newx = (cos(pieyedi*4-(flat1*2))*flatx) - (sin((flat1*2)-(pieyedi*4))*flaty)/flatz'*(xfive*1.5))' * (flatz)))
'newx=flatx
'newy=flaty










'dim as SINGLE flatz,newz
		

flatx=stars(pointidx).x*((xfive))/(stars(pointidx).z/16+zoomlevel)
flaty=stars(pointidx).y*((xfive))/(stars(pointidx).z/16+zoomlevel)
flatz=stars(pointidx).z*((xfive)/16)
 
 newx = flatx'newx'
newy = (sin(slphouse2) * flatz) + (cos(slphouse2) * flaty)
newz = (cos(slphouse2) * flatz) - (sin(slphouse2) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(slphouse2) * flatx) - (sin(slphouse2) * flatz)
newy=flaty''y = y'
newz = (sin(slphouse2) * flatx) + (cos(slphouse2) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 
 newx = (cos(slphouse2) * flatx) + (sin(slphouse2) * flaty)
newy = (cos(slphouse2) * flaty) - (sin(slphouse2) * flatx)
newz = flatz'
 flatx=newx
 flaty=newy
 flatz=newz
 
 
casual=1-((1)*flatz*100/((1280)/2)/100)
if casual>1 then casual=1
if casual<0 then casual=0
    rainbow=rgb(255,255,251)
 '  rainbow=rgb(238,130,238)
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )

rainbow=rgba(d*casual,n*casual,e*casual,127)
 'screenlock
 '  circle (newX+((1280)/2), newy+((720)/2)),2,rainbow,,,,F

'screenunlock













             ' newy = (sin((flat1*2)-(pieyedi*4)) * flaty) + (cos((pieyedi*4-flat1*2)) * flatx)/flatz'/stars(pointidx).z
             ' dim as SINGLE newestx,newesty
             ' newestx=newx
             ' newesty=newy
              
      '  newx=newx/(xfive*1.5*2)
       ' newx=flatx/flatz
        
' newY = SIN(slphouse) * flaty + COS(slphouse) * flatx'/stars(pointidx).z
       ' newy = flaty/flatz'*100'2'

       ' newz = (sin(flat1*2-(pieyedi/2))*flatx) + (cos(flat1*2-(pieyedi/2))/flatz)' * (flatz)))

'newy=newy*(xfive*1.5*2)'/stars(pointidx).z
'newx=newx*(xfive*1.5*2)'/stars(pointidx).z

' x = (cos(angle) * x) - (sin(angle) * z)
'y = y'
'z = (sin(angle) * x) + (cos(angle) * z)
 ' newX = COS((pieyedi)) * flatx - SIN((pieyedi)) * flaty'myarray(pointidx).y
        
' newY = SIN((pieyedi)) * flatx + COS((pieyedi)) * flaty
 
 
 
 
 'newx=flatx
 
 'newy=((sin(flat1*2-(pieyedi/2)) * flatz) + (cos(flat1*2-(pieyedi/2)) * stars(pointidx).y))   
    'elseif stars(pointidx).z<0 then
   ' x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')
   ' dim as SINGLE flatz
    '    newx = stars(pointidx).x * (xfive*1.5*2)
'flaty = (sin(flat1*2) * stars(pointidx).z)+ (cos(flat1*2) / stars(pointidx).y)
'flatz = (cos(flat1*2) * stars(pointidx).z) - (sin(flat1*2) * stars(pointidx).y)
 'flatX = stars(pointIdx).X' * ((xfive)*1.5*2) '/ (flatZ+zoomlevel)
'flatx=flatx/(flatz*5)'stars(pointidx).z'*(xfive*1.5)
'flaty=flaty/(flatz*5)'stars(pointidx).z'*(xfive*1.5)

'newy=flaty
'newx=flatx


'		flatY = stars(pointIdx).Y * ((xfive)*1.5*2) / (flatZ+zoomlevel)
'x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')

'flatx=flatx/flatz
'flaty=flaty*flatz
'    end if
       ' flatX = stars(pointIdx).X * ((xfive)*1.5*2)*COS(flat1*2) * (stars(pointidx).z+zoomlevel)
	'	flatY = stars(pointIdx).Y * ((xfive)*1.5*2)*sin(flat1*2) * (stars(pointidx).z+zoomlevel)
    '    end if
    '    airbornx(pointidx)=flatx:airborny(pointidx)=flaty
     '   if pointidx=numberofpoints then jetflag=1
   ' end if
    
       ' flat1 = stars(pointIdx-1).X * (1280) / (stars(pointIdx-1).Z + zoomLevel)
		'flat2 = stars(pointIdx-1).Y * (720) / (stars(pointIdx-1).Z + zoomLevel)
		'// Plot a white dot at the point's translated coordinates
       

' casual=(stars(pointidx).x+zoomlevel*100/(1+zoomlevel)/100)
       ' if casual>1 then casual=1
 '  if stars(pointidx).x<>0 and safe2x=0 then 
 '      safe2x=safe1x:safe2y=safe1y
 '      safe1x=flatx:safe1y=flaty
 '     '     line(safe1x+((1280)/2),safe1y+((720)/2))-(safe2x+((1280)/2),safe2y+((720)/2)),rgb(255*casual,255*casual,255*casual),bf
''       end if
'tie=2
'game2=2
'if game2=2 then
'newy=flaty

     '   newX = COS(flat1) * flatx - SIN((pieyedi*2)-flat1) * flaty'/((xfive*1*.5)*flatz)'
        '/stars(pointidx).z
        
' newY = SIN((pieyedi*2)-flat1) * flaty + COS(flat1) * flatx'/((xfive*1*.5)*flatz)
'    newx=flatx
'    newy=flaty
 '   game8=0                                                                                       'elseif tie=2 then
                                                                                     '  newX = COS(slphouse) * flatx - SIN(slphouse) * flaty'stars(pointidx).y
'elseif game2=0 then
'game8=0
'     newX = COS(pieyedi*2-flat1+(pieyedi/2/1.5)) * flatx - SIN(pieyedi*2-flat1+(pieyedi/2/1.5)) * flaty'stars(pointidx).y
'     newY = SIN(pieyedi*2-flat1) * flatx + COS(pieyedi*2-flat1) * flaty'stars(pointidx).y 
    ' newy=flaty
'end if
'if   (game2=1 or game2=12) then
'newx=flatx
'newy=flaty
'game8=1
'if game2>0 then newy=flaty
'newY = SIN(slphouse) * flatx + COS(slphouse) * flaty
'end if
'game2=0

 ' if game2=3 and game8=1 then
 '   newx=flatx
'newy=flaty
'end if
'newy=alpy*(720-shrinky)/(alpz+zoomlevel)'/zoomlevel
'newx=newx*cos(pieyedi/4)

'if game<>0 then casual=1

'(alpy*2)/100*4/100*2
'casual=
'if alpx<0 then casual=1+alpx
'if alpx
'alpx2=stars(pointidx).x
'if alpx<0 
'alphy=*cos(+(pieyedi*2/20)-slphouse-(pieyedi/2))
'alphx=stars(pointidx).y*sin((pieyedi*2/20)-slphouse-(pieyedi/2))
'if slphouse>=pieyedi/4+(pieyedi*2/20)-(pieyedi/2) then  alphy2=stars(pointidx).y*sin((pieyedi*2/20)-slphouse+pieyedi)
'alphyx=stars(pointidx).x*cos(pieyedi*2+(pieyedi*2/20)-slphouse-(pieyedi/2))
'alphyy=stars(pointidx).y*sin((pieyedi*2+pieyedi*2/20)-slphouse+pieyedi-(pieyedi/2))

'alphyx=stars(pointidx).x*cos(pieyedi*2+(pieyedi*2/20)-slphouse)

'casual=1




'ELSE
'  alphy2=stars(pointidx).y*sin(pieyedi*2+(pieyedi*2/20)-(PIEYEDI)-slphouse)
'casual=casual*1.33:if casual>1 then casual=1
'else
'    rainbow=rgb(0,255*casual,255*casual)
'end if
 screenlock
 ' tier=1
rem   if ((slphouse<(pieyedi*2)+(pieyedi*2/20)-(pieyedi/3)) and (slphouse>(pieyedi*2)+(pieyedi*2/20)-(pieyedi/2)-(pieyedi/3))) or ((slphouse<(pieyedi*2)+(pieyedi*2/20)-pieyedi-(pieyedi/3)) and (slphouse>(pieyedi*2)+(pieyedi*2/20)-pieyedi-(pieyedi/2)-(pieyedi/3))) then
 rem  tier=0   		
         rem     circle (newX+((1280)/2), newy+((720)/2)),3,rgb(0,0,0)'rainbow'rgb(255*casual,255*casual,0*casual)', rgb(d*casual,n*casual,e*casual)
                	'	circle (newX+((1280)/2), newy+((720)/2)),2, rgb(d*casual,n*casual,e*casual)
     	rem	circle (newX+((1280)/2), newy+((720)/2)),2,rgb(0,0,0)
rem  else
 rem   tier=1
' if newy+((720)/2)<=(720)/2 and game=0 then

   'and newx+hammerx2>hammerx2-(xfive*1.5)  and newx+hammerx2<hammerx2+(xfive*1.5) and newy+hammery2>hammery2-(xfive*1.5) and newy+hammery2<hammery2+(xfive*1.5) then
  ' dim zzzz as SINGLE
  ' zzzz=(stars(pointidx).z-(stars(pointidx).z*2)
   rem yellow
   pset (newX+hammerx2, newy+hammery2),rainbow
   'dim randy as SINGLE=rnd
   'if rand
   screenunlock

  ' if rnd>.5 then pset (newX+hammerx2, newy+hammery2),rgb(255*casual,255*casual,255*casual)'rainbow
   end if
    
    
    'rgb(238*casual,127+3*casual,238*casual)'rainbow'rgb(255*casual,255*casual,0*casual)', rgb(d*casual,n*casual,e*casual)
                	'	circle (newX+((1280)/2), newy+((720)/2)),2, rgb(d*casual,n*casual,e*casual)
    '		circle (newX+(1280*.75), newy+(720)/2-(720/16)),2,rainbow'rgb(238*casual,127+3*casual,238*casual)'rg
   
rem  end if


'circle(newX+(1280*.75),newy+(720)/2-(720/16)),1,rainbow'rgb(255,255,255)'255,251,0)'rgb(238*casual,127+3*casual,238*casual)'rgb(255*casual,251*casual,0*casual)
'else
'circle (newX+((1280)/2), newy+((720)/2)),3,rainbow'rgb(238*casual,127+3*casual,238*casual)'rainbow'rgb(255*casual,255*casual,0*casual)', rgb(d*casual,n*casual,e*casual)
                	'	circle (newX+((1280)/2), newy+((720)/2)),2, rgb(d*casual,n*casual,e*casual)
'    		circle (newX+((1280)/2), newy+((720)/2)),2,rainbow'rgb(238*casual,127+3*casual,238*casual)'rg
   
rem  end if


'circle(newx+((1280)/2),newy+((720)/2)),1,rainbow'rgb(255,255,255)'255,251,0)'rgb(238*casual,127+3*casual,238*casual)'rgb(255*casual,251*casual,0*casual)    
'end if
'END IF
'end if
NEXT

end if
        ' if game2=4 or game2=6 or game2=11 then screenlock:cls:paint(1,1),rgb(127/2,127/2,127/2):screenunlock

'gameseven:
 zoomlevel=6.33
dim ggb as SINGLE
'dim ggbflag AS integer
'if game2<>1 and game2<>12 then
    if game2=3 or game2=5 or game2=10 OR GAME2=4 OR GAME2=6 OR GAME2=11 then
   ' paint(1,1),rgb(127,127,127)
    'dim greenplay as any ptr=imagecreate(1280,720)
    color rgb(255,255,255),rgb(0,0,0)
    if greenflag=0 then
        greenflag=1
        ggb=255
        'ggbflag=0
        for greeny=1 to (720)/2
           ggb=(255*(greeny*100/((720)/2)/100)) 
   ' for greeny=720/4 to (720)/2
        screenlock
        line(0,greeny)-(1280-1,greeny),rgb(0,255-ggb,255-ggb)'255*((greeny)*100/((720)/2)/100),0)
                line(0,720-greeny)-(1280-1,720-greeny),rgb(0,255-ggb,255-ggb)'255*((greeny)*100/((720)/2)/100),0)

          screenunlock
next greeny
'cls

'end if 
dim as integer markx,marky,greeny2
markx=128
marky=72
for marky=1 to 720 step +72
for markx=1 to 1280 step +128
    line(markx,0)-(markx,720),rgb(255,0,0)
    line(0,marky)-(1280,marky),rgb(255,0,0)
    'line(markx,marky)-(markx+128,marky+72),rgb(0,255,0)
    'line(markx,marky+72)-(markx+128,marky),rgb(0,255,0)
   ' markx=markx+128
   ' marky=marky+72
next
next
line(1,720)-(1,1),rgb(0,0,0),,&b1010101010101010
line(1279,720)-(1279,1),rgb(0,0,0),,&b1010101010101010


line(0,720/2)-(1280,720/2),rgb(0,0,0),,&b1010101010101010
                                                                            
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                
   ' for greeny=720/4 to (720)/2 'to 720 step +1
   '     screenlock
   '            line(1,720-greeny)-(1279,720-greeny),rgb(0,255*((greeny)*100/((720)/2)/100),0)
   '           screenunlock
   
' next greeny
'   paint(1,1),rgb(0,192,0)
get(0,0)-(1280-1,720-1),greenplay
greenflag=1
elseif greenflag=1 then
    screenlock
    put(0,0),greenplay,pset
    
   screenunlock
   if game2<>1 and game2<>12 and game2<>2 and game2<>4 and game2<>6 and game2<>11 and game2<>13 and (game2=3 or game2=5 or game2=10) then
    screenlock
    drawdraw(1280/2-(5*8*8/2),720/2-(8*8),"TOP:"+str(cup9top),rgb(255,255,0),8)
       drawdraw(1280/2-(9*8*8/2),720/2+(8*8/2),"BOTTOM:"+str(cup9bottom),rgb(255,255,0),8)
   screenunlock
'elseif game2=1 or game2=12 then
  
   end if
    end if
  end if
 ' if zcount=0 and (game2=0 or game2=2 or game2=5 or game2=6 or game2=11 or game2=13) then
      
'if game2>0 then goto ender
 if game2=0 and zcount=0 then
FOR pointIdx = 1 TO numberOfPoints step +2
    flatx=0:flaty=0
	'// Do not draw points that are behind the screen
	 IF MyArray(pointIdx).Z+zoomlevel > 0 and (myarray(pointidx).x+myarray(pointidx).y)>0 or (myarray(pointidx).x+myarray(pointidx).y)<0 THEN
    '    if flag=1 then jetflag=0
    'if jetflag=1 then
     '   flatx=airbornx(pointidx)
      '  flaty=airborny(pointidx)
       ' end if
   ' if jetflag=0 then
            'jetflag=1
		'// Translate points
       ' MyArray(pointIdx-1).X<>0
		flatX = MyArray(pointIdx).X * (1280*2.33) / (MyArray(pointIdx).Z + zoomLevel)
		flatY = MyArray(pointIdx).Y * (720*1) / (MyArray(pointIdx).Z + zoomLevel)
    '    airbornx(pointidx)=flatx:airborny(pointidx)=flaty
     '   if pointidx=numberofpoints then jetflag=1
   ' end if
    
       ' flat1 = MyArray(pointIdx-1).X * (1280) / (MyArray(pointIdx-1).Z + zoomLevel)
		'flat2 = MyArray(pointIdx-1).Y * (720) / (MyArray(pointIdx-1).Z + zoomLevel)
		'// Plot a white dot at the point's translated coordinates
       

' casual=(myarray(pointidx).x+zoomlevel*100/(1+zoomlevel)/100)
       ' if casual>1 then casual=1
 '  if myarray(pointidx).x<>0 and safe2x=0 then 
 '      safe2x=safe1x:safe2y=safe1y
 '      safe1x=flatx:safe1y=flaty
 '     '     line(safe1x+((1280)/2),safe1y+((720)/2))-(safe2x+((1280)/2),safe2y+((720)/2)),rgb(255*casual,255*casual,255*casual),bf
''       end if
'tie=2
'game2=2
if game2=4 or game2=6 or game2=11 or game2=2 then
        newX = COS(pieyedi*2-slphouse) * flatx - SIN(pieyedi*2-slphouse) * flaty'myarray(pointidx).y
        
 newY = SIN(pieyedi*2-slphouse) * flatx + COS(pieyedi*2-slphouse) * flaty'myarray(pointidx).y
 end if   
   ' game8=0                                                                                       'elseif tie=2 then
 'if game2=2 then
 '     newX = COS(pieyedi*2-slphouse) * flatx - SIN(slphouse-(pieyedi*2)) * flaty'myarray(pointidx).y
        
 'newY = SIN(pieyedi*2-slphouse) * flatx + COS(slphouse-(pieyedi*2)) * flaty'myarray(pointidx).y
    
     
     
     
     
     '  newX = COS(slphouse) * flatx - SIN(slphouse) * flaty'myarray(pointidx).y
if game2=0 Then
	slphouse=flat1
'EndIf
'game8=0
     newX = COS(pieyedi*2-slphouse) * flatx - SIN(pieyedi*2-slphouse) * flaty'myarray(pointidx).y
     newY = SIN(pieyedi*2-slphouse) * flatx'x + COS(slphouse) * flaty'myarray(pointidx).y 
    ' newy=flaty
end if
if   (game2=1 or game2=12) or game2=3 or game2=5 or game2=10  then
newx=flatx
newy=flaty

'game8=1
'if game2>0 then newy=flaty
'newY = SIN(slphouse) * flatx + COS(slphouse) * flaty
end if
'game2=0

 ' if game2=3 and game8=1 then
 '   newx=flatx
'newy=flaty
'end if
 
casual=0
alpx=myarray(pointidx).x
alpy=myarray(pointidx).y
alpz=myarray(pointidx).z

'if alpx<0 then casual=casual+alpx
'if alpx>0 then casual=casual-alpx
'if alpy<0 then casual=casual+alpy
'if alpy>0 then casual=casual-alpy
'alpx=alpx+1
'alpy=alpy+1
'newx=flatx'cos(slphouse)*flatx'-sin(slphouse)*flaty
'casual=1-alpx*sin(slphouse)'  - SIN((slphouse)) 'myarray(pointidx).y'*(alpy*sin(slphouse))'alpx*tan((pieyedi*2/20)-slphouse-(pieyedi/2))*.5
'newy=flaty
casual=alpx*cos(pieyedi*2-slphouse)'+(pieyedi*2/20))'-(pieyedi*2/20))
casual2=alpy*sin(pieyedi*2-slphouse)'+(pieyedi*2/20))
casual3=alpx*cos(pieyedi*2-slphouse-pieyedi)'*2/20))
casual4=alpy*sin(pieyedi*2-slphouse-pieyedi)
'-(pieyedi*2/20))
apx1=(casual+casual2)*100/1/100'*32'*100/2/100)+(casual
apy1=(casual3+casual4)*100/1/100'*32'*100/2/100)+(casual
casual=(apx1+apy1)*100/1/100
casual=apx1*4
'newx=alpx*(1280-shrinkx)/(alpz+zoomlevel)'/zoomlevel
'newy=alpy*(720-shrinky)/(alpz+zoomlevel)'/zoomlevel
'newx=newx*cos(pieyedi/4)

if game2<>0 then casual=1

'(alpy*2)/100*4/100*2
'casual=
'if alpx<0 then casual=1+alpx
'if alpx
'alpx2=myarray(pointidx).x
'if alpx<0 
'alphy=*cos(+(pieyedi*2/20)-slphouse-(pieyedi/2))
'alphx=myarray(pointidx).y*sin((pieyedi*2/20)-slphouse-(pieyedi/2))
'if slphouse>=pieyedi/4+(pieyedi*2/20)-(pieyedi/2) then  alphy2=myarray(pointidx).y*sin((pieyedi*2/20)-slphouse+pieyedi)
'alphyx=myarray(pointidx).x*cos(pieyedi*2+(pieyedi*2/20)-slphouse-(pieyedi/2))
'alphyy=myarray(pointidx).y*sin((pieyedi*2+pieyedi*2/20)-slphouse+pieyedi-(pieyedi/2))

'alphyx=myarray(pointidx).x*cos(pieyedi*2+(pieyedi*2/20)-slphouse)

'casual=1




'ELSE
'  alphy2=myarray(pointidx).y*sin(pieyedi*2+(pieyedi*2/20)-(PIEYEDI)-slphouse)
' END IF  
'alphyx=myarray(pointidx).x*cos(pieyedi*2+(pieyedi*2/20)-slphouse+pieyedi-(pieyedi/2))
 'alphyy=myarray(pointidx).y*sin(pieyedi*2+(pieyedi*2/20)-slphouse+pieyedi)
  'alphy2=myarray(pointidx).y
'alphy=alphy+1
' alphy2=alphy2+1
' alphyx=alphyx+1
' alphyy=alphyy+1
' omegy=.5
' if alphy>1 then 
     'casual=(1*(alphy*100/2/100))*(1*(alphyx*100/2/100))*2
    ' casual=(1*(alphy2*100/2/100))*(1*(alphyy*100/2/100))*2
     
         ' casual3=1*(alphyx*100/2/100)
         ' casual4=1*(alphyy*100/2/100)
          'casual=(casual+casual2)*100/2/100*1'.5'33333333
          'casual=casual-(.25/2)
                  '  if casual<=.49999999 then casual=casual-.25
 dim x as SINGLE
 
 'rernd2:
 x=rnd(timered)
 'if x=0 then goto rernd2
if x<.6 then casual=casual-.04'03333333
 if x>.5 then casual=casual+.04'03333333
'if (pieyedi*2/20)+slphouse'1*(pieyedi*2/20)-(slphouse-pieyedi/2)*100/(pieyedi*2/20-pieyedi/2)/100'(100/2/100)
          if casual>1 then casual=1
          if casual<0 then casual=0
         ' casual=casual*.5'33333333
          'casual=1-casual
          '*.5'*.03333333
          'casual3=casual3+casual4/2
'omega=casual+casual3
'casual=omega
         ' casual=(casual+casual2/2)
'          casual2=1-casual2
'omegy=casual+casual2/2
' elseif alphy<=1 then
'  alphy=alphy+1
'     casual=1*(alphy*100/2/100)
'     end if
 'casual=omegy 
       '	newx=flatx
       '     newy=flaty
'[flat1 = COS(slphouse) * safe2x - SIN(slphouse) * safe2y'flaty'myarray(pointidx).y
        
'flat2 = SIN(slphouse) * safe2x + COS(slphouse) * safe2y'flaty'myarray(pointidx).y
'           line(newx+((1280)/2),newy+((720)/2))-(flat1+((1280)/2),flat2+((720)/2)),rgb(255*casual,255*casual,255*casual),bf
'safe2x=0:safe2y=0

'end if

'casual=1-casual
'if myarray(pointidx).z<>0 then 
'else
        '        circle (newx+(136*6/2), newy+(136*6/2)),3, rgb(255*casual,255*casual,251*casual)
'zoomlevel=zoomlevel=-0.1
'end if
'if myarray(pointidx).y<0 then
'    casual=1
'    elseif myarray(pointidx).y>=0 then
'if slphouse>=pieyedi then

'tie=2
'if tie=1 then
rem casual=1-(myarray(pointidx).x*100/2/100)*sin(slphouse)'*(1-myarray(pointidx).y*100/1/100)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)
rem casual=casual+(1-(myarray(pointidx).y*100/2/100))/2*cos(slphouse)'*100/2/100'    end if
'elseif tie=2 then
'casual=1-(myarray(pointidx).x*100/(1*2)/100)*sin(slphouse-(pieyedi*2)+(pieyedi*2/20)'*(1-myarray(pointidx)y*100/1/100)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)
' casual=casual+(1-(myarray(pointidx).y*100/(1*2)/100)*cos((pieyedi*2)+(pieyedi*2/20)-slphouse))/2'-slphouse)  
'if slphouse>pieyedi-(pieyedi*2/20*2) then
' casual=1-(myarray(pointidx).x*100/(1*2)/100)*cos((pieyedi*2-pieyedi*2/20)-slphouse)-(1-(myarray(pointidx).y*100/(1*2)/100)*sin(slphouse-(pieyedi*2-pieyedi*2/20)))'*(1-myarray(pointidx)y*100/1/100)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)
' casual2=1-(myarray(pointidx).x*100/(1*2)/100)*sin((pieyedi*2-pieyedi*2/20)-slphouse)+(1-(myarray(pointidx).y*100/(1*2)/100)*sin(slphouse-(pieyedi*2-pieyedi*2/20)))'-slphouse)  
'casual=.5
'casual2=.5
'casual=(myarray(pointidx).x*100/1/100)+1



'if myarray(pointidx).x>=0 then casual=(myarray(pointidx).x*100/(1)/100)*cos(-(pieyedi*2/20)+slphouse)'+(pieyedi*2/20) ' casual2=casual+(1-(myarray(pointidx).y*100/2/100)*cos(slphouse-(pieyedi*2)-(pieyedi*2/20)))/2'-(pieyedi*2-pieyedi*2/20))'*(1-myarray(pointidx)y*100/1/100)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)
'casual=.677'2-(myarray(pointidx).x*100/(1)/100)'+1*sin(-(pieyedi*2/20)+slphouse))'+(pieyedi*2/20) ' casual2=casual+(1-(myarray(pointidx).y*100/2/100)*cos(slphouse-(pieyedi*2)-(pieyedi*2/20)))/2'-(pieyedi*2-pieyedi*2/20))'*(1-myarray(pointidx)y*100/1/100)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)

'casual2=2-(myarray(pointidx).y*100/(2)/100)/2*sin(-(pieyedi*2/20)+(pieyedi*2)-slphouse)

 'casual=casual+(1-(myarray(pointidx).y*100/(1*2)/100))*sin(slphouse)/2'-slphouse)     
   ' casual=2-myarray(pointidx).x+1*100/2/100*(slphouse*100/(pieyedi)/100)
'if slphouse<=pieyedi-(pieyedi*2/20) then casual=1-casual
'casual=casual+casual2/2'1-casual
'casual=casual+casual2/(1-(myarray(pointidx).z*100/(1*2)/100))
'casual=1
'end if
'casual=1-casual
' casual=(2-(myarray(pointidx).x+1)*100/2/100)*(2-(myarray(pointidx).y+1)*100/2/100)*(myarray(pointidx).z*100/1/100)*(slphouse*100/(pieyedi*2)/100)
 'if casual>1 then casual=1
  ' casual=casual/2'.5
'   if pieyedi*2-slphouse>pieyedi then 
'       casual= 1-(1*(slphouse*100/pieyedi/100))
'elseif pieyedi*2-slphouse<pieyedi then
'          casual= (1*(slphouse*100/pieyedi/100))
 
' if myarray(pointidx).z>0 then casual=casual*(1-myarray(pointidx).z)   
'end if
'if slphouse=pieyedi then casual=0
'if pointidx<(6*8*6)+1 then


'dim as SINGLE coil(4)
'coil(1)=pieyedi/7
'coil(2)=pieyedi/5
'coil(3)=pieyedi/3
'coil(4)=pieyedi
'if alphy<=(pieyedi*2+(pieyedi*2/20))-pieyedi/4 and alphy2<=(pieyedi*2+(pieyedi*2/20))-pieyedi/4 then'or slphouse-(pieyedi/2)>=0-(pieyedi/2)+pieyedi*2/20+pieyedi/4 then
   ' rainbow=rgb(0*casual,255*casual,255*casual)
'else
'    rainbow=rgb(238*casual,130*casual,238*casual)
'    end if
' rainbow=wave2rgb(400+(waverycalc-((waverycalc)*pointidx*100/numberofpoints/100)))
'if casual>.5 then 
   ' rainbow=rgb(int(255*rnd)+1,int(255*rnd)+1,0)'rgb(255,251,0)'RGB (208, 83, 64)'rgb(255,255,0)'153,101,21)'&h00d024'rgb(153, 101, 21)
'else
'if   (game2=1 or game2=12) then rainbow=wave2rgb(rainglow)

   ' casual=.5-casual
   ' casual=.5+casual
 '   rainbow=RGB(0,0,0)
  '  end if
'd = RGBA_R( rainbow )
''n = RGBA_G( rainbow )
'e = RGBA_B( rainbow )
'if game2>0 then
'rainbow=rgb(d*casual,n*casual,e*casual)
'casual=casual*1.33:if casual>1 then casual=1
'else
    rainbow=rgb(255*casual,255*casual,0)
'end if
if game2=4 or game2=6 or game2=11 then rainbow=rgb(255*casual,255*casual,251*casual)
if MyArray(pointIdx).X<0 and game2<>0 then rainbow=rgb(0,0,255*casual)
    if MyArray(pointIdx).X>0 and game2<>0 then rainbow=rgb(255*casual,0,0)
 screenlock
  tier=1
rem   if ((slphouse<(pieyedi*2)+(pieyedi*2/20)-(pieyedi/3)) and (slphouse>(pieyedi*2)+(pieyedi*2/20)-(pieyedi/2)-(pieyedi/3))) or ((slphouse<(pieyedi*2)+(pieyedi*2/20)-pieyedi-(pieyedi/3)) and (slphouse>(pieyedi*2)+(pieyedi*2/20)-pieyedi-(pieyedi/2)-(pieyedi/3))) then
 rem  tier=0   		
         rem     circle (newX+((1280)/2), newy+((720)/2)),3,rgb(0,0,0)'rainbow'rgb(255*casual,255*casual,0*casual)', rgb(d*casual,n*casual,e*casual)
                	'	circle (newX+((1280)/2), newy+((720)/2)),2, rgb(d*casual,n*casual,e*casual)
     	rem	circle (newX+((1280)/2), newy+((720)/2)),2,rgb(0,0,0)
rem  else
 rem   tier=1
 'flatx<>+:flaty=0
 'if newy+((720)/2)<=(720)/2 and game=0 then
 '   circle (newX+((1280)/2), newy+((720)/2)),3,rainbow'rgb(238*casual,127+3*casual,238*casual)'rainbow'rgb(255*casual,255*casual,0*casual)', rgb(d*casual,n*casual,e*casual)
                	'	circle (newX+((1280)/2), newy+((720)/2)),2, rgb(d*casual,n*casual,e*casual)
 '   		circle (newX+((1280)/2), newy+((720)/2)),2,rainbow'rgb(238*casual,127+3*casual,238*casual)'rg
   
rem  end if


'circle(newx+((1280)/2),newy+((720)/2)),1,rainbow'rgb(255,255,255)'255,251,0)'rgb(238*casual,127+3*casual,238*casual)'rgb(255*casual,251*casual,0*casual)
'else
'if newx<>(1280)/2 and newy<>(720)/2 then
circle (newX+((1280)/2), newy+((720)/2)),4,rainbow,,,,F'rgb(238*casual,127+3*casual,238*casual)'rainbow'rgb(255*casual,255*casual,0*casual)', rgb(d*casual,n*casual,e*casual)
                		'circle (newX+((1280)/2), newy+((720)/2)),2, rgb(d*casual,n*casual,e*casual)
 '   		circle (newX+((1280)/2), newy+((720)/2)),2,rainbow'rgb(238*casual,127+3*casual,238*casual)'rg
   
rem  end if


'circle(newx+((1280)/2),newy+((720)/2)),1,rainbow'rgb(255,255,255)'255,251,0)'rgb(238*casual,127+3*casual,238*casual)'rgb(255*casual,251*casual,0*casual)    
'end if
screenunlock

	END IF
NEXT
ender:
end if
'paint(hammerx,hammery-89),RGB(0,0,0),RGB(0,0,1)'rgb(255,255,255)
'paint(hammerx,hammery+89),RGB(0,0,0),RGB(0,0,1)'rgb(255,255,255)
'rgb(0,215,255)'
'end if
'xfive=1
   
'end if
'if game2>0 then goto gamethree
'if flat1=0 then flat1=pieyedi*2
'dim as integer rock1=720,rock2=360
'4.66'3.66'pieyedi'4.33'4.5'zoomlevel/pieyedi'2
xfive=135/1.5+(sin(flat2+pieyedi)*(135/1.5/3))

if game2=0 then 
   ' circle(((1280)/2)+cos(flat1)/pee,((720)/2)+sin(flat1)/pee),720/(pieyedi*2)*.66,RGB(255,255,255)

'paint(((1280)/2)+cos(flat1)/pee,((720)/2)+sin(flat1)/pee),rgb(255, 255,255),RGB(255,255,255)
'hammerx=((1280)/2)+(cos(flat1-(pieyedi*2/20))*(1280*.5*.5))
'hammery=((720)/2)+(sin(flat1-(pieyedi*2/20))*(720*.5*.5))
'circle(hammerx,hammery),180*.75,rgb(255,255,255),,,1-xfive,F
'paint(hammerx,hammery-89),rgb(255,255,255),rgb(255,255,254)
'paint(hammerx,hammery+89),rgb(255,255,255),rgb(255,255,254)
hammerx2=((1280)/2)+(cos(flat2+PIEYEDI)*(1280*.25*.75))
hammery2=((720)/2)+(sin(flat2+PIEYEDI)*(720*.25*.75))
dim as double pieye=pieyedi/(xfive)*1
dim as single radnum=(xfive)*1,radcount
dim as single radcountlineout=radnum+1,radcountlinein=radnum
dim as single a,x,y,x1,y1
'screenlock
For a = 0 To 360 step +.25
        x = Cos(a) * radnum
        y = Sin(a) * radnum
       ' x1=cos(a) * radcountlinein
       ' y1=sin(a) * radcountlinein
        screenlock

        PSet (hammerx2 + x, hammery2 + y),rgb(192,192,192)'point(hammerx2+x,hammery2+y)
    screenunlock

Next
'screenlock
'paint(hammerx2,hammery2),rgb(192,192,192),rgb(192,192,192)
'circle(hammerx2,hammery2),(xfive)*1,RGB(0,0,0)',,,,F'VIOLET'rgb(255,255,255)
'screenunlock
for radcount=radcountlineout to (xfive*2)
radcountlinein=radcountlinein-1
 For a = 0 To 360 step +.25'33'1'.5'3333333'25
        x = Cos(a) * radcount
        y = Sin(a) * radcount
        x1=cos(a) * radcountlinein
        y1=sin(a) * radcountlinein
        screenlock

        PSet (hammerx2 + x1, hammery2 + y1),point(hammerx2+x,hammery2+y)
    screenunlock

Next
next
'screenunlock
'flip\
end if
'xfive=(1.33/2*(flat1+(pieyedi/2)*100/pieyedi/100))
'xfive=(1.5/2*(flat1+pieyedi*100/(pieyedi*2)/100))
xfive=135/1.5+(sin(flat2)*(135/1.5/3))

if game2=0 then 
   ' circle(((1280)/2)+cos(flat1)/pee,((720)/2)+sin(flat1)/pee),720/(pieyedi*2)*.66,RGB(255,255,255)

'paint(((1280)/2)+cos(flat1)/pee,((720)/2)+sin(flat1)/pee),rgb(255, 255,255),RGB(255,255,255)
hammerx2=((1280)/2)+(cos(flat2)*(1280*.25*.75))
hammery2=((720)/2)+(sin(flat2)*(720*.25*.75))
'screenlock
'circle(hammerx2,hammery2),(xfive)*1,rgb(255,255,255),,,,F
'screenunlock
dim as double pieye=pieyedi/(xfive)*1
dim as single radnum=(xfive)*1,radcount
dim as single radcountlineout=radnum+1,radcountlinein=radnum
dim as single a,x,y,x1,y1
'screenlock
For a = 0 To 360 step +.25
        x = Cos(a) * radnum
        y = Sin(a) * radnum
       ' x1=cos(a) * radcountlinein
       ' y1=sin(a) * radcountlinein
        screenlock

        PSet (hammerx2 + x, hammery2 + y),rgb(192,192,192)'point(hammerx2+x,hammery2+y)
    screenunlock

Next
'screenlock
'paint(hammerx2,hammery2),rgb(192,192,192),rgb(192,192,192)
'circle(hammerx2,hammery2),(xfive)*1,RGB(0,0,0)',,,,F'VIOLET'rgb(255,255,255)
'screenunlock
for radcount=radcountlineout to (xfive*2)
radcountlinein=radcountlinein-1
 For a = 0 To 360 step +.25'.5'3333333'25
        x = Cos(a) * radcount
        y = Sin(a) * radcount
        x1=cos(a) * radcountlinein
        y1=sin(a) * radcountlinein
        screenlock

        PSet (hammerx2 + x1, hammery2 + y1),point(hammerx2+x,hammery2+y)
    screenunlock

Next
next

'paint(hammerx,hammery-89),rgb(255,255,255),rgb(255,255,254)
'paint(hammerx,hammery+89),rgb(255,255,255),rgb(255,255,254)
'hammerx=((1280)/2)+(cos(flat1+PIEYEDI-(pieyedi*2/20))*(1280*.5*.5))
'hammery=((720)/2)+(sin(flat1+PIEYEDI-(pieyedi*2/20))*(720*.5*.5))
'circle(hammerx,hammery),180*.75,RGB(0,0,0),,,xfive,F'VIOLET'rgb(255,255,255)
'paint(hammerx,hammery-89),RGB(0,0,0),RGB(0,0,1)'rgb(255,255,255)
'paint(hammerx,hammery+89),RGB(0,0,0),RGB(0,0,1)'rgb(255,255,255)
'rgb(0,215,255)'
end if
gamethree:
if game2=0 then
    flat1=flat1+(pieyedi*2/(360/2))

if flat1>pieyedi*2 then flat1=0+(pieyedi*2/(360/2))
end if
if game2=1 or game2=12 or game2=2 or game2=13 then
    flat1=flat1+(pieyedi*2/(360/24))

if flat1>pieyedi*2 then flat1=0+(pieyedi*2/(360/24))
end if

'then flat1=0'-(pieyedi*2/20)'pieyedi*2
flat2=pieyedi*2-flat1

'if game2>0 then goto gamefour
'if gonersflag=0 then gonersx=gonersx:gonersy=gonersy-.5
'if gonersflag=1 then gonersx=gonersx:gonersy=gonersy+.5

'straight

    
'  pset(1280-straightx,straighty),point(straightx,straighty)
'next
'next

'end if
'tie=1
gamefour:

      ' if tie=2 then
    '  if modnar=1 then
slphouse=slphouse+(pieyedi*2)
slphouse2=slphouse2+(pieyedi*2/(720*32))

'else
'  slphouse=slphouse-(pieyedi*2)
'  end if
  
'elseif tie=1 then
'  slphouse=slphouse+(pieyedi*2)
'end if
'pieyedi*2
'jump3d:
'if game2=4 then
'line((1280)/2,1+(6*8))-((1280)/2,720),rgb(0,0,0)
'line(1,(720)/2)-(1280,(720)/2),rgb(0,0,0)
'end if
 
 
 
 
 
 
 

'stp=stp+.14159265359
'meta=2*pieyedi
'if counts=3 then
'goto star2
   ' counts2=0
'    counts=0
'    tmeout=timer+1
'end if
'do until meta>5'gammy>(pieyedi*2)'322+gammy'(.14159265359*2/12)
   ' if tmeout<timer then
'meta=pieyedi*2/20
'tmeout=timer+1
'else
'end if

'randomize timer
'paint(0,0),waver3,wave2rgb(400+shave)
'drawbox((1280)/2,(720)/2,wavermid)
'Draw String ((1280)/2-3,60+80+30-3,wavermid), chr(64+cc1d,wavermid),waver3
'goto jerkout
'drawbox((1280)/2-(26*5*1.750),(720)/2,wavermid)
''Draw String ((1280)/2,120+80,wavermid), chr(64+cc1d,wavermid),waver3, trans
'Draw String ((1280/6.67,wavermid)+80+30+9-3,720024/4.25-80-15-3), chr(64+cc1d),waver3
'drawbox((1280)/2+(26*5*1.333),(720)/2-(26*5*1.333),wavermid)
'Draw String (1280-(1280/6.67)-80-30-9-3,720024/4.25-80-15-3), chr(64+cc1d),waver3
'drawbox((1280)/2+(26*5*1.333),(720)/2+(26*5*1.333),wavermid)
'Draw String ((1280)/2-3,720-(60+80+30)-3), chr(64+cc1d),waver3
'drawbox((1280)/2+(26*5*1.750),(720)/2,wavermid)
'Draw String ((1280/6.67)+80+30+9-3,720/4.25+80+15-3), chr(64+cc1d),waver3
'drawbox((1280)/2-(26*5*1.333),(720)/2+(26*5*1.393))
'Draw String (1280-(1280/6.67)-80-30-9-3,720/4.25+80+15-3), chr(64+cc1d),waver3
'drawbox((1280)/2-(26*5*1.393),(720)/2-(26*5*1.333),wavermid)

'Draw String  (-3+((1280)/2)-(1280/6.67+80)/2-40,-3+((720)/2)-(720/4.25+80)/2-100),chr(64+cc1d),waver3
'drawbox((1280)/2-(26*5*1.393),(720)/2+(26*5*1.333),wavermid)

'Draw String  (-3+(1280)-(((1280)/2)-(1280/6.67+80)/2-40),-3+(720)-(((720)/2)-(720/4.25+80)/2-100)),chr(64+cc1d),waver3
'drawbox((1280)/2,(720)/2-(26*5*1.777),wavermid)
'Draw String  (-3+(1280)-(((1280)/2)-(1280/6.67+80)/2-40),-3+(((720)/2)-(720/4.25+80)/2-100)),chr(64+cc1d),waver3
'drawbox((1280)/2,(720)/2+(26*5*1.777),wavermid)
'Draw String  (-3+(((1280)/2)-(1280/6.67+80)/2-40),-3+(720)-(((720)/2)-(720/4.25+80)/2-100)),chr(64+cc1d),waver3
'drawbox((1280)/2+(26*5*2*1.222),(720)/2+4-(26*5/1.390),wavermid)
'Draw String  (-3+((1280)/2)-(1280/6.67+80)/2-100-(26*2.5),-3+((720)/2)-(720/4.25+80)/2-100-(26*2.5)),chr(64+cc1d),waver3
'drawbox((1280)/2+4+(26*5*2*1.222),(720)/2+(26*5/1.390),wavermid)
'Draw String  (-3+(1280)-(((1280)/2)-(1280/6.67+80)/2-100-(26*2.5)),-3+((720)/2)-(720/4.25+80)/2-100-(26*2.5)),chr(64+cc1d),waver3

'drawbox((1280)/2-4-(26*5*2*1.222),(720)/2-(26*5/1.390),wavermid)
'Draw String (-3+((1280)/2)-(1280/6.67+80)/2-100-(26*2.5),-3+(720)-(((720)/2)-(720/4.25+80)/2-100-(26*2.5),wavermid)),chr(64+cc1d),waver3
'drawbox((1280)/2-4-(26*5*2*1.222),(720)/2+(26*5/1.390),wavermid)
'Draw String (-3+1280-(((1280)/2)-(1280/6.67+80)/2-100-(26*2.5),wavermid),-3+(720)-(((720)/2)-(720/4.25+80)/2-100-(26*2.5),wavermid)),chr(64+cc1d),waver3
'drawbox((1280)/2-(26*5/1.111),(720)/2-(26*5*2*1.222),wavermid)
'Draw String (-3+((1280)/2)-(1280/6.67+80)/2,-3+((720)/2)-(720/4.25+80)/2-240),chr(64+cc1d),waver3
'drawbox((1280)/2+(26*5/1.111),(720)/2+(26*5*2*1.222),wavermid)
'Draw String (-3+1280-(((1280)/2)-(1280/6.67+80)/2),-3+((720)/2)-(720/4.25+80)/2-240),chr(64+cc1d),waver3

'drawbox((1280)/2+(26*5/1.111),(720)/2-(26*5*2*1.222),wavermid)
'Draw String (-3+(((1280)/2)-(1280/6.67+80)/2),-3+720-(((720)/2)-(720/4.25+80)/2-240),wavermid),chr(64+cc1d),waver3
'drawbox((1280)/2-(26*5/1.111),(720)/2+(26*5*2*1.222),wavermid)
'Draw String(-3+(1280)/2-340,-3+(720)/2),chr(64+cc1d),waver3
'drawbox((1280)/2-(26*5/1.667)-(26*5*2*.900),(720)/2+(26*5*1.1023),wavermid)
''Draw String(-3+1280-((1280)/2-340),-3+(720)/2),chr(64+cc1d),waver3
'drawbox((1280)/2-(26*5/1.667)-(26*5*2*.900),(720)/2-(26*5*1.1023),wavermid)
''Draw String(-3+(1280)/2-460,-3+(720)/2-(26*2.5+25),wavermid),chr(64+cc1d),waver3
'drawbox((1280)/2+(26*5/1.667)+(26*5*2*.900),(720)/2+(26*5*1.1023),wavermid)
''Draw String(-3+1280-((1280)/2-460),-3+(720)/2-(26*2.5+25),wavermid),chr(64+cc1d),waver3
'drawbox((1280)/2+(26*5/1.667)+(26*5*2*.900),(720)/2-(26*5*1.1023),wavermid)
'Draw String(-3+(1280)/2-460,-3+720-((720)/2-(26*2.5+25),wavermid)),chr(64+cc1d),waver3
'drawbox((1280)/2,(720)/2-((26*5*3.111),wavermid))
'draw string(-3+1280-((1280)/2-460),-3+720-((720)/2-(26*2.5+25),wavermid)),chr(64+cc1d),waver3
'drawbox((1280)/2,(720)/2+((26*5*3.111),wavermid) )

'drawbox((1280)/2-(26*5*3.111),(720)/2)
'Draw String (-3+1280-(((1280)/2)-(1280/6.67+80)/2),-3+720-(((720)/2)-(720/4.25+80)/2-240),wavermid),chr(64+cc1d),waver3
'drawbox((1280)/2+(26*5*3.111),(720)/2)
'drawbox((1280)/2,(720)/2+(26*5*3),wavermid)
'Draw String(-3+1280-((1280)/2-340),-3+(720)/2),chr(64+cc1d),waver3
'drawbox((1280)/2,(720)/2-(26*5*3),wavermid)
'Draw String(-3+(1280)/2-460,-3+(720)/2-(26*2.5+25),wavermid),chr(64+cc1d),waver3
'drawbox((1280)/2-4-(26.5*5*3.222),(720)/2,wavermid)
'Draw String(-3+1280-((1280)/2-460),-3+(720)/2-(26*2.5+25),wavermid),chr(64+cc1d),waver3
'drawbox((1280)/2+4+(26.5*5*3.222),(720)/2,wavermid)

'line(1,720/3)-(1280,720/3),waver4
'line(1280,720/3)-((1280)/2,720),wave2rgb(400+shave)
'line((1280)/2,720)-(1,720/3),wave2rgb(400+shave)
'line(1,720-(720/3))-(1280,720-(720/3)),wave2rgb(400+shave)
'line(1280,720-(720/3))-((1280)/2,1),wave2rgb(400+shave)
'line((1280)/2,1)-(1,720-(720/3)),wave2rgb(400+shave)
'paint((1280)/2,(720)/2-25),waver4,waver3
'paint((1280)/2,(720)/2+25),waver4,waver3'

















'randomize timer+(255/(int(rnd(1)*511)+2))



rem if game2=4 then helloirene

'end if
'end if
'clifford=0

'screenres










' if savex=0 and savey=0 then
'   savex=xxxx
'   savey=yyyy
'   save2x=savex
'   save2y=savey
   
'end if
'line(xxxx,yyyy)-(savex,savey),colr2
'if savex=0 and savey=0 then
'savex=xxxx

'savey=yyyy
'end if
rem if ((xxxx>(1280)/2-(16*8) and xxxx<(1280)/2+(16*8)) or (yyyy>(720)/2-(16*8) and yyyy<(720)/2+(16*8))) then
rem    circle(xxxx,yyyy),2,rgb(0,0,0)
rem    else
'if (xxxx<(1280)/2-((8*13)*(radian*2*100/(pieyedi*2)/100)) or xxxx>(1280)/2+((8*13)*(radian*2*100/(pieyedi*2)/100))) and (yyyy<(720)/2-((8*13)*(radian*2*100/(pieyedi*2)/100)) or yyyy>(720)/2+((8*13)*(radian*2*100/(pieyedi*2)/100))) then



rem end if


'for cccc=1 to 26
     ' circle (xxxx, yyyy),(27-1)*1,cow2(cccc)'2.5, cow2(cccc)
     ' paint(xxxx,yyyy),wavepool,cow2(cccc)
'next cccc


rem circle(xxxx,yyyy),2,rgb(255/3*bright, 7/3*bright, 7/3*bright)
'savey=yyyy:savex=xxxx
'draw string(-3+xxxx,-3+yyyy),chr(64+charpos),colr'waver3
'sleep
'if ((xxxx>(1280)/2-(16*8) and xxxx<(1280)/2+(16*8)) or (yyyy>(720)/2-(16*8) and yyyy<(720)/2+(16*8))) then

'theta=theta+((3.14159265359*2/(360)))    ' add step to theta
'else
 'theta=theta+((3.14159265359*2/(360*15)))    ' add step to theta
' end if
 'gammy=gammy+((3.14159265359*2)/5)    ' add step to theta

'meta=meta+1

' points=points+points   '}
'loop
'if cccc=1 then paint(1,1),waver3,waver3
'if cccc=3 then paint(1,1),rgb(d,n,e),rgb(d,n,e)
 'screencopy
'circ:
'screencopy
'rad=279/2+(720)/2-75+32
'line(0,(720)/2+(rad*.831))-(1280,(720)/2+(rad*.831)),flagger
'line((1280)/2+(rad*.935),0)-((1280)/2+(rad*.935),720),flagger
'line(0,(720)/2-(rad*.831))-(1280,(720)/2-(rad*.831)),flagger
'line((1280)/2-(rad*.935),0)-((1280)/2-(rad*.935),720),flagger

'goto peanutbutter






'peanutbutter:





'if cccc=2 then

'else
'draw string ((1280)/2-(13*8)+(ccccc*8-8),(720)/2-(26*5)),chr(ccccc+64),wavermid'rgb(7,7,255)'waver2

' end if
'thirdjump:
'thirdjump:
'if cccc=1 then  draw string ((1280)/2-(3.5*8),(720)/2-3-10-8),"JEHOVAH",wavermid'rgb(7,7,255)'waver2
 
  ' draw string ((1280)/2-(3.5*8),(720)/2-3-10-8),"JEHOVAH",waversave'rgb(7,7,255)'waver2
'rgb(7,7,255)'waver2


'screencopy
'if flag=2 then
'if cccc=1 then sound pulsewave((220*2*(1.05946309436*cc1d))),3/(2*24) else sound pulsewave((220*2*(1.05946309436*(27-cc1d)))),3/(2*24)

rem code below for sound
'skipper9:
'DIM PULSECALC as SINGLE
'    dim safe as SINGLE

     ' sound PULSEwave(notes(14)-NOTES(13)/2+NOTES(13)),3/(2*24)

'cccc=1


'if cc1d>(27-cc1d) then sound pulsewave(notes(cc1d-(27-cc1d)/2)),3/(2*24)
'if cc1d<(27-cc1d) then sound pulsewave(notes((27-cc1d)-cc1d/2)),3/(2*24)
'end if
'sleep 3/(2*12),1
'screenset 1,0
'paint(0,0),rgb(0,0,0),rgb(238,130,238)
'sleep (1.5/2,1)
'cls
'screencopy
'screenset 0,1
'cls
'screenset 1,0

'if paintflag=0 then
'    paintflag=1
'else
'    paintflag=0
'end if
'if flag=1 then paintflag=0:exit for

'ender:
'next cccc

'    counter_begin(1,1,1)













'circle((1280)/2,(720)/2),(1280)/2-(16+50),waver3,0,1*3.14159265359
'circle((1280)/2,(720)/2),279/2+(720)/2-90,rgb(d,n,e),,,.89
'paint((1280)/2-(279/2+(720)/2-90-3),(720)/2),waver3,waver3
'screencopy
'next cccc
'k=inkey
'if k=chr(27) then end
'screencopy 1,0
'wavefreq=wavefreq-1
'if wavefreq<1 then wavefreq=26'650
'next corky

'dim gg as integer

'for gg=1 to 26
'    line(1,gg)-(100,gg),wave2rgb((650)-(waverycalc*(gg*100/26/100)))

'screencopy 1,0
'sleep
'next gg









'circle((1280-66)/3+33,720/4.25),4,waver3
'paint((1280-66)/3+33,720/4.25),waver3
'circle((1280-66)/3*2+33,720/4.25),4,waver3
'paint((1280-66)/3*2+33,720/4.25),waver3''''

'circle((1280-66)/3+33-15,720024/4.25-33),4,waver3
'paint((1280-66)/3+33-15,720024/4.25-33),waver3
'circle((1280)-(1280-66)/3-15,720024/4.25-33),4,waver3
'paint((1280)-(1280-66)/3-15,720024/4.25-33),waver3
'circle((1280-66)/3+33,720024/4.25),4,waver3
'paint((1280-66)/3+33,720024/4.25),waver3
'circle(1280-(1280-66)/3+33,720024/4.25),4,waver3
'paint(1280-(1280-66)/3+33,720024/4.25),waver3
'' Here we draw a string using the custom font
'Draw String (10, 10), "ABCDEFGHIJKLMNOPQRSTUVWXYZ", , myFont




'circle ((1280)/2-4, (720)/2-4),21, rgb(255,7,7)'rgb(d,n,e)
'paint ((1280)/2-4, (720)/2-4), rgb(238,130,238),rgb(255,7,7)'rgb(d,n,e)

'Draw String ((1280)/2-(26*8/2)+(tt22*8-8), (720)/2+1), chr(64+tt22),rgb(d,n,e)
'Draw String ((1280)/2-6, (720)/2-6), chr(64+tt22),waver3'rgb(d,n,e)















'Draw String ((1280)/2-4+1, (720)/2-9), chr(64+cc1d),waver3


 '   print mid(bbb,t2,1)
'next tt22
'line ((1280)/2,50)-(50,(720)/2),rgb(255,255,255)
'line (50,(720)/2)-(1280-50,(720)/2),rgb(255,255,255)
'line (1280-50,(720)/2)-((1280)/2,50),rgb(255,255,255)
'dim as integer redd(26),greenn(26),bluee(26),alph(26)
    'count_cycles=0
'    screencopy
'sleep (25,1)
'next cccc


   ' end if
'line ((1280)/2,50)-(50,(720)/2),rgb(255,255,255)
'line (50,(720)/2)-(1280-50,(720)/2),rgb(255,255,255)
'line (1280-50,(720)/2)-((1280)/2,50),rgb(255,255,255)

'if drawcount=0 then drawcount=int(1280/8)+len(aaa))
       ' locate 4,1280/8/2-(26/2)+t2
'for t2=1 to 26
'    pset(t2*8,4*8+9),rgb(0,0,0)
'next t2
'waver2=percent2rgb(100*(cc1d*100/26/100))

'waver=percent2rgb(100*(cc1d*100/26/100))
'waver=
'waver=wave2rgb(400)
'paint((1280)/2,(720)/2-50),waver3,rgb(255,255,255)'wave2rgb((720)-(waverycalc*(cc1d*100/26/100))),rgb(255,255,255)
'locate 1,1:print "  ";tt4;"  "
'line ((1280)/2,50)-(50,(720)/2),waver3'rgb(0,0,0)
'line (50,(720)/2)-(1280-50,(720)/2),waver3'rgb(0,0,0)
'line (1280-50,(720)/2)-((1280)/2,50),waver3'rgb(0,0,0)
'line(0,8)-(1280,8),waver3
'line(0,1)-(1022/2-4,1),waver3
'line(1022/2+4,1)-(1022,1),waver3
'line(0,720)-(0,0),waver3
'line(1,720)-(1,0),waver3

'line(0,(720)/2)-(1280,(720)/2),waver3
'line(1280,720)-(1280,0),waver3
'line(1022,720)-(1022,0),waver3

'paint((1280)/2,720/3+70),waver3,waver3
'line(1,0)-(1280,0),waver
'line((1280)/2-(26*8/2)-2,0)-((1280)/2-(26*8/2)-2,8+1),waver3
'line((1280)/2+(26*8/2),0)-((1280)/2+(26*8/2),8+1),waver3
'paint(4,4),waver3,waver3
'paint(1280-4,4),waver3,waver3
'asm
 '   jmp continue
  '  end asm
  'locate 1,1:print mid(aaa,tt,1)
'Draw String ((1280)/2-(26*8/2)+(cc1d*8-8), (720)/2), chr(64+cc1d),waver3


'    end asm
'screencopy 1,0
'    next tt3
'    drawline(0, 0, i, H_, Int(Rnd * &HFFFFFF))
'dim as SINGLE tt33,tt44,tt55,t66,t77,t88
'tt33=(1280)/2
'tt44=(720)/2
'tt33=tt33/2
'tt44=tt44/2
'line(tt33,tt44)-(1280-tt33,(720)/2-tt44),rgb(0,0,0)
'line(tt33,tt44)-((1280)/2+50,(720)/2),rgb(0,0,0)
'line(1280-tt33,(720)/2-tt44)-(1280-tt33,(720)/2-tt44),rgb(0,0,0)
   'if reed=ree and greeen=gre and euln=blu then exit for
'tmeend=timer
'locate 2,2:print str(tmeend-tmebegin)+"/fps:"+str(tt4)
'ext countit
'ocate 1,1:print tt4

'counter_end()
'sleep:cls
if helpflag=0 then k=inkey

if k=chr(8) and game2>0 and flag=0 then
    fsound_stream_stop(stream)
    FMUSIC_StopAllSongs
'game2=0
    'mutexlock mutex
    game=0
    ''mutexunlock mutex
    cup9top=0
    cup9bottom=0
    'starflag=0
    k=""
zcount=0
pressed=0
pressaflg=0
end if
GetJoystick(0,buttons,xxx,yyy,xxx2,yyy2)
GetJoystick(0,buttons2,xxx,yyy,xxx2,yyy2)
IF (BUTTONS=1 OR BUTTONS2=1) AND HELPFLAG=101 THEN K="L"
if helpflag>0 and(buttons=512 or buttons2=512 or lcase(k)="h") then 
   buttons=0
   buttons2=0
   k=""
   sleep 1333,1
   buttons=0
   buttons2=0
   k=""
       '     k="h"':exit if
    'else
    '    helpflag=0
        k=""
            screenset 1,0
'timeflag=helpflag-1
     'if k<>"h" then   k=""':exit if':sleep 1000,1
'if button=512 or buttons2=512
helpflag=0
end if

if (buttons=512 or buttons2=512 or lcase(k)="h") and helpflag=0 then helpflag=1:buttons=0:buttons2=0:k="":sleep 1333,1:k="":buttons2=0:buttons=0
'buttons=0:buttons2=0
'    carleywarnings:
'    k=inkey
'    GetJoystick(0,buttons,xxx,yyy,xxx2,yyy2)
'GetJoystick(0,buttons2,xxx,yyy,xxx2,yyy2)
'    if k<>"p" and k<>"P" and buttons<>512 and buttons2<>512 then sleep 50,1:goto carleywarnings
'    k="h"
'end if

    
'    sleep
'if k="q" or k="Q" then k=chr(27)

if k=chr(27) and flag=0 then 
    flag=1
'        'mutexlock mutex

'game=3

'''mutexunlock mutex
shrinkx=2000'1280
shrinky=2000'720
k=""
end if
if k=CHR(255) then
     kill("scores.dat"):sleep 1000
             open "scores.dat" for OUTPUT as #1

    for score=1 to 10
        print #1,starname(score)
        print #1,starscore(score)
    next score
    for score=1 to 10
        print #1,pongname(score)
        print #1,pongscore(score)
    next score
        print #1,timeflag
print #1,tiesaucer:print #1,vol255:print #1,muting:print #1,fps:Print #1,fullscreentoggle
    close #1
    FSOUND_Stream_Close(stream)
        FSOUND_Stream_Close(stream2)
    FSOUND_Stream_Close(stream3)
    FSOUND_Stream_Close(stream4)

       FSOUND_Close()
    imagedestroy(greenplay):imagedestroy(skyblue)
    ImageDestroy(myImage):ImageDestroy(mirrored):ImageDestroy(pic):mutexdestroy mutex:end
end if
           if flag=1 then
                kill("scores.dat"):sleep 1000
             open "scores.dat" for OUTPUT as #1

    for score=1 to 10
        print #1,starname(score)
        print #1,starscore(score)
    next score
    for score=1 to 10
        print #1,pongname(score)
        print #1,pongscore(score)
    next score
        print #1,timeflag
print #1,tiesaucer:print #1,vol255:print #1,muting
print #1,fps:Print #1,fullscreentoggle
    close #1
               FSOUND_Stream_Close(stream)
                FSOUND_Stream_Close(stream2)
    FSOUND_Stream_Close(stream3)
    FSOUND_Stream_Close(stream4)

       FSOUND_Close()
               imagedestroy(greenplay):imagedestroy(skyblue):ImageDestroy(myImage):ImageDestroy(mirrored):ImageDestroy(pic):mutexdestroy mutex:end 'imagedestroy picture:end
end if
'if k="h" or k="H" or k="p" or k="P" then
'sleep 500,1
'end if

if helpflag=1 then
   ' if helpflag=0 and lcase(k)="h" then helpflag=timeflag+1:timeflag=0:k="":sleep 1000,1
    zcount=0
    screenset 2,0
    cls
    color rgb(255,255,255),rgb(0,0,0)
    print
    print
    print
    print'"      Ping Pong Tennis only the left joystick of the gamepad is used (on normal logitech gamepad)"
    print
    print"      Use left stick up/down and press the 1 key to select (or A key same thing on other logitechs)"
    print'"      Press H key or any gamepad right start button in the middle of the logitech pad to continue........"
    print'"      Press the following keys to operate......."
    print
    print
    if choice=1 then     color rgb(0,0,0),rgb(255,255,0)

    print"      H or right start button in middle of logitech gamepad for pausing game/help screen"
        color rgb(255,255,255),rgb(0,0,0)

    print
        if choice=2 then     color rgb(0,0,0),rgb(255,255,0)

    print"      T for toggle the display of the time and date during gameplay (Default is off):";
                color rgb(255,255,255),rgb(0,0,0)

    if timeflag=0 then print"SET AT OFF" else print"SET AT ON":end if

    
    print
        if choice=3 then     color rgb(0,0,0),rgb(255,255,0)

    print"      1 for one or two player game Starship Shooters (one player game with only one attached gamepad)"
    print
            color rgb(255,255,255),rgb(0,0,0)
        if choice=4 then     color rgb(0,0,0),rgb(255,255,0)

    print"      2 for two player game only of Ping Pong Tennis"
        color rgb(255,255,255),rgb(0,0,0)

print
        if choice=5 then     color rgb(0,0,0),rgb(255,255,0)

print"      3 for one player game only of Ping Pong Tennis"
        color rgb(255,255,255),rgb(0,0,0)

print
        if choice=6 then     color rgb(0,0,0),rgb(255,255,0)

print"      4 for CPU vs CPU game of Ping Pong Tennis"
        color rgb(255,255,255),rgb(0,0,0)

print
        if choice=7 then     color rgb(0,0,0),rgb(255,255,0)

print"      5 for cpu vs cpu game of Starship Shooters"
        color rgb(255,255,255),rgb(0,0,0)

print
        if choice=8 then     color rgb(0,0,0),rgb(255,255,0)

print"      S for Toggle Starship shooters TieFighter or Flying Saucer mode:";
        color rgb(255,255,255),rgb(0,0,0)

if tiesaucer=0 then print"TIEFIGHTER MODE" else print"FLYING SAUCER MODE":end if

print
        if choice=9 then     color rgb(0,0,0),rgb(255,255,0)

print"      V for toggle sound on/off:";
        color rgb(255,255,255),rgb(0,0,0)

if vol255=0 then print"OFF" else print"ON":end if

print
        if choice=10 then     color rgb(0,0,0),rgb(255,255,0)

print"      C for change music mod file song"
        color rgb(255,255,255),rgb(0,0,0)

print
        if choice=11 then     color rgb(0,0,0),rgb(255,255,0)

print"      M for muting music on/off:";
        color rgb(255,255,255),rgb(0,0,0)

if muting=0 then print"OFF" else print"ON":end if

print
        if choice=12 then     color rgb(0,0,0),rgb(255,255,0)

print"      R for resetting software settings and TOP TEN Scores"
        color rgb(255,255,255),rgb(0,0,0)

print
        if choice=13 then     color rgb(0,0,0),rgb(255,255,0)


print"      L for list top ten high scores for starship shooters and ping pong tennis"
        color rgb(255,255,255),rgb(0,0,0)

print
        if choice=14 then     color rgb(0,0,0),rgb(255,255,0)

print"      Backspace to return to screen saver mode (3D Graphics and UFO and current time and date etc.)"
        color rgb(255,255,255),rgb(0,0,0)
print
if choice=15 then color rgb(0,0,0),rgb(255,255,0)

print"      F for frame rate display per second (FPS):";

color rgb(255,255,255),rgb(0,0,0)
if fps=1 then print"ON" else print"OFF":end If
Print
if choice=16 then color rgb(0,0,0),rgb(255,255,0)

print"      N for full screen or windowed screen toggle:";

color rgb(255,255,255),rgb(0,0,0)
if fullscreentoggle=0 then print"FULLSCREEN" else print"WINDOWED":end If
print

        if choice=17 then     color rgb(0,0,0),rgb(255,255,0)

print"      Esc to exit software program"
        color rgb(255,255,255),rgb(0,0,0)



print
'  ' print"      Spacebar will suddenly terminate this software program anywhere in the software program (even here maybe)"
    print
    print"      Feel free to change options during help screen by pressing keystroke explained on this help screen"
    print
    print"      Starship Shooters both gamepad joysticks are used and front TOP buttons fire (on normal logitech gamepad)"
    print"      The left stick controls craft direction while the up/down motion with right stick moves craft higher/lower"
    print
    print
    print"      Ping Pong Tennis only the left joystick of the gamepad is used (on normal logitech gamepad)"
    print
    print
    print"      Press and H key or any gamepad right start button in the middle of the logitech gamepad to continue........"
   'if (game2>0 and timeflag=1) or game2=0 then
    screenlock
'for straightx=1 to (1280)/2:for straighty=1 to 720 'to 1 step -1
  put((1280)/2-((21*8*6)/2),720-200),pic,ALPHA,INT(255/2)'setalpha
  if val(left(time,2))<12 then
      draw string ((1280)/2-24,(720-200)+(8*6/3-4)),"AM",rgb(255,255,0)
  else
          draw string ((1280)/2-24,(720-200)+(8*6/3*2-4)),"PM",rgb(255,255,0)
  
      end if
      screenunlock
  'end if

   screencopy
 '   sleep 500/4,1
    'sleep
   ' dim timekeeper as single
   ' timekeeper=timer'+1.5'.75
   ' do
  
   ' sleeper:
    sleep 25,1
    DIM DULL AS SINGLE
    GetJoystick(0,buttons,xxx,yyy,DULL,DULL)
GetJoystick(1,buttons2,xxx2,yyy2,DULL,DULL)
    k=inkey
if choice=choice2 and (buttons=1 or buttons2=1) then buttons=0:buttons2=0:k="":k2="":choice2=0
if buttons=1 or buttons2=1 then
    if choice=17 then k2=chr(27):flag=1:screenset 1,0:helpflag=0
    If choice=16 Then k2="N"
if choice=15 then k2="F"   
   if choice=14 then k2=chr(8):screenset 1,0:helpflag=0
    if choice=13 then k2="L":helpflag=101
    if choice=12 then k2="R"
    if choice=11 then k2="M"
    if choice=10 then k2="C"
    if choice=9 then k2="V"
    if choice=8 then k2="S"
    if choice=7 then k2="5":screenset 1,0:helpflag=0
    if choice=6 then k2="4":screenset 1,0:helpflag=0
    if choice=5 then k2="3":screenset 1,0:helpflag=0
    if choice=4 then k2="2":screenset 1,0:helpflag=0
    if choice=3 then k2="1":screenset 1,0:helpflag=0
    if choice=2 then k2="T"
    if choice=1 then k2="H"
    k=k2
    
    'sleep 250,1
    choice2=choice
    end if
    if (yyy <-.75 and yyy>-1.5) OR (yyy2 <-.75 and yyy2>-1.5) then 
        choice=choice-1
        if choice=0 then choice=17
        end if
if (yyy>.75 and yyy<1.5) OR (yyy2>.75 and yyy2<1.5) then 
    'singletopz=singletopz-singled
   
choice=choice+1
if choice=18 then choice=1
end if
sleep 75,1
   ' if lcase(k)<>"h" and k<>"" then exit do
  '  sleeper2:
   ' GetJoystick(0,buttons,xxx,yyy,xxx2,yyy2)
'GetJoystick(0,buttons2,xxx,yyy,xxx2,yyy2)
'loop until timer>timekeeper

  '  if k<>"h" AND k<>"H" then 
  '      sleep 50,1:k="":goto sleeper
  '  else
  '      k=""
        
  '      end if
  if helpflag>0 and (buttons=512 or buttons2=512 or lcase(k)="h") then
      buttons=0:buttons2=0
    sleep 1000,1
       '     k="h"':exit if
    'else
        helpflag=0
        k=""
            screenset 1,0
'timeflag=helpflag-1
     'if k<>"h" then   k=""':exit if':sleep 1000,1
'if button=512 or buttons2=512

end if      
'end if
end if
rem love
if ucase(k)="F" then
    if fps=0 then 
        fps=1
    else
        fps=0
    end if
    end if
if ucase(k)="L" then
    if game2<>13 then
        'screenset 1,0:cls
        cls
        screencopy
        screenset 1,0
        cls
        SCREENCOPY
    'mutexlock mutex
    game=13
    ''mutexunlock mutex
    game13=game2
    game2=13
   '; if helpflag>0 then helpflag=13
   IF HELPFLAG=1 THEN SLEEP 250,1:HELPFLAG=101:BUTTONS=0:BUTTONS2=0 
else
   ' if helpflag=13 then helpflag=1
    'mutexlock mutex
    game=game13
    ''mutexunlock mutex
    game2=game13
    IF HELPFLAG=101 THEN HELPFLAG=1
end if
end if
if k="r" or k="R" then
    k=""
    do
        sleep 25,1
    
   ' k=inkey
    cls
    print"Are you sure you want to reset all settings and high scores (yY/nN)????????"
    print"On logitech gamepad press start button for Yes and A (or 1) button for No"
    screencopy
    k=ucase(inkey)
        GetJoystick(0,buttons,xxx,yyy,xxx2,yyy2)
if buttons=512 then k="Y"
if buttons=1 then k="N"
    if k="Y" then
        FOR SCORE=10 TO 1 STEP -1
            PONGNAME(SCORE)=""'PONGNAME(SCORE)
            PONGSCORE(SCORE)=0'PONGSCORE(SCORE)
            starname(score)=""
            starscore(score)=0
            
        NEXT SCORE
        timeflag=0
    tiesaucer=0
    vol255=255
    muting=0
            kill"scores.dat":sleep 1000
 open "scores.dat" for OUTPUT as #1

    for score=1 to 10
        print #1,starname(score)
        print #1,starscore(score)
    next score
    for score=1 to 10
        print #1,pongname(score)
        print #1,pongscore(score)
    next score
        print #1,timeflag
print #1,tiesaucer:print #1,vol255:print #1,muting
print #1,fps:Print #1,fullscreentoggle
    close #1
        k=chr(255)
    end if
    if k="N" then k=chr(255)
loop until k=chr(255)
k=""
end if
  If k="N" Or k="n" Then
  	If fullscreentoggle=0 Then fullscreentoggle=1 Else fullscreentoggle=0:End If
  	If fullscreentoggle=0 Then screenres 1280,720,32,4,&h1' or &h80 or &h40000
  	If fullscreentoggle=1 Then ScreenRes 1280,720,32,4,&h00
  EndIf  
if k="m" or k="M" then
    if muting=0 then
        muting=1
            FMUSIC_stopAllSongs
'FMUSIC_SetPaused(byval mod_ as FMUSIC_MODULE ptr, byval pause as byte)
    else
'        fmusic_playallsongs
        'fmusic_unpauseallsongs
        muting=0
    end if
   end if
   if k="c" or k="C" then fmusic_stopallsongs
if k="v" or k="V" then
    if vol255=0 then
        vol255=255
    else
     '   fsound_stream_stop(stream)
   ' FMUSIC_StopAllSongs
        vol255=0
    end if
    fsound_setvolume(fsound_all,vol255)
end if

    if k="t" or k="T" then
        if timeflag=0 then 
            timeflag=1
        else
            timeflag=0
        end if
    end if
    if k="S" or k="s" Then
    	shipsflag=0
   ' EndIf
        if tiesaucer=0 then 
            tiesaucer=1
        else
            tiesaucer=0
        end if
    end if
    
if k="1" and game2<>1 then
    astrocnt=0
    'fsound_stream_stop(stream)
    '    FMUSIC_StopAllSongs
'    sleep 250
'shipsflag=1
   'mutexlock mutex
game3=1
    game=2
   ' game2=1
 '  game2=game
    flag=0
    pressaflg=0
readyflg=0
livestop=9999
    livesbottom=9999
    energytop=100
    energybottom=100
''mutexunlock mutex
 'game2=1
 'FMUSIC_StopAllSongs
 '   fsound_stream_stop(stream)
 'lockon=1
   timered2=timer+60'120*.5*10
pressed=0
zcount=0
shipsflag=0
'pressaflg=0
' exit if
'zcount=0
'pressed=0
'pressaflg=0
'readyflg=0
 '       FSOUND_Stream_Stop(stream)
'screenset 1,0
'cls
'k=""
end if
if k="2" and game2<>3 then
        FMUSIC_StopAllSongs

 pingpongtop=0
 pingpongbottom=0
cup9top=0
    cup9bottom=0
  singletopx=0
    singlebottomx=0
 livestop=5
    livesbottom=5
    energytop=100
    energybottom=100
 
 '  game16=3
   'mutexlock mutex
    game=4
     'game2=game
   ' game3=3'game-1
    'game2=3
    'flag=0
''mutexunlock mutex
 'game2=3
 'FMUSIC_StopAllSongs
    'fsound_stream_stop(stream)
     topxxx=1280/2
  bottomxxx=1280/2
   timered2=timer+60'120*100*.75
zcount=0
pressed=0
pressaflg=0
            gameonflag=0
            if rnd>.5 then
                headstales=0
            else
                headstales=1
            end if
            ' FSOUND_Stream_Close(stream)
         '      FSOUND_Stream_Close(stream3)
       ' FSOUND_Stream_stop(stream)

       '  stream=FSOUND_Stream_Open(SOUND_FILE, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
        FSOUND_Stream_Stop(stream)
        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream)
zcount=0
pressed=0
pressaflg=0
readyflg=0
'screenset 1,0
'cls
'sleep
end if
if k="3" and game2<>5 then
        FMUSIC_StopAllSongs

pingpongtop=0
 pingpongbottom=0
cup9top=0
    cup9bottom=0
  '  game16=3
   'mutexlock mutex
    game=6
    'game2=3
    'flag=0
     singletopx=0
    singlebottomx=0
    ' game2=game
   ' game3=5
''mutexunlock mutex
 'game2=3
 'FMUSIC_StopAllSongs
    'fsound_stream_stop(stream)
     topxxx=1280/2
  bottomxxx=1280/2
   timered2=timer+60'120*100*.75
zcount=0
pressed=0
pressaflg=0
livestop=5
    livesbottom=5
    energytop=100
    energybottom=100
            gameonflag=0
            if rnd>.5 then
                headstales=0
            else
                headstales=1
            end if
            ' FSOUND_Stream_Close(stream)
         '      FSOUND_Stream_Close(stream3)
       ' FSOUND_Stream_stop(stream)

       '  stream=FSOUND_Stream_Open(SOUND_FILE, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
        FSOUND_Stream_Stop(stream)
        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream)
zcount=0
pressed=0
pressaflg=0
readyflg=0
   ' FMUSIC_StopAllSongs
'screenset 1,0
'cls
'sleep
end if
if k="4" and game2<>10 then
        FMUSIC_StopAllSongs
 pingpongtop=0
 pingpongbottom=0
cup9top=0
    cup9bottom=0
  '  game16=3
   'mutexlock mutex
    game=11
    'game2=game
    singletopx=0
    singlebottomx=0
    livestop=5
    livesbottom=5
    energytop=100
    energybottom=100
  '  game3=10
    'game2=3
    'flag=0
''mutexunlock mutex
 'game2=3
 'FMUSIC_StopAllSongs
    'fsound_stream_stop(stream)
     topxxx=1280/2
  bottomxxx=1280/2
   timered2=timer+60'120*100*.75
zcount=0
pressed=0
pressaflg=0
            gameonflag=0
            if rnd>.5 then
                headstales=0
            else
                headstales=1
            end if
            ' FSOUND_Stream_Close(stream)
         '      FSOUND_Stream_Close(stream3)
       ' FSOUND_Stream_stop(stream)

       '  stream=FSOUND_Stream_Open(SOUND_FILE, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
        FSOUND_Stream_Stop(stream)
        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream)
zcount=0
pressed=0
pressaflg=0
readyflg=0
end if
if k="5" and game2<>12 then
' game2=99
astrocnt=0
   game3=12
   game=2
   pressaflg=0
readyflg=0
livestop=99
    livesbottom=99
    energytop=100
    energybottom=100
   ' game2=game
   ' game3=game
   ' game2=1
    'flag=0
''mutexunlock mutex
 'game2=1
 'FMUSIC_StopAllSongs
 '   fsound_stream_stop(stream)
   
            timered2=timer+60'120*5*.75
pressed=0
zcount=0
'lockon=0
'pressaflg=0
' exit if
'zcount=0
'pressed=0
'pressaflg=0
'readyflg=0
 '       FSOUND_Stream_Stop(stream)
'screenset 1,0
'cls
'k=""
end if
if helpflag=1 then goto jumptokey

'if k=chr(13) and flag=0 then flag=2 else flag=0
'if k=" " then mutexdestroy mutex:end
'dim clcd1 as SINGLE
'if flag<>1 then flag=2
'clcd1=1000*(count_cycles*100/totaledcycles/100)
'if (1000/fs-clc1)>(1000/fs) then
'sleep 1000/25-clcd1,1
'flag=1
'else
'clc=1000*(tmeend-tmebegin)
'clc=clc/(10000*60)
'if 1000/60-clc1>0 then
'clc=0
'sleep ((1000/33-clc),1)
'sleep(1000/60,1)
'sleep (sleep (1000000/6666666,1)
'clc1=0
'end if
'if tt<len(aaa)-25 then
   ' cls 
'    screencopy 1,0
'elseif tt>len(aaa)-26 then
'    screencopy 1,0
'    end if
'if clcd1<(1000/60) then
' sleep ((1000/60)-clcd1,1)
'else
' sleep (1000/60,1)
 'sleep (33,1)  
'sleep (0,1)
'cls
'end if
'screencopy

'sleep
'sleep (1000/60-clcd1,1)'=1000*(count_cycles*100/totaledcycles/100),1)
'cls
'sleep (3,1)
'screencopy'/333*1000,1)
'if flag=0 then sleep (3/2/2,1)
'sleep (3/2/2/2/2/2/2/2/2/2,1)
'sound pulsewave((220*2*(1.05946309436*cc1d))),3/(2*24)
'sound pulsewave((220*2*(1.05946309436*(27-cc1d)))),3/(2*24)
'counter_end()
'dim sleeptime as SINGLE
'sleeptime=(1000)*(count_cycles*100/totalcycles/100)
'sleep (1000/30-sleeptime,1)
'sleeper=1
'k=inkey
'''''''''''''''''''''''sleep (3/2/2/2/2/2/2/2/2/2,1)
'if k=chr(27) then end
'sleeper=1
'if int(timer)>seconds-1 then radian=radian+(3.14159265359*2/60):seconds=int(timer)+1
'if radian>3.14159265359 then radian=0
'if flag=0 then sleep(3/2/2,1)
'sleeper=1
'screencopy:sleeper=1
'if sleeper=1 then sleep
'if cccc=3 then
 '         sound PULSEwave(notes(14)-NOTES(13)/2+NOTES(13)),3/(2*24)
         
'screencopy
'cls
'    cccc=2
'elseif cccc=1 then
              'sound PULSEwave(notes(14)-NOTES(13)/2+NOTES(13)),3/(2*24)

'    cccc=3
'elseif cccc=2 then
'    cccc=1
'end if
'cls
'screencopy

'sleep(1.5,1)
'cls





'drawstar((1280)/2-(26*5*2*1.222),(720)/2-((26*5*1.333)+(26*5*.444)),cc1d,waver3,backup2,cccc,cccc2)
'draw string(-3+1280-((1280)/2-460),-3+720-((720)/2-(26*2.5+25))),chr(64+cc1d),waver3
'drawstar((1280)/2+(26*5*2*1.222),(720)/2-((26*5*1.333)+(26*5*.444)) ,cc1d,waver3,backup2,cccc,cccc2)

'drawstar((1280)/2-(26*5*2*1.222),(720)/2+((26*5*1.333)+(26*5*.444)),cc1d,waver3,backup2,cccc,cccc2)
'Draw String (-3+1280-(((1280)/2)-(1280/6.67+80)/2),-3+720-(((720)/2)-(720/4.25+80)/2-240)),chr(64+cc1d),waver3
'drawstar((1280)/2+(26*5*2*1.222),(720)/2+((26*5*1.333)+(26*5*.444)),cc1d,waver3,backup2,cccc,cccc2)

'waver3=backup
'jerkout:
'next cccc
'dim rgbbackup1 as long
'rgbbackup1=circjerk(1)
'for jerkcount=26*2.5 to 2 step -1'to 26*2.5
   ' if jerkflag=0 then
   '     jerkflag=1
'        circjerk(jerkcount-1)=circjerk(jerkcount)
'    next jerkcount
'    circjerk(26*2.5)=rgbbackup1
    
   ' elseif jerkflag=1 then
   '             circjerk(jerkcount)=wave2rgb(378+waveryclac)
'jerkflag=2
'elseif jerkflag=2 then
'    circjerk(jerkcount)=rgb(0,0,0)
'    jerkflag=0
'end if
'next jerkcount
'jerkout:
'screencopy
'screencopy cccc,2
'sleep(1,1)
'clsdim waver4
 ''mutexlock mutex
 '     game2=game
 'dim as single flatxxx
 'if astroz(astrocnt)>.66 then astrocount=0
      if game2<>13 or game2<>12 or game2<>1 or game2<>2 then
     '     color rgb(255,255,255),rgb(0,0,0)
     lockon=0
     
      end If
       if game2<>13 or game2<>12 or game2<>1 or game2<>2 And judgedbottom<Timer then
     '     color rgb(255,255,255),rgb(0,0,0)
     blowbottomlx=0
     blowbottomly=0
     blowbottomrx=0
     blowbottomry=0
     blowbottomx=0
     blowbottomy=0
     end if
if game2<>13 or game2<>12 or game2<>1 or game2<>2 And judgedtop<Timer then
     '     color rgb(255,255,255),rgb(0,0,0)
     blowtoplx=0
     blowtoply=0
     blowtoprx=0
     blowtopry=0
     blowtopx=0
     blowtopy=0
     end if

if game2=1 or game2=12 or game2=2 or game2=13 then 
'	 for astrocnt=3 to 1 step -1
astrocnt2=astrocnt2+1
'EndIf
 '     ''mutexunlock mutex
    
if game2=13 or game2=12 or game2=1 or game2=2 then
    '  screenset 1,0
'cls

    'paint(1,1),rgb(7,7,7)
    zoomlevel=-1.5'.5'6.33'9'6.5'4'3.5'4.66'10'2.5
    dim as ulongint rainbow2 
    FOR pointIdx =  1 TO numberOfstars 'step+int(rnd*2)+1
	'starcounter=starcounter+1
    ' if starcounter=100 and ((stars(pointidx).x+stars(pointidx).y+stars(pointidx).z>0) or (stars(pointidx).x+stars(pointidx).y+stars(pointidx).z<0)) then
   'starcounter=0
if lockon=0 then
lockon=1
xxxx=stars(1).x
yyyy=stars(1).y		
zzzz=stars(1).z
rainbow2=stars(1).c
if zzzz>1.5 then zzzz=-1.5+(1.5-zzzz)
end if
stars(pointidx).z=stars(pointidx).z+((99+66)/(720*2+(720/2)*(33/3)))
if stars(pointidx).z>1.5 then stars(pointidx).z=-1.5+(1.5-stars(pointidx).z)
stars(pointidx-1).x=stars(pointidx).x
  stars(pointidx-1).y=stars(pointidx).y
 stars(pointidx-1).z=stars(pointidx).z
'if game2=13 then 'or ((game2=1 or game2=12)hen
 '    starcounter=0
flatx=stars(pointidx).x*((1280))/(stars(pointidx).z+zoomlevel)
flaty=stars(pointidx).y*((720))/(stars(pointidx).z+zoomlevel)
flatz=stars(pointidx).z*(720*2+(720/2))
if game2=13 then 
 newx = flatx'newx'
newy = (sin(flat1) * flatz) + (cos(flat1) * flaty)
newz = (cos(flat1) * flatz) - (sin(flat1) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
newy=flaty''y = y'
newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) + (sin(flat2) * flaty)
newy = (cos(flat2) * flaty) - (sin(flat2) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
end if
if singlebottomx>0 and singletopx>0 then
 newx = flatx'(cos(flat1) * flatx) + (sin(flat1) * flaty)
newy = (sin(flat1x) * flatz) + (cos(flat1x) * flaty)
newz = (cos(flat1x) * flatz) - (sin(flat1x) * flaty)
flat1x=flat1x-(pieyedi*2/(1024*256*128*2))
if flat1x<0 then flat1x=pieyedi*2-(pieyedi*2/(1024*256*128*2))
else
  newx = flatx'(cos(flat1) * flatx) + (sin(flat1) * flaty)
newy = (sin(flat1x) * flatz) + (cos(flat1x) * flaty)
newz = (cos(flat1x) * flatz) - (sin(flat1x) * flaty)
'flat1x=flat1x+(pieyedi*2/(1024*256*128/2))
'if flat1x>pieyedi*2 then flat1x=0+(pieyedi*2/(1024*256*128/2))
end if
flatx=newx
 flaty=newy
 flatz=newz
if singlebottomx<0 and singletopx<0 then
  newx = flatx'(cos(flat1) * flatx) + (sin(flat1) * flaty)
newy = (sin(flat1x) * flatz) + (cos(flat1x) * flaty)
newz = (cos(flat1x) * flatz) - (sin(flat1x) * flaty)
flat1x=flat1x+(pieyedi*2/(1024*256*128*2))
if flat1x>pieyedi*2 then flat1x=0+(pieyedi*2/(1024*256*128*2))
else
     newx = flatx'(cos(flat1) * flatx) + (sin(flat1) * flaty)
newy = (sin(flat1x) * flatz) + (cos(flat1x) * flaty)
newz = (cos(flat1x) * flatz) - (sin(flat1x) * flaty)
'flat1x=flat1x-(pieyedi*2/(1024*256*128/2))
'if flat1x<0 then flat1x=pieyedi*2-(pieyedi*2/(1024*256*128/2))
end if
flatx=newx
 flaty=newy
 flatz=newz
 if singletopy>0 then
 newx = (cos(flat1y) * flatx) - (sin(flat1y) * flatz)
newy=flaty''y = y'
newz = (sin(flat1y) * flatx) + (cos(flat1y) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz


flat1y=flat1y-(pieyedi*2/(1024*512*128*2))
if flat1y<0 then flat1y=pieyedi*2-(pieyedi*2/(1024*512*128*2))
else
     newx = (cos(flat1y) * flatx) - (sin(flat1y) * flatz)
newy=flaty''y = y'
newz = (sin(flat1y) * flatx) + (cos(flat1y) * flatz)
end if
flatx=newx
 flaty=newy
 flatz=newz
if singlebottomy<0 then
 newx = (cos(flat1y) * flatx) - (sin(flat1y) * flatz)
newy=flaty''y = y'
newz = (sin(flat1y) * flatx) + (cos(flat1y) * flatz)
 


flat1y=flat1y+(pieyedi*2/(1024*512*128*2))
if flat1y>pieyedi*2 then flat1y=0+(pieyedi*2/(1024*512*128*2))
else
    newx = (cos(flat1y) * flatx) - (sin(flat1y) * flatz)
newy=flaty''y = y'
newz = (sin(flat1y) * flatx) + (cos(flat1y) * flatz)

end if 
flatx=newx
 flaty=newy
 flatz=newz 

   
    
    
'end if    
rem dream come true
if topxxx>1280/2 and bottomxxx>1280/2 then
newx = (cos(flat1z) * flatx) + (sin(flat1z) * flaty)
newy = (cos(flat1z) * flaty) - (sin(flat1z) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
 flat1z=flat1z-(pieyedi*2/(1024*512))
if flat1z<0 then flat1z=pieyedi*2-(pieyedi*2/(1024*512))
 
 
'end if
elseif topxxx<1280/2 and bottomxxx<1280/2 then
  newx = (cos(flat1z) * flatx) + (sin(flat1z) * flaty)
newy = (cos(flat1z) * flaty) - (sin(flat1z) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
 flat1z=flat1z+(pieyedi*2/(1024*512))
if flat1z>pieyedi*2 then flat1z=0+(pieyedi*2/(1024*512))
 
else
     newx = (cos(flat1z) * flatx) + (sin(flat1z) * flaty)
newy = (cos(flat1z) * flaty) - (sin(flat1z) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz  
end if
   
'newx=flatx
'newy=flaty
'newz=flatz
 

 dim as single cas1=flatz,cas2 'as single=flatz
 if cas1<0 then cas1=cas1-(cas1*2)
 cas2=(1-((1)*cas1*100/(720*2+(720/2))/100))
casual=cas2'1*(cas2*100/3/100)
if casual>1 then casual=1'casual-(casual*1)
if casual<.20 then casual=.20'casual-(casual*1)
   ' rainbow=rgb(255,192,203)
   rainbow=rgb(int(rnd*255),int(rnd*255),int(rnd*255))'stars(pointidx).c'rgb(255*rnd,255*rnd,255*rnd)
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )
'd=255
'n=255
'e=251
rainbow=rgb(d*casual,n*casual,e*casual)
 screenlock
   circle (newX+((1280)/2), newy+((720)/2)),2+(.25/2)*casual,rainbow,,,,F

screenunlock
'end if
NEXT
lockon=0
stars(numberofstars).x=xxxx
stars(numberofstars).y=yyyy		
stars(numberofstars).z=zzzz
stars(numberofstars).c=rainbow2

end if
'if astrocnt=0 then 
'    for astrocnt=1 to 3
'        astroz(astrocnt)=.55

'if astrocount=10 then astrocnt=1
'if astrocount=20 then astrocnt=2
'if astrocount=30 then astrocnt=3


'dim astrocnt as integer

 For astrocnt=3 to 1 step -1

if game2=1 or game2=12 or game2=2 and astroz(1)<>10 AND ASTROZ(ASTROCNT)<.20 then
        zoomlevel=-1.5'.5'6.33'9'6.5'4'3.5'4.66'10'2.5
 'for astrocnt=3 to 1 step -1

 ' astrocnt=1  
  if astroz(astrocnt)<.20 then


        'if astroz(astrocnt)<-1.5 then
         if (astroz(astrocnt)<.020 and astrot(astrocnt)>4) or (astrot(astrocnt)=2 and astrox(astrocnt)>1280) or (astrot(astrocnt)=4 and astrox(astrocnt)<0) then
         '    timer7=int(timer+16)
        ' if rnd<5 then
        'randomize timer
     ' dim tine1 as integer
      'for tine1=1 to 3
      '  flatter(astrocnt)=-(rnd*pieyedi*2)+(pieyedi*2*rnd)
        astroz(astrocnt)=5
       'astrox(astrocnt)=int(rnd*1280)+1
       'astroy(astrocnt)=int(rnd)+1
       astrot(astrocnt)=1
       astroh(astrocnt)=0
      ' exit for
   goto end1
   end if
     if astrot(astrocnt)=4 then astrox(astrocnt)=astrox(astrocnt)-16
      if astrot(astrocnt)=2 then astrox(astrocnt)=astrox(astrocnt)+16
if (astrot(astrocnt)=4 or astrot(astrocnt)=2)  and astroy(astrocnt)>720 then astroy(astrocnt)=astroy(astrocnt)-6
      if (astrot(astrocnt)=4 Or astrot(astrocnt)=2) and astroy(astrocnt)<0 then astroy(astrocnt)=astroy(astrocnt)+6


 '  if (astrot(astrocnt)=1 and topxxx>astrox(astrocnt)) or (astrot(astrocnt)=3 and bottomxxx>astrox(astrocnt)) then 
 '      astrox(astrocnt)=astrox(astrocnt)+4
 '  elseif (astrot(astrocnt)=1 and topxxx<astrox(astrocnt)) or (astrot(astrocnt)=3 and bottomxxx<astrox(astrocnt)) then
 '         astrox(astrocnt)=astrox(astrocnt)-4
 '  end if 
 '  if (astrot(astrocnt)=1 and topyyy>astroy(astrocnt)) or (astrot(astrocnt)=2 and bottomyyy>astroy(astrocnt)) then 
 '      astroy(astrocnt)=astroy(astrocnt)+4
 '  elseif (astrot(astrocnt)=1 and topyyy<astroy(astrocnt)) or (astrot(astrocnt)=2 and bottomyyy<astroy(astrocnt)) then
 '         astroy(astrocnt)=astroy(astrocnt)-4
 '  end if 
      ' astrocnt=97 game2=1 or game2
    dim as single topzzz2=int(topzzz*5*12),bottomzzz2=int(bottomzzz*5*12)
   ' if astrocnt=97 and topzzz<astroz(astrocnt) then astroz(astrocnt)=astroz(astrocnt)-.0125/(3)
    'if astrocnt=98 and bottomzzz>astroz(astrocnt) then astroz(astrocnt)=astroz(astrocnt)+.0125/(3)
'    if astrocnt=97 and bottomzzz<astroz(astrocnt) then astroz(astrocnt)=astroz(astrocnt)-.0125/(3)

    
    
    for pointidx=1 to pixela(astrocnt) step +int(rnd*16)+1
        'if astroids(pointidx).c<>rgb(0,0,0) then
       ' IF astroids(pointIdx).Z+zoomlevel>0 then 
        if flatter(astrocnt)>pieyedi then
flatx=astroidsx(astrocnt,pointidx)*cos((flatter(astrocnt)+flat1))*(32*(astroz(astrocnt)*5))/(astroidsz(astrocnt,pointidx)+zoomlevel)
flaty=astroidsy(astrocnt,pointidx)*sin((flatter(astrocnt)+flat1))*(32*(astroz(astrocnt)*5))/(astroidsz(astrocnt,pointidx)+zoomlevel)
flatz=astroidsz(astrocnt,pointidx)*sin((flatter(astrocnt)+flat1))*(32*(astroz(astrocnt)*5))/((720*2+(720/2)*8)+zoomlevel)
      '  flatter(astrocnt)=pieyedi*2*rnd
      
else
flatx=astroidsx(astrocnt,pointidx)*cos((flatter(astrocnt)+flat2))*(32*(astroz(astrocnt)*5))/(astroidsz(astrocnt,pointidx)+zoomlevel)
flaty=astroidsy(astrocnt,pointidx)*sin((flatter(astrocnt)+flat2))*(32*(astroz(astrocnt)*5))/(astroidsz(astrocnt,pointidx)+zoomlevel)
flatz=astroidsz(astrocnt,pointidx)*sin((flatter(astrocnt)+flat2))*(32*(astroz(astrocnt)*5))/((720*2+(720/2)*8)+zoomlevel)
     
      end if  
    
'    flatx=astroids(pointidx).x*cos((flatter(astrocnt)+flat2))*(96*(astroz(astrocnt)*5))/(astroids(pointidx).z+zoomlevel)
'flaty=astroids(pointidx).y*sin((flatter(astrocnt)+flat2))*(96*(astroz(astrocnt)*5))/(astroids(pointidx).z+zoomlevel)
'flatz=astroids(pointidx).z*sin((flatter(astrocnt)+flat2))*(96)*(720*2+(720/2))*(astroz(astrocnt)*5)
'end if
if astrocnt<0 then
 newx = flatx'newx'
newy = (sin((flatter(astrocnt)+flat1)) * flatz) + (cos((flatter(astrocnt)+flat1)) * flaty)
newz = (cos((flatter(astrocnt)+flat1)) * flatz) - (sin((flatter(astrocnt)+flat1)) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos((flatter(astrocnt)+flat1)) * flatx) - (sin((flatter(astrocnt)+flat1)) * flatz)
newy=flaty''y = y'
newz = (sin((flatter(astrocnt)+flat1)) * flatx) + (cos((flatter(astrocnt)+flat1)) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flatter(astrocnt)+(flat1)) * flatx) + (sin((flatter(astrocnt)+flat1)) * flaty)
newy = (cos((flatter(astrocnt)+flat1)) * flaty) - (sin((flatter(astrocnt)+flat1)) * flatx)
newz = flatz
'end if
 flatx=newx
 flaty=newy
 flatz=newz
'end if
   end if
    
    newx=flatx
        newy=flaty

'end if    
'rem dream come true
 'newx=flatx
 'newy=flaty
' if newx>bottomxxx-48 and newx<bottomxxx+48 and newy>bottomyyy and newy<bottomyyy+48 and bottomzzz2>=int(astroz(astrocnt)*5*12)-6 and bottomzzz2<=int(astroz(astrocnt)*5*12)+6 then
       'color ,rgb(255,0,0)
 '      topgn3=0
'goto doblin

If timer>judgedbottom and newx+astrox(astrocnt)>bottomxxx-(30*(BOTTOMZZZ*5)) and newx+astrox(astrocnt)<bottomxxx+(30*(BOTTOMZZZ*5)) and newy+astroy(astrocnt)>bottomyyy and newy+astroy(astrocnt)<bottomyyy+((45)*(BOTTOMZZZ*5)) and bottomzzz2>=int(astroz(astrocnt)*5*12)-8 and bottomzzz2<=int(astroz(astrocnt)*5*12)+8 then
     'RANDOMIZE RND+TIMER+Rnd
       
       ' FSOUND_Stream_Close(stream6)
         '      FSOUND_Stream_Close(stream3)

        ' stream=FSOUND_Stream_Open(SOUND_FILE6, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
       ' FSOUND_Stream_Stop(stream)
        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream6)
    '   bottomxxx=1280/2
     '  bottomyyy=720-(720/4)
       If astroh(astrocnt)=1 Then
       	energybottom=100
       	livesbottom=livesbottom+5
       	bottomfired=0
      ' End If 	
       	
       EndIf
       ' screenlock
        'cls
        If astroh(astrocnt)<>1 Then
         IF RND>.49 THEN 
                   paint(1,1),rgb(255,0,0)
                   screencopy:flip',rgb(255,5,5)
              ' else
              end if
              ' IF RND>2 THEN 
              '     paint(1,1),rgb(255,0,0)',rgb(255,5,5)
              ' else
                  for hondal=1 to (25)*(25/2)
                  	''RANDOMIZE RND+TIMER+Rnd
                       hondax=rnd*128
                       honday=rnd*128
                       screenlock
                       circle(bottomxxx+64-hondax,bottomyyy+32+64-honday),(rnd*256)*(bottomzzz*5),rgb(255,(255*rnd),(127*rnd)),,,,F
                               screenunlock
judgedbottom=timer+6
                   screencopy
                   flip
                   sleep 0,1
                   next hondal
              '     end if',rgb(
                ' paint(1,1),rgb(255,0,0)',rgb(255,5,5)

  ' circle(bottomxxx,bottomyyy+32),64,rgb(255,192,0),,,,F
       ' screenunlock
     '   screencopy
        'color ,rgb(0,0,0)
       'LINE(BOTTOMXXX-32,BOTTOMYYY+32)-(BOTTOMXXX+32,BOTTOMYYY),rgb(255,7,7),BF',rgb(255,3,3)
                cup9top=cup9top+50
                 energybottom=0'energybottom-5.0
                if energybottom<1 then 
                    energybottom=100
                    livesbottom=livesbottom-1
                    END IF
                    if livesbottom<1 then livesbottom=0
       '             ; if rnd<5 then
      '    ;  astroz(astrocnt)=(topzzz*5)*(bottomzzz*5)
      
      ' astrocnt=99
        End If
astroz(astrocnt)=5
ASTROT(ASTROCNT)=0
astroh(astrocnt)=0
bottomfired=0
' ;else
      ' ;     astroz(astrocnt)=(bottomzzz*5)*(topzzz*5)
      ' ;     astrocnt=98
      ' ;     end if
     '  astrox(astrocnt)=int(rnd*(1280/2))+1
     '  astroy(astrocnt)=int(rnd*(720/2))+1
        '   flatter(astrocnt)=pieyedi*2*rnd
                  '  astrocnt=99
               ' for rannycount=1 to 12
               '         sound pulsewave(notes(rannycount)),1/16
               '         next rannycount
'topgun(tempgunx,tempguny,1)=0
'topgn3=topgun3(tempgunx,tempguny)
'topgun3(tempgunx,tempguny)=0
'topgunx(calca)=0
'topguny(calca)=0
end if
'draw string (1,8),str(bottomzzz2),rgb(255,255,255)
'draw string (1,16),str(bottomyyy),rgb(255,255,255)
'rem discotech
'dim as integer bottomzzz2,topzzz2
'if bottomgun(tempgunx,tempguny,1)>0 and tempguny<topyyy then sleep




'if singletopz<0 and topzzz>.25/2 then topzzz=topzzz-.0125:singletopz=singletopz+1:end if 'else singletopz=singletopz+.5:end if
'if singletopz>0 and topzzz<.25 then topzzz=topzzz+.0125:singletopz=singletopz-1:end if  'else singletopz=singletopz-.5:end if
'if singlebottomz<0 and bottomzzz>.25/2 then bottomzzz=bottomzzz-.0125:singlebottomz=singlebottomz+1:end if 'else singlebottomz=singlebottomz+.5:end if
'if singlebottomz>0 and bottomzzz<.25 then bottomzzz=bottomzzz+.0125:singlebottomz=singlebottomz-1:end if 'else singlebottomz=singlebottomz-.5:end if



 if timer>judgedtop and newy+astrox(astrocnt)>topxxx-(30*(TOPZZZ*5)) and newy+astrox(astrocnt)<topxxx+(30*(TOPZZZ*5)) and newy+astroy(astrocnt)<topyyy and newy+astroy(astrocnt)>topyyy-((45)*(TOPZZZ*5)) and topzzz2>=int(astroz(astrocnt)*5*12)-8 and topzzz2<=int(astroz(astrocnt)*5*12)+8 then
      '  bottomgn3=0
        
        'RANDOMIZE RND+TIMER+Rnd
   '  if newx+astrox(astrocnt)>topxxx-48 and newx+astrox(astrocnt)<topxxx+48 and newy+astroy(astrocnt)<topyyy and newy+astroy(astrocnt)>topyyy-48 and topzzz2>=int(astroz(astrocnt)*5*12)-6 and topzzz2<=int(astroz(astrocnt)*5*12)+6 then
    
        
       '  FSOUND_Stream_Close(stream6)
         '      FSOUND_Stream_Close(stream3)

       '  stream=FSOUND_Stream_Open(SOUND_FILE6, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
       ' FSOUND_Stream_Stop(stream)
        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream6)
        'screenlock
       ' cls
      ' topxxx=1280/2
       'topyyy=720/4
        If astroh(astrocnt)=1 Then
       	energytop=100
      ' End If 	
       	livestop=livestop+5
   topfired=0
       EndIf
       ' screenlock
        'cls
        If astroh(astrocnt)<>1 Then
               'IF RND>2 THEN 
               '    paint(1,1),rgb(255,0,0)',rgb(255,5,5)
               'else
                IF RND>.49 THEN 
                   paint(1,1),rgb(255,0,0)
                   screencopy:flip',rgb(255,5,5)
              ' else
              end if
                 '  dim as integer hondax.honday,hondar,hondal
                  for hondal=1 to (25)*(25/2)
                  '	'RANDOMIZE RND+TIMER+Rnd
                     hondax=rnd*128
                       honday=rnd*128
                       screenlock
                       circle(topxxx+64-hondax,topyyy-32+64-honday),rnd*256*(topzzz*5),rgb(255,(255*rnd),(127*rnd)),,,,F
                               screenunlock
judgedtop=timer+6
                   screencopy
                   flip
                   sleep 0,1
                   next hondal
                '   end if',rgb(
 ' paint(1,1),rgb(255,0,0)
 ' circle(topxxx,topyyy-32),64,rgb(255,0,0),,,,F
       ' screenunlock
      '  dim as integer astrayx as integer
 '       screencopy
 '       sleep 500,1
    '    screencopy
        'LINE(TOPXXX-32,TOPYYY-32)-(TOPXXX+32,TOPYYY),rgb(255,7,7),BF',rgb(255,3,3)
                cup9bottom=cup9bottom+50
                energytop=0
                if energytop<1 then 
                    energytop=100
                    livestop=livestop-1
                    END IF
                    if livestop<1 then livestop=0
    '    astrocnt=99            
                ' if rnd<5 then
        End If
astroz(astrocnt)=5
ASTROT(ASTROCNT)=0
astroh(astrocnt)=0
topfired=0
    '   astrocnt=97
    '   else
    '        astroz(astrocnt)=(bottomzzz*5)*(topzzz*5)
       '     astrocnt=99
     '       end if
       ' astrox(astrocnt)=int(rnd*(1280/2))+1
       ' astroy(astrocnt)=int(rnd*(720/2))+1
       '    flatter(astrocnt)=pieyedi*2*rnd
             ' cup9top=cup9top+1
              '  for rannycount=12 to 1 step -1' to 12
              '          sound pulsewave(notes(rannycount)),1/16'1000/360
'next rannycount
'bottomgun(tempgunx2,tempguny2,1)=0
'bottomgn3=bottomgun3(tempgunx,tempguny)
'bottomgun3(tempgunx2,tempguny2)=0
'bottomgunx(calca)=0
'bottomguny(calca)=0
end If

 
'end if
if newx+astrox(astrocnt)>0 and newx+astrox(astrocnt)<1280 and newy+astroy(astrocnt)>0 and newy+astroy(astrocnt)<720 then

'if +astrox(astrocnt)>0 and astrox(astrocnt)<1280 and astroy(astrocnt)>0 and astroy(astrocnt)<720 then
 screenlock
   circle (newX+astrox(astrocnt), newy+astroy(astrocnt)),5.5*(astroz(astrocnt)*5),astroidsc(astrocnt,pointidx),,,,f

screenunlock
end if
if pointidx=int(pixela(astrocnt)/2) then
    astx(astrocnt)=newx
    asty(astrocnt)=newy
    astz(astrocnt)=int(astroz(astrocnt)*5*12)'newz/(astroz(astrocnt)*5)/(64)
   end if
'end if
'end if
'doblin:
'astroz(astrocnt)=astroz(astrocnt)+.0125/(7)
next
'if (astrot(astrocnt)<>2 or astrot(astrocnt)<>4) and astrot(astrocnt)<5 then astroz(astrocnt)=astroz(astrocnt)-(.0125/8*3+(astroz(astrocnt)))-(.0125/8*3+(astroz(astrocnt)))
'if astrot(astrocnt)>4 then astroz(astrocnt)=astroz(astrocnt)+(.0125/8*3+(astroz(astrocnt)))-(.0125/8*3+(astroz(astrocnt)))
if (astrot(astrocnt)<>2 or astrot(astrocnt)<>4) then astroz(astrocnt)=astroz(astrocnt)+(((1/3)/(60)-(((1/3/(30))*(-.5*(ASTROZ(ASTROCNT)*5))))))
if astrot(astrocnt)>4 then astroz(astrocnt)=astroz(astrocnt)-((((1/3)/(60)+(((1/3/(15))*(.525*(ASTROZ(ASTROCNT)*5)))))))
if astrot(astrocnt)=4 then  astroz(astrocnt)=1.5*((topzzz*5)*(bottomzzz*5)/5)
if astrot(astrocnt)=2 then  astroz(astrocnt)=1.5*((bottomzzz*5)*(topzzz*5)/5)


'indestructable words of this text are one word Heaven and earth factorial of problems solved forever unstopable indestructabley one word 
'unconditionally all this one word Everything's everywhere's everyone's problems are solved forever unstopable indestructable one word
'incorruptible twenty five times one word indestructably corey wilson's problems are solved forever unstopable one word
'incorruptible one word sins and sin done to corey wilson is undone from everlasting to everlasting unstopable one word
'end if
'next
   ' astroz(astrocnt)=astroz(astrocnt)+.0125/3
  End if
end1:
'End if
end if
	 Next
     '  if judgedbottom2<timer then judgedbottom2=timer+1

     '  if judgedtop2<timer then judgedtop2=timer+1

'if tt=0 then tt=1

'tt=tt+1
'if tt>len(aaa)-(1280/8*2) then tt=1

'gameseven:
  zoomlevel=6.33
 'dim greeny as integer
'if   (game2=1 or game2=12) then sleep
if   (game2=1 or game2=12) then
     screenlock
    drawdraw(1,720-(32),"TOP:"+str(cup9top),rgb(255,255,0),4)
       drawdraw(1280-(32*10)-64,720-(32),"BOTTOM:"+str(cup9bottom),rgb(255,255,0),4)
        drawdraw(1280/2-(2*10*8)-48,720-17,"TOPLIVES:"+str(LIVESTOP),rgb(255,255,0),2)
       drawdraw(1280/2,720-17,"BOTLIVES:"+str(LIVESbottom),rgb(255,255,0),2)
       DIM COUNTIT AS INTEGER
       FOR COUNTIT=1 TO ENERGYTOP*.5
           LINE(1+(32*11)+COUNTIT,720)-(1+(32*11)+COUNTIT,720-32),RGB(255-(255*(ENERGYTOP/100)),255*(ENERGYTOP/100),0)
       NEXT
        FOR COUNTIT=1 TO ENERGYBOTTOM*.5
           LINE(1280-(32*11)-100+COUNTIT,720)-(1280-(32*11)-100+COUNTIT,720-32),RGB(255-(255*(ENERGYBOTTOM/100)),255*(ENERGYBOTTOM/100),0)
       NEXT
   screenunlock
'END IF

goto ender2


' paint(1,1),rgb(255,255,251)
FOR pointIdx = 1 TO numberOfPoints step +2
   IF MyArray(pointIdx).Z+zoomlevel > 0 and (myarray(pointidx).x+myarray(pointidx).y)>0 or (myarray(pointidx).x+myarray(pointidx).y)<0 THEN
  	flatX = MyArray(pointIdx).X * (1280*2/2.5) / (MyArray(pointIdx).Z + zoomLevel)
		flatY = MyArray(pointIdx).Y * (720/2.5) / (MyArray(pointIdx).Z + zoomLevel)
    '    airbornx(pointidx)=flatx:airborny(pointidx)=flaty
     '   if pointidx=numberofpoints then jetflag=1
   ' end if
    
       ' flat1 = MyArray(pointIdx-1).X * (1280) / (MyArray(pointIdx-1).Z + zoomLevel)
		'flat2 = MyArray(pointIdx-1).Y * (720) / (MyArray(pointIdx-1).Z + zoomLevel)
		'// Plot a white dot at the point's translated coordinates
       

' casual=(myarray(pointidx).x+zoomlevel*100/(1+zoomlevel)/100)
       ' if casual>1 then casual=1
 '  if myarray(pointidx).x<>0 and safe2x=0 then 
 '      safe2x=safe1x:safe2y=safe1y
 '      safe1x=flatx:safe1y=flaty
 '     '     line(safe1x+((1280)/2),safe1y+((720)/2))-(safe2x+((1280)/2),safe2y+((720)/2)),rgb(255*casual,255*casual,255*casual),bf
''       end if
'tie=2
'game2=2
'if game2=4 or game2=6 or game2=11 then
'        newX = COS(slphouse) * flatx - SIN(slphouse) * flaty'myarray(pointidx).y
        
' newY = SIN(slphouse) * flatx + COS(slphouse) * flaty'myarray(pointidx).y
' end if   
   ' game8=0                                                                                       'elseif tie=2 then
' if game2=2 then
'      newX = COS(pieyedi*2-slphouse) * flatx - SIN(slphouse-(pieyedi*2)) * flaty'myarray(pointidx).y
        
' newY = SIN(pieyedi*2-slphouse) * flatx + COS(slphouse-(pieyedi*2)) * flaty'myarray(pointidx).y
    
     
     
     
     
     '  newX = COS(slphouse) * flatx - SIN(slphouse) * flaty'myarray(pointidx).y
'elseif game2=0 then
'game8=0
'     newX = COS(slphouse) * flatx - SIN(slphouse) * flaty'myarray(pointidx).y
'     newY = SIN(slphouse) * flatx'x + COS(slphouse) * flaty'myarray(pointidx).y 
    ' newy=flaty
'end if
'if   (game2=1 or game2=12) or game2=3 or game2=5 or game2=10  then
newx=flatx
newy=flaty
'game8=1
'if game2>0 then newy=flaty
'newY = SIN(slphouse) * flatx + COS(slphouse) * flaty
'end if
'game2=0

 ' if game2=3 and game8=1 then
 '   newx=flatx
'newy=flaty
'end if
 

'newx=alpx*(1280-shrinkx)/(alpz+zoomlevel)'/zoomlevel
'newy=alpy*(720-shrinky)/(alpz+zoomlevel)'/zoomlevel
'newx=newx*cos(pieyedi/4)

casual=1

'(alpy*2)/100*4/100*2
'casual=
'if alpx<0 then casual=1+alpx
'if alpx
'alpx2=myarray(pointidx).x
'if alpx<0 
'alphy=*cos(+(pieyedi*2/20)-slphouse-(pieyedi/2))
'alphx=myarray(pointidx).y*sin((pieyedi*2/20)-slphouse-(pieyedi/2))
'if slphouse>=pieyedi/4+(pieyedi*2/20)-(pieyedi/2) then  alphy2=myarray(pointidx).y*sin((pieyedi*2/20)-slphouse+pieyedi)
'alphyx=myarray(pointidx).x*cos(pieyedi*2+(pieyedi*2/20)-slphouse-(pieyedi/2))
'alphyy=myarray(pointidx).y*sin((pieyedi*2+pieyedi*2/20)-slphouse+pieyedi-(pieyedi/2))

'alphyx=myarray(pointidx).x*cos(pieyedi*2+(pieyedi*2/20)-slphouse)

'casual=1




'ELSE
'  alphy2=myarray(pointidx).y*sin(pieyedi*2+(pieyedi*2/20)-(PIEYEDI)-slphouse)
' END IF  
'alphyx=myarray(pointidx).x*cos(pieyedi*2+(pieyedi*2/20)-slphouse+pieyedi-(pieyedi/2))
 'alphyy=myarray(pointidx).y*sin(pieyedi*2+(pieyedi*2/20)-slphouse+pieyedi)
  'alphy2=myarray(pointidx).y
'alphy=alphy+1
' alphy2=alphy2+1
' alphyx=alphyx+1
' alphyy=alphyy+1
' omegy=.5
' if alphy>1 then 
     'casual=(1*(alphy*100/2/100))*(1*(alphyx*100/2/100))*2
    ' casual=(1*(alphy2*100/2/100))*(1*(alphyy*100/2/100))*2
     
         ' casual3=1*(alphyx*100/2/100)
         ' casual4=1*(alphyy*100/2/100)
          'casual=(casual+casual2)*100/2/100*1'.5'33333333
          'casual=casual-(.25/2)
                  '  if casual<=.49999999 then casual=casual-.25
 dim x as SINGLE
 
 'rernd2:
 x=rnd(timered)
 'if x=0 then goto rernd2
if x<.6 then casual=casual-.04'03333333
 if x>.5 then casual=casual+.04'03333333
'if (pieyedi*2/20)+slphouse'1*(pieyedi*2/20)-(slphouse-pieyedi/2)*100/(pieyedi*2/20-pieyedi/2)/100'(100/2/100)
          if casual>1 then casual=1
          if casual<0 then casual=0
         ' casual=casual*.5'33333333
          'casual=1-casual
          '*.5'*.03333333
          'casual3=casual3+casual4/2
'omega=casual+casual3
'casual=omega
         ' casual=(casual+casual2/2)
'          casual2=1-casual2
'omegy=casual+casual2/2
' elseif alphy<=1 then
'  alphy=alphy+1
'     casual=1*(alphy*100/2/100)
'     end if
 'casual=omegy 
       '	newx=flatx
       '     newy=flaty
'[flat1 = COS(slphouse) * safe2x - SIN(slphouse) * safe2y'flaty'myarray(pointidx).y
        
'flat2 = SIN(slphouse) * safe2x + COS(slphouse) * safe2y'flaty'myarray(pointidx).y
'           line(newx+((1280)/2),newy+((720)/2))-(flat1+((1280)/2),flat2+((720)/2)),rgb(255*casual,255*casual,255*casual),bf
'safe2x=0:safe2y=0

'end if

'casual=1-casual
'if myarray(pointidx).z<>0 then 
'else
        '        circle (newx+(136*6/2), newy+(136*6/2)),3, rgb(255*casual,255*casual,251*casual)
'zoomlevel=zoomlevel=-0.1
'end if
'if myarray(pointidx).y<0 then
'    casual=1
'    elseif myarray(pointidx).y>=0 then
'if slphouse>=pieyedi then

'tie=2
'if tie=1 then
rem casual=1-(myarray(pointidx).x*100/2/100)*sin(slphouse)'*(1-myarray(pointidx).y*100/1/30)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)
rem casual=casual+(1-(myarray(pointidx).y*100/2/100))/2*cos(slphouse)'*100/2/100'    end if
'elseif tie=2 then
'casual=1-(myarray(pointidx).x*100/(1*2)/100)*sin(slphouse-(pieyedi*2)+(pieyedi*2/20)'*(1-myarray(pointidx)y*100/1/30)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)
' casual=casual+(1-(myarray(pointidx).y*100/(1*2)/100)*cos((pieyedi*2)+(pieyedi*2/20)-slphouse))/2'-slphouse)  
'if slphouse>pieyedi-(pieyedi*2/20*2) then
' casual=1-(myarray(pointidx).x*100/(1*2)/100)*cos((pieyedi*2-pieyedi*2/20)-slphouse)-(1-(myarray(pointidx).y*100/(1*2)/100)*sin(slphouse-(pieyedi*2-pieyedi*2/20)))'*(1-myarray(pointidx)y*100/1/30)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)
' casual2=1-(myarray(pointidx).x*100/(1*2)/100)*sin((pieyedi*2-pieyedi*2/20)-slphouse)+(1-(myarray(pointidx).y*100/(1*2)/100)*sin(slphouse-(pieyedi*2-pieyedi*2/20)))'-slphouse)  
'casual=.5
'casual2=.5
'casual=(myarray(pointidx).x*100/1/30)+1



'if myarray(pointidx).x>=0 then casual=(myarray(pointidx).x*100/(1)/100)*cos(-(pieyedi*2/20)+slphouse)'+(pieyedi*2/20) ' casual2=casual+(1-(myarray(pointidx).y*100/2/100)*cos(slphouse-(pieyedi*2)-(pieyedi*2/20)))/2'-(pieyedi*2-pieyedi*2/20))'*(1-myarray(pointidx)y*100/1/30)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)
'casual=.677'2-(myarray(pointidx).x*100/(1)/100)'+1*sin(-(pieyedi*2/20)+slphouse))'+(pieyedi*2/20) ' casual2=casual+(1-(myarray(pointidx).y*100/2/100)*cos(slphouse-(pieyedi*2)-(pieyedi*2/20)))/2'-(pieyedi*2-pieyedi*2/20))'*(1-myarray(pointidx)y*100/1/30)*sin(pieyedi*2-slphouse)'-sin(slphouse)*(myarray(pointidx).y+1*100/2/100)

'casual2=2-(myarray(pointidx).y*100/(2)/100)/2*sin(-(pieyedi*2/20)+(pieyedi*2)-slphouse)

 'casual=casual+(1-(myarray(pointidx).y*100/(1*2)/100))*sin(slphouse)/2'-slphouse)     
   ' casual=2-myarray(pointidx).x+1*100/2/100*(slphouse*100/(pieyedi)/100)
'if slphouse<=pieyedi-(pieyedi*2/20) then casual=1-casual
'casual=casual+casual2/2'1-casual
'casual=casual+casual2/(1-(myarray(pointidx).z*100/(1*2)/100))
'casual=1
'end if
'casual=1-casual
' casual=(2-(myarray(pointidx).x+1)*100/2/100)*(2-(myarray(pointidx).y+1)*100/2/100)*(myarray(pointidx).z*100/1/30)*(slphouse*100/(pieyedi*2)/100)
 'if casual>1 then casual=1
  ' casual=casual/2'.5
'   if pieyedi*2-slphouse>pieyedi then 
'       casual= 1-(1*(slphouse*100/pieyedi/100))
'elseif pieyedi*2-slphouse<pieyedi then
'          casual= (1*(slphouse*100/pieyedi/100))
 
' if myarray(pointidx).z>0 then casual=casual*(1-myarray(pointidx).z)   
'end if
'if slphouse=pieyedi then casual=0
'if pointidx<(6*8*6)+1 then


'dim as SINGLE coil(4)
'coil(1)=pieyedi/7
'coil(2)=pieyedi/5
'coil(3)=pieyedi/3
'coil(4)=pieyedi
'if alphy<=(pieyedi*2+(pieyedi*2/20))-pieyedi/4 and alphy2<=(pieyedi*2+(pieyedi*2/20))-pieyedi/4 then'or slphouse-(pieyedi/2)>=0-(pieyedi/2)+pieyedi*2/20+pieyedi/4 then
   ' rainbow=rgb(0*casual,255*casual,255*casual)
'else
'    rainbow=rgb(238*casual,130*casual,238*casual)
'    end if
' rainbow=wave2rgb(400+(waverycalc-((waverycalc)*pointidx*100/numberofpoints/100)))
'if casual>.5 then 
    rainbow=rgb(rnd*255,rnd*255,0)'rgb(255,251,0)'RGB (208, 83, 64)'rgb(255,255,0)'153,101,21)'&h00d024'rgb(153, 101, 21)
    'dim run as integer
   if MyArray(pointIdx).X<0 and game2<>0 then rainbow=rgb(0,0,255*casual)
    if MyArray(pointIdx).X>0 and game2<>0 then rainbow=rgb(255*casual,0,0)
'rainbow=wave2rgb(rnd*(651-400))

'else
   ' casual=.5-casual
   ' casual=.5+casual
 '   rainbow=RGB(0,0,0)
  '  end if
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )
'if game2=0 then
rainbow=rgb(d*casual,n*casual,e*casual)
'casual=casual*1.33:if casual>1 then casual=1
'else
'locate 1,1:print "RED "+str(d)+"GREEN "+str(n)+"BLUE "+str(e)

'    rainbow=rgb(0,255*casual,255*casual)
'end if
 screenlock
  tier=1
rem   if ((slphouse<(pieyedi*2)+(pieyedi*2/20)-(pieyedi/3)) and (slphouse>(pieyedi*2)+(pieyedi*2/20)-(pieyedi/2)-(pieyedi/3))) or ((slphouse<(pieyedi*2)+(pieyedi*2/20)-pieyedi-(pieyedi/3)) and (slphouse>(pieyedi*2)+(pieyedi*2/20)-pieyedi-(pieyedi/2)-(pieyedi/3))) then
 rem  tier=0   		
         rem     circle (newX+((1280)/2), newy+((720)/2)),3,rgb(0,0,0)'rainbow'rgb(255*casual,255*casual,0*casual)', rgb(d*casual,n*casual,e*casual)
                	'	circle (newX+((1280)/2), newy+((720)/2)),2, rgb(d*casual,n*casual,e*casual)
     	rem	circle (newX+((1280)/2), newy+((720)/2)),2,rgb(0,0,0)
rem  else
 rem   tier=1
 'flatx<>+:flaty=0
 'if newy+((720)/2)<=(720)/2 and game=0 then
 '   circle (newX+((1280)/2), newy+((720)/2)),3,rainbow'rgb(238*casual,127+3*casual,238*casual)'rainbow'rgb(255*casual,255*casual,0*casual)', rgb(d*casual,n*casual,e*casual)
                	'	circle (newX+((1280)/2), newy+((720)/2)),2, rgb(d*casual,n*casual,e*casual)
 '   		circle (newX+((1280)/2), newy+((720)/2)),2,rainbow'rgb(238*casual,127+3*casual,238*casual)'rg
   
rem  end if


'circle(newx+((1280)/2),newy+((720)/2)),1,rainbow'rgb(255,255,255)'255,251,0)'rgb(238*casual,127+3*casual,238*casual)'rgb(255*casual,251*casual,0*casual)
'else
'if newx<>(1280)/2 and newy<>(720)/2 then
circle (newX+((1280)/2), newy+((720)/2)),1,rainbow,,,,F'rgb(238*casual,127+3*casual,238*casual)'rainbow'rgb(255*casual,255*casual,0*casual)', rgb(d*casual,n*casual,e*casual)
                		'circle (newX+((1280)/2), newy+((720)/2)),2, rgb(d*casual,n*casual,e*casual)
 '   		circle (newX+((1280)/2), newy+((720)/2)),2,rainbow'rgb(238*casual,127+3*casual,238*casual)'rg
   
rem  end if


'circle(newx+((1280)/2),newy+((720)/2)),1,rainbow'rgb(255,255,255)'255,251,0)'rgb(238*casual,127+3*casual,238*casual)'rgb(255*casual,251*casual,0*casual)    
'end if
screenunlock

	END IF
NEXT
ender2:
end if
    dim as integer bottomzzz2,topzzz2
dim as SINGLE zzzz
if game2=1 or game2=12 then
    if tempgunx=0 then tempgunx=1
 '   tempgunx=tempgunx+1
if tempguny=0 then tempguny=1
bottomzzz2=int(bottomzzz*5*12)
topzzz2=int(topzzz*5*12)
'for tempgunx=1 to 1280 'step int(rnd*2)+1
'    for tempguny=1 to 720
tempgunz=1
for calca=1 to 720/2
    tempgunx=topgunx(calca)
    tempguny=topguny(calca)
    tempgunx2=bottomgunx(calca)
    tempguny2=bottomguny(calca)
if topgun(tempgunx,tempguny,tempgunz)>0 and topgun3(tempgunx,tempguny)<=bottomzzz2 then 
    zzzz=(topgun3(tempgunx,tempguny))/12'*25/100'*25/100'/4

    circle(tempgunx,tempguny),1+((4)*zzzz*1),rgb(255,7,7),,,,F
'if topgun(tempgunx,tempguny,tempgunz)=1 then circle(tempgunx,tempguny),int(4*(zzzz)),rgb(255,7,7),,,,f
end if
if bottomgun(tempgunx2,tempguny2,tempgunz)>0 and bottomgun3(tempgunx2,tempguny2)<=topzzz2 then 
                   zzzz=(bottomgun3(tempgunx2,tempguny2))/12'*25/100'*25/100'/4

                   circle(tempgunx2,tempguny2),1+((4)*zzzz*1),rgb(255,7,7),,,,F
'if topgun(tempgunx,720-tempguny)>3 then line(tempgunx-3,720-tempguny-3)-(tempgunx+3,720-tempguny+3),rgb(255,127,0),bf
'                if bottomgun(tempgunx,tempguny)>2 then line(tempgunx-3,tempguny-3)-(tempgunx+3,tempguny+3),rgb(255,127,0),bf
end if

next
'next
end if
    
    
    
    
    
  If judgedtop>timer and judgedtop2<timer then judgedtop2=judgedtop-33'int(timer)+.5
If judgedbottom>timer and judgedbottom2<timer then judgedbottom2=judgedbottom2-33'int(timer)+.5
  
    
    
    

if game2<>1 and game2<>12 then shipsflag=0
'dim as SINGLE ship1=0-(pieyedi*2/20),ship2=0-(pieyedi*2/20),ship1y=-(pieyedi*2/20)-pieyedi,ship2y=ship2
'if astrocnt=3 and (game2=1 or game2=12) then
if (game2=1 or game2=12) and shipsflag=1 and tiesaucer=1 Then
dim as SINGLE shade1,shade2

for pointidx3=1 to 2
   if pointidx3=1 then
                   ' topzzz3=topzzz*4
'topzzz3=topzzz
      '  if topzzz<0 then topzzz3=(topzzz-(topzzz*2))
               ' if topzzz>0 then topzzz3=(topzzz-(topzzz*2))

'ship(pointidx).z=1
'topzzz3=topzzz3*2
'if topzzz3<.25/2 then topzzz3=.25/2
'if topzzz3>.25 then topzzz3=.25
'if topzzz3<.5 then topzzz3=topzzz3*2  
'topzzz3=topzzz3/2
'flatx=ship(pointidx).x*((65)*((topzzz3*4)+.25))/(ship(pointidx).z+zoomlevel)
'SHADEA=ship1rotatex*100/(pieyedi/2)/100
'IF SHIP1ROTATEX>=0 THEN 
    SHADE1=(SHIP1ROTATEX*100/(PIEYEDI/2)/100)
'ELSEIF SHIP1ROTATEX<0 THEN
'     SHADE1=1.5+ship(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'     END IF
   
':shade1=shade1-(shade1*2)
'shade1=.5-SHADEA*4
'flaty=ship(pointidx).y*((33+9)*((topzzz3*4)+.25))/(ship(pointidx).z+zoomlevel)
'IF SHIP1ROTATEY>0 THEN 
   ' SHADEA=(SHIP(POINTIDX).X*sin(SHIP1ROTATEY-(pieyedi/2)))+shade1
'ELSEIF SHIP1ROTATEY<0 THEN
'     SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'     END IF
'flatz=ship(pointidx).Z*256*((topzzz3*4))'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).z+zoomlevel)'((3)
else
           ' bottomzzz=bottomzzz*4
'bottomzzz3=bottomzzz
           ' if bottomzzz<0 then bottomzzz=1-(bottomzzz-(bottomzzz*2))
'if bottomzzz<0 then bottomzzz3=(bottomzzz-(bottomzzz*2))
'bottomzzz3=bottomzzz3*2
'if bottomzzz3<.25/2 then bottomzzz3=.25/2
'if bottomzzz3>.25 then bottomzzz3=.25'-.1
'flatx=ship(pointidx).x*((65)*((bottomzzz3*4)+.25))/(ship(pointidx).z+zoomlevel)
'IF SHIP2ROTATEX>=0 THEN 
    SHADE2=(SHIP2ROTATEX*100/(PIEYEDI/2)/100)
'ELSEIF SHIP2ROTATEX<=0 THEN    
'    SHADE2=1.5+ship(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'     END IF
'flaty=ship(pointidx).y*((33+9)*((bottomzzz3*4)+.25))/(ship(pointidx).z+zoomlevel)
'IF SHIP2ROTATEY>0 THEN 
    'SHADEB=(SHIP(POINTIDX).X*sin(SHIP2ROTATEY-(pieyedi/2)))+shade2
'ELSEIF SHIP2ROTATEY<0 THEN
'     SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'     END IF
'flatz=ship(pointidx).Z*256*((bottomzzz3*4))'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).z+zoomlevel)'((3)
end if
'flatx=newx
' flaty=newy
' flatz=newz
 'newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz





'flatx2=flatx
'flaty2=flaty
'flatz2=flatz'*((1280)/2)
'if pointidx3=1 then
 '   if singletopx=0 then ship1rotatex=0
  '  if (singletopx<0 and ship1rotatex<=-(pieyedi/2)+(pieyedi/360)) or (singletopx>0 and ship1rotatex><=(pieyedi/2)-(pieyedi/360)) then
  '          ship1rotatey=ship1rotatey
  '          else
    'if singletopx<0 and ship1rotatex>-pieyedi/2 then ship1rotatex=ship1rotatex-(pieyedi/360/50*(tx))
    'if singletopx>0 and ship1rotatex<+pieyedi/2 then ship1rotatex=ship1rotatex+(pieyedi/360/50*(tx))
   
   'if singletopx<0 and ship1rotatex>-pieyedi/3.33 then ship1rotatex=ship1rotatex-(pieyedi/(720*64*2)/16)
   ' if singletopx>0 and ship1rotatex<+pieyedi/3.33 then ship1rotatex=ship1rotatex+(pieyedi/(720*64*2)/16)
   
   
   ' end if
'newx = (cos(ship1rotatex) * flatx) - (sin(ship1rotatex) * flatz)
'newy=flaty''y = y'

'end if
'if singletopy<0 or singletopy>0 or singletopy=0 then
'if singletopy=0 then ship1rotatey=0'pieyedi
    '    if ship1rotatey>=(pieyedi/2)-(pieyedi/360/50) or ship1rotatey<=-(pieyedi/2)-(pieyedi/360/50) then
    '        ship1rotatey=ship1rotatey
    '        else

  '  if singletopy<0 and ship1rotatey>-pieyedi/3.33 then ship1rotatey=ship1rotatey-(pieyedi/(720*64*2)/16)
  '  if singletopy>0 and Ship1rotatey<+pieyedi/3.33 then ship1rotatey=ship1rotatey+(pieyedi/(720*64*2)/16)
'end if

'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' newx = flatx'newx'

'newy = (sin(ship1rotatey)*flatz) + (cos(ship1rotatey) * flaty)

'newy = (sin(0)*flatz) + (cos(0) * flaty)

'end if

'if pointidx3=2 then
  ' if singlebottomx=0 then ship2rotatex=0
  '  if (singlebottomx<0 and ship1rotatex<=-(pieyedi/2)+(pieyedi/360/50)) or (singlebottomx>0 and ship1rotatex><=(pieyedi/2)-(pieyedi/360/50)) then
  '          ship1rotatey=ship1rotatey
  '          else
    'if singlebottomx<0 and ship2rotatex>-pieyedi/2 then ship2rotatex=ship2rotatex-(pieyedi/360/50*(bx))
    'if singlebottomx>0 and ship2rotatex<+pieyedi/2 then ship2rotatex=ship2rotatex+(pieyedi/360/50*(bx))
  '  if singlebottomx<0 and ship2rotatex>-pieyedi/3.33 THEN ship2rotatex=ship2rotatex-(pieyedi/(720*64*2)/16)':shade2=ship2rotatex*100/(pieyedi/2)/100:shade2=shade2-(shade2*2)
  '  if singlebottomx>0 AND ship2rotatex<+pieyedi/3.33 then ship2rotatex=ship2rotatex+(pieyedi/(720*64*2)/16)':shade2=ship2rotatex*100/(pieyedi/2)/100
   ' end if
'newx2 = (cos(ship2rotatex) * flatx) - (sin(ship2rotatex) * flatz)
'newy=flaty''y = y'

'end if
'if singlebottomy<0 or singlebottomy>0 or singlebottomy=0 then
'if singlebottomy=0 then ship2rotatey=0'pieyedi
    
    '    if ship1rotatey>=(pieyedi/2)-(pieyedi/360/50) or ship1rotatey<=-(pieyedi/2)-(pieyedi/360/50) then
    '        ship1rotatey=ship1rotatey
    '        else

 '   if singlebottomy<0 and ship2rotatey>-pieyedi/3.33 then ship2rotatey=ship2rotatey-(pieyedi/(720*64*2)/16)
 '   if singlebottomy>0 and ship2rotatey<+pieyedi/3.33 then ship2rotatey=ship2rotatey+(pieyedi/(720*64*2)/16)
'end if

'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' newx = flatx'newx'


'newy2 = (sin(0+pieyedi)*flatz) + (cos(0+pieyedi) * flaty)

'newy2 = (sin(ship2rotatey)*flatz) + (cos(ship2rotatey+pieyedi) * flaty)


'end if
'newz = (cos(ship1rotatex) * flatz) - (sin(ship1rotatex) * flaty)
 'flatx=newx
 'flaty=newy
 'flatz=newz
 'newy=flaty
 rem xyrotatecode
' alphyx=ship(pointidx).x*cos(ship1rotatex-pieyedi/4)
'  alphyy=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
'  if singletopy<1 and singletopy>0 then singletopy=0
'  if singlebottomy<1 and singlebottomy>0 then singlebottomy=0

' x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')

'The second set of algorithms is for rotation about the y axis:

'x = (cos(angle) * x') - (sin(angle) * z')
'y = y'
'z = (sin(angle) * x') + (cos(angle) * z')

 'newx=flatx
 'newy2=newy
 'newx2=newx
' newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz
 
' newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
'newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
'newz = flatz'
' flatx=newx
' flaty=newy
' flatz=newz
 
' alphyx=ship(pointidx).x*cos(ship1rotatex)
'  alphyx=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' dim as SINGLE aly1,aly2
 ' aly1=ship(pointidx).x*cos(ship2rotatex)
   ' aly1=ship(pointidx).y*cos(ship2rotatey-pieyedi/4)
'casual=alphyx'y*(100/1/30)
'casual2=aly1'2*(100/1/30)
'casual=casual*4
'casual2=casual2*4
'if casual>1 then casual=1
'if casual<0 then casual=0
'if casual2>1 then casual2=1
'if casual2<0 then casual2=0

casual=1*(topzzz*5)'casual+5
casual2=1*(bottomzzz*5)'=casual2+5
if casual<.2 then casual=.2
if casual2<.2 then casual2=.2
'casual=casual*shade1'*SHADEA
'casual2=casual2*shade2'*SHADEB

'casual=1'-((1)*flatz*100/((1280)/2)/100)
'dim x as SINGLE
 
 'rernd2:
' x=rnd'(timered)
 'if x=0 then goto rernd2
'if x<.6 then casual=casual-.08'03333333
' if x>.5 then casual=casual+.08'03333333
'if (pieyedi*2/20)+slphouse'1*(pieyedi*2/20)-(slphouse-pieyedi/2)*100/(pieyedi*2/20-pieyedi/2)/100'(100/2/100)
          if casual>1 then casual=1
          if casual<0 then casual=0
if casual2>1 then casual2=1
if casual2<0 then casual2=0
    'rainbow=ship(pointidx).c'rgb(255,255,251)
   
'd = RGBA_R( rainbow )
'n = RGBA_G( rainbow )
'e = RGBA_B( rainbow )
dim as integer shad1',shad2,shada,shadb

'rainbow=rgb(192*casual,192*casual,192*casual)',127)
if pointidx3=1 then
 for shad1=0 to 64'0 step -1
     
    rainbow=rgb(255*(casual*((shad1)*(100/64/100))),255*(casual*((shad1)*(100/64/100))),255*(casual*((shad1)*(100/64/100))))',127)
 if judgedtop2>timer then rainbow=rgb(255,127,0)
 screenlock

   circle ((topxxx), (topyyy)),(64-shad1)*(topzzz*5),rainbow,,,.5,F'+ship1rotatex,F
    screenunlock

next shad1
for shad1=0 to 14'0 step -1
 'shad1=14   
        rainbow=rgb(0,255,255)',127)
  screenlock

   circle ((topxxx), (topyyy)),(14-shad1)*(topzzz*5),rainbow,,,.5,F'+ship1rotatex,F
   screenunlock
next shad1
   'screenunlock
   else
   'rainbow=rgb(192*casual2,192*casual2,192*casual2)',127)
'screenlock
    for shad1=0 to 64' step -1
    rainbow=rgb(255*(casual2*((shad1)*(100/64/100))),255*(casual2*((shad1)*(100/64/100))),255*(casual2*((shad1)*(100/64/100))))',127)
 if judgedbottom2>timer then rainbow=rgb(255,127,0)
 screenlock
   circle ((bottomxxx), (bottomyyy)),(64-shad1)*(bottomzzz*5),rainbow,,,.5,F
   screenunlock
next shad1
for shad1=0 to 14'0 step -1
 'shad1=14   
        rainbow=rgb(0,255,255)',127)
screenlock
   circle ((bottomxxx), (bottomyyy)),(14-shad1)*(bottomzzz*5),rainbow,,,.5,F
screenunlock
next shad1
'circle(newx2+(bottomxxx),newy2+(bottomyyy)+(33+9)),1.3343*(bottomzzz*4*7*100/(7/2)/100),rainbow,,,,F
'screenunlock
end if
next
end if
if (game2=1 or game2=12) and shipsflag=1 and tiesaucer=0 Then
    zoomlevel=1'0'6.33'9'6.5'4'3.5'4.66'10'2.5
'if singletopx<0 or singletopx>0 or singletopx=0 then
  '  if (singletopx<0 and ship1rotatex<=-(pieyedi/2)+(pieyedi/360)) or (singletopx>0 and ship1rotatex><=(pieyedi/2)-(pieyedi/360)) then
  '          ship1rotatey=ship1rotatey
  '          else
    'if singletopx<0 and ship1rotatex>-pieyedi/2 then ship1rotatex=ship1rotatex-(pieyedi/360/50*(tx))
    'if singletopx>0 and ship1rotatex<+pieyedi/2 then ship1rotatex=ship1rotatex+(pieyedi/360/50*(tx))
 if timer>judgedtop then  
If singleTOPx<0 then ship1rotatex=SHIP1ROTATEX-(pieyedi*2/(360/32)):IF SHIP1ROTATEX<-(pieyedi*2)+(pieyedi*2/(360/32)) THEN SHIP1ROTATEX=SHIP1ROTATE+(pieyedi*2)'-pieyedi-ship2rotatex'-pieyedi
If singletopx>0 then ship1rotatex=SHIP1ROTATEX+(pieyedi*2/(360/32)):IF SHIP1ROTATEX>(pieyedi*2)-(pieyedi*2/(360/32)) THEN SHIP1ROTATEX=SHIP1ROTATE-(PIEYEDI*2)'pieyedi*2'pieyedi'SHIP2ROTATEX
if ship1rotatex<0 then ship1rotatex=ship1rotatex+(pieyedi*2/(360/32))/4
if ship1rotatex>0 then ship1rotatex=ship1rotatex-(pieyedi*2/(360/32))/4
 Else
    if Rnd>.49 and rnd>.49 then
        singletopz=singletopz+1
    else
        singletopz=singletopz-1
        end if
    if topxxx<1280/2 then 
       ship1rotatex=ship1rotatex+(pieyedi/(64))*720
       if rnd>.49 and rnd>.49 then
       SINGLEtopX=SINGLEtopX+(pieyedi*2/(360))*1.5
   else
       SINGLEtopX=SINGLEtopX-(pieyedi*2/(360))*1.5
   end if
else
   
 ship1rotatex=ship1rotatex-(pieyedi/(64))*720 
 if rnd>.49 and rnd>.49 then
       SINGLEtopX=SINGLEtopX+(pieyedi*2/(360))*1.5
   else
       SINGLEtopX=SINGLEtopX-(pieyedi*2/(360))*1.5
   end if
    end If
    singletopx=0
end if
    if singletopx=0 then ship1rotatex=0'-pieyedi*2/20

'singletopx=ship1rotatex 
  '  if singletop=0 then ship1rotate=0
  '  if (singletopx<0 and ship1rotatex<=-(pieyedi/2.10)+(pieyedi/360)) or (singletopx>0 and ship1rotatex><=(pieyedi/2.10)-(pieyedi/360)) then
  '          ship1rotatey=ship1rotatey
  '          else
    'if singletopx<0 and ship1rotatex>-pieyedi/2.10 then ship1rotatex=ship1rotatex-(pieyedi/360/50*(tx))
    'if singletopx>0 and ship1rotatex<+pieyedi/2.10 then ship1rotatex=ship1rotatex+(pieyedi/360/50*(tx))
   
  '  if singletop<0 and ship1rotate>-pieyedi/10 then ship1rotate=ship1rotate-(pieyedi/(720/25))
  '  if singletop>0 and ship1rotate<+pieyedi/10 then ship1rotate=ship1rotate+(pieyedi/(720/25))
   
   
 '  if singletopx<0 and ship1rotatex>-pieyedi/2.10 then ship1rotatex=ship1rotatex-(pieyedi/(720/25))
 '   if singletopx>0 and ship1rotatex<+pieyedi/2.10 then ship1rotatex=ship1rotatex+(pieyedi/(720/25))
   ' end if
'newx = (cos(ship1rotatex) * flatx) - (sin(ship1rotatex) * flatz)
'newy=flaty''y = y'

'end if

'if singletopy<0 or singletopy>0 or singletopy=0 then
    '    if ship1rotatey>=(pieyedi/2.10)-(pieyedi/360/50) or ship1rotatey<=-(pieyedi/2.10)-(pieyedi/360/50) then
    '        ship1rotatey=ship1rotatey
    '        else
'ship1rotatey=0
'if singletop<0 and ship1rotate>-pieyedi/10 then ship1rotate=ship1rotate-(pieyedi/(720/25))
 '   if singletop>0 and ship1rotate<+pieyedi/10 then ship1rotate=ship1rotate+(pieyedi/(720/25))
 ' singletopy=0
  if timer>judgedtop then
   if singletopy>0 then ship1rotatey=ship1rotatey-(pieyedi*2/(360*(1.25*2*20))):IF SHIP1ROTATEy<-(pieyedi/(8)) THEN SHIP1ROTATEy=-(pieyedi/(8))
   if singletopy<0 then ship1rotatey=ship1rotatey+(pieyedi*2/(360*(1.25*2*20))):IF SHIP1ROTATEy>+(pieyedi/(8)) THEN SHIP1ROTATEy=+(pieyedi/(8))




'ship1rotatey=0
'SINGLETOPY=0
'end if



If ship1rotatey<0 then ship1rotatey=ship1rotatey+(pieyedi/(360*(1.25*2*(10*4))))
If ship1rotatey>0 then ship1rotatey=ship1rotatey-(pieyedi/(360*(1.25*2*(10*4))))
'If SHIP1ROTATEY>-(pieyedi*2/(360*(1.25*2*20))) AND SHIP1ROTATEY<(pieyedi*2/(360*(1.25*2*5))) THEN SHIP1ROTATEY=0
  Else
'  	If Rnd>.93 Then ship1rotatey=pieyedi*2*Rnd
    if topyyy<720/4 then
       ship1rotatey=ship1rotatey+(pieyedi/(64))*720
if rnd>.49 and rnd>.49 then
       SINGLEtopy=SINGLEtopy+(pieyedi*2/(360))*1.5
   else
       SINGLEtopy=SINGLEtopy-(pieyedi*2/(360))*1.5
   end if 
   else
     ship1rotatey=ship1rotatey-(pieyedi/(64))*720
if rnd>.49 and rnd>.49 then
       SINGLEtopy=SINGLEtopy+(pieyedi*2/(360))*1.5
   else
       SINGLEtopy=SINGLEtopy-(pieyedi*2/(360))*1.5
   end if
    end If
    singletopy=0
  end If
  if singletopy=0 then ship1rotatey=0'-pieyedi*2/20'pieyedi
'ship1rotatey=0'-pieyedi*2/20'pieyedi

'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' newx = flatx'newx'
'end if
'newy = (sin(ship1rotatey)*flatz) + (cos(ship1rotatey) * flaty)

'newy = (sin(0)*flatz) + (cos(0) * flaty)

'end if

'if singlebottomx<0 or singlebottomx>0 or singlebottomx=0 then
  '  if (singlebottomx<0 and ship1rotatex<=-(pieyedi/3.10)+(pieyedi/360/50)) or (singlebottomx>0 and ship1rotatex><=(pieyedi/3.10)-(pieyedi/360/50)) then
  '          ship1rotatey=ship1rotatey
  '          else
  if timer>judgedbottom then
    'if singlebottomx<0 and ship2rotatex>-pieyedi/3.10 then ship2rotatex=ship2rotatex-(pieyedi/360/50*(bx))
    'if singlebottomx>0 and ship2rotatex<+pieyedi/3.10 then ship2rotatex=ship2rotatex+(pieyedi/360/50*(bx))
'REM  'if singlebottomx<0 then ship2rotatex=ship2rotatex-(pieyedi/(360*(1.25*5))):IF ship2ROTATEX<-(pieyedi/6) THEN ship2ROTATEX=-(pieyedi/6)
'REM'if singlebottomx>0 then ship2rotatex=ship2rotatex+(pieyedi/(360*(1.25*5))):IF ship2ROTATEX>(pieyedi/6) THEN ship2ROTATEX=(pieyedi/6)
If singlebottomx<0 then ship2rotatex=ship2ROTATEX-(pieyedi*2/(360/32)):IF ship2ROTATEX<-(pieyedi*2)+(pieyedi*2/(360/32)) THEN ship2ROTATEX=ship2ROTATE+(pieyedi*2)'-pieyedi-ship2rotatex'-pieyedi
If singlebottomx>0 then ship2rotatex=ship2ROTATEX+(pieyedi*2/(360/32)):IF ship2ROTATEX>(pieyedi*2)-(pieyedi*2/(360/32)) THEN ship2ROTATEX=ship2ROTATE-(PIEYEDI*2)'pieyedi*2'pieyedi'SHIP2ROTATEX
'if ship1rotatex<0 then ship1rotatex=ship1rotatex+(pieyedi*2/(360/32))/4
'if ship1rotatex>0 then ship1rotatex=ship1rotatex-(pieyedi*2/(360/32))/4

if ship2rotatex<0 then ship2rotatex=ship2rotatex+(pieyedi*2/(360/32))/4
if ship2rotatex>0 then ship2rotatex=ship2rotatex-(pieyedi*2/(360/32))/4
else

 if Rnd>.49 and rnd>.49 then
        singlebottomz=singlebottomz+1
    else
        singlebottomz=singlebottomz-1
        end if

 if topxxx<1280/2 then 
       ship2rotatex=ship2rotatex+(pieyedi/(64))*720
       if rnd>.49 and rnd>.49 then
       SINGLEbottomX=SINGLEbottomX+(pieyedi*2/(360))*1.5
   else
       SINGLEbottomX=SINGLEbottomX-(pieyedi*2/(360))*1.5
   end if
   else
 ship2rotatex=ship2rotatex-(pieyedi/(64))*720 
if rnd>.49 and rnd>.49 then
       SINGLEbottomX=SINGLEbottomX+(pieyedi*2/(360))*1.5
   else
       SINGLEbottomX=SINGLEbottomX-(pieyedi*2/(360))*1.5
   end if
 end If
 singlebottomx=0
end if
If singlebottomx=0 then ship2rotatex=0'-pieyedi*2/20

'if singlebottomy<0 or singlebottomy>0 or singlebottomy=0 then
'    ship2rotatey=0
    '    if ship1rotatey>=(pieyedi/3.10)-(pieyedi/360/50) or ship1rotatey<=-(pieyedi/3.10)-(pieyedi/360/50) then
    '        ship1rotatey=ship1rotatey
    '        else
'if singlebottom<0 and ship2rotate>-pieyedi/10 then ship2rotate=ship2rotate-(pieyedi/(720/25))
'    if singlebottom>0 and ship2rotate<+pieyedi/10 then ship2rotate=ship2rotate+(pieyedi/(720/25))
'if singlebottomy<0 and ship2rotatey>-(pieyedi/(360/2)) then ship2rotatey=ship2rotatey-(pieyedi/3):IF SHIP2ROTATEy<-(PIEYEDI/3) THEN SHIP2ROTATEy=SHIP2ROTATEy+(PIEYEDI/3)
'   if singlebottomy>0 and Ship2rotatey<+(pieyedi/(360/2)) then ship2rotatey=ship2rotatey+(pieyedi/3):IF SHIP2ROTATEy>(PIEYEDI/3) THEN SHIP2ROTATEy=SHIP2ROTATEy-(PIEYEDI/3)
'if singlebottomy<0 and ship2rotatey>-pieyedi/10 then ship2rotatey=ship2rotatey-(pieyedi/(720/25))
'if singlebottomy>0 and ship2rotatey<+pieyedi/10 then ship2rotatey=ship2rotatey+(pieyedi/(720/25))
'end if
'if singletop=0 then ship1rotate=0
  '  if (singletopx<0 and ship1rotatex<=-(pieyedi/3.10)+(pieyedi/360)) or (singletopx>0 and ship1rotatex><=(pieyedi/3.10)-(pieyedi/360)) then
  '          ship1rotatey=ship1rotatey
  '          else
  
 ' singlebottomy=0
  if timer>judgedbottom then
    'if singletopx<0 and ship1rotatex>-pieyedi/3.10 then ship1rotatex=ship1rotatex-(pieyedi/360/50*(tx))
    'if singletopx>0 and ship1rotatex<+pieyedi/3.10 then ship1rotatex=ship1rotatex+(pieyedi/360/50*(tx))
'SINGLEBOTTOMY=0
If singlebottomy>0 then ship2rotatey=ship2rotatey-(pieyedi*2/(360*(1.25*2*20))):IF ship2ROTATEy<-(pieyedi/(8)) THEN ship2ROTATEy=-(pieyedi/(8))
   if singlebottomy<0 then ship2rotatey=ship2rotatey+(pieyedi*2/(360*(1.25*2*20))):IF ship2ROTATEy>+(pieyedi/(8)) THEN ship2ROTATEy=+(pieyedi/(8))
'ship2rotatey=0
'If Rnd>.93 Then ship1rotatey=pieyedi*2*rnd
  '  if singletop<0 then ship1rotate=ship1rotate-(pieyedi/(720/25))
  '  if singletop>0 then ship1rotate=ship1rotate+(pieyedi/(720/25))
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' newx = flatx'newx'
If ship2rotatey<0 then ship2rotatey=ship2rotatey+(pieyedi/(360*(1.25*2*(10*4))))
If ship2rotatey>0 then ship2rotatey=ship2rotatey-(pieyedi/(360*(1.25*2*(10*4))))
'IF SHIP2ROTATEY>-(pieyedi*2/(360*(1.25*2*5))) AND SHIP2ROTATEY<(pieyedi*2/(360*(1.25*2*5))) THEN SHIP2ROTATEY=0
'newy2 = (sin(0+pieyedi)*flatz) + (cos(0+pieyedi) * flaty)
else
'newy2 = (sin(ship2rotatey)*flatz) + (cos(ship2rotatey+pieyedi) * flaty)
 If Rnd>.93 Then ship2rotatey=pieyedi*2*rnd
 if topyyy<720/4 then
       ship2rotatey=ship2rotatey-(pieyedi/(64))*720
'if timer<
       if rnd>.49 and rnd>.49 then
       SINGLEbottomy=SINGLEbottomy-(pieyedi*2/(360))*1.5
 else
     SINGLEbottomy=SINGLEbottomy+(pieyedi*2/(360))*1.5
'     
end if
else 
         ship2rotatey=ship2rotatey+(pieyedi/(64))*720
'   '  ship2rotatey=ship2rotatey+(pieyedi*2/(360*(1.25*2*5)))
if rnd>.49 and rnd>.49 then
       SINGLEbottomy=SINGLEbottomy-(pieyedi*2/(360))*1.5
 else
     SINGLEbottomy=SINGLEbottomy+(pieyedi*2/(360))*1.5
     
end if
 end If
 singlebottomy=0
end if
If singlebottomy=0 then ship2rotatey=0'-pieyedi*2/20'pieyedi
'ship2rotatey=0'-pieyedi*2/20
'newz = (cos(ship1rotatex) * flatz) - (sin(ship1rotatex) * flaty)
 'flatx=newx
 'flaty=newy
 'flatz=newz
 'newy=flaty
' rem xyrotatecode
' alphyx=ship(pointidx).x*cos(ship1rotatex-pieyedi/4)
'  alphyy=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' if singletopy<1 and singletopy>0 then singletopy=0
 ' if singlebottomy<1 and singlebottomy>0 then singlebottomy=0
'goto jumpship

'jumpship:
'if timer>judgedbottom then toybottom=0 else toybottom=toybottom+.125:end if
'if timer>judgedtop then toytop=0 else toytop=toytop+.125:end if
'if judgedbottom>timer and toybottom>360 then toybottom=0+.125
'if judgedtop>timer and toytop>360 then toytop=0+.125
dim as integer pointa,pointb
'dim as single ram
'ram=1'rnd
'if ram<.34 then
'pointa=int(rnd*(pixelagent2/2))+1
'pointa=1
'pointb=int(pixelagent2/2)+1'int(rnd*(pixelagent2/2))+1
'elseif ram>.33 and ram<.67 then
    
pointa=1'int(pixelagent2)
pointb=pixelagent2
'else
    pointa=1
    pointb=pixelagent2
'end if

'EndIf
   ' rainbow=rgb(255*casual2,255*casual2,0)
 If judgedbottom>Timer And choicedrivea=0 Then
	choicedrivea=1
'EndIf 
 choicestbottom=Int(Rnd*4)+1
'  choicesttop=Int(Rnd*4)+1
End If
 If judgedtop>Timer And choicedriveb=0 Then
	choicedriveb=1
'EndIf 
' choicestbottom=Int(Rnd*4)+1
  choicesttop=Int(Rnd*4)+1
End If

'  choicestbottom=1
If judgedbottom>timer Then
	'Dim As Integer choicestbottom,choicesttop
'EndIf
   ' rainbow=rgb(255*casual2,255*casual2,0)
 'choicestbottom=Int(Rnd*4)+1
 ' choicesttop=Int(Rnd*4)+1
'  choicestbottom=1
'  choicesttop=3
 If choicestbottom<2 Then
   blowbottomrx=blowbottomrx+(blowbottomrx+tripper96a+blowbottomrx*(bottomzzz*5))
   
   blowbottomry=blowbottomry+(blowbottomry+tripper96a+blowbottomry*(bottomzzz*5))
 end If
 If choicestbottom=2 Then
   blowbottomrx=blowbottomrx-(blowbottomrx+tripper96a+blowbottomrx*(bottomzzz*5))
   
   blowbottomry=blowbottomry-(blowbottomry+tripper96a+blowbottomry*(bottomzzz*5))
 end If
 If choicestbottom=3 Then
   blowbottomrx=blowbottomrx-(blowbottomrx+tripper96a+blowbottomrx*(bottomzzz*5))
   
   blowbottomry=blowbottomry+(blowbottomry+tripper96a+blowbottomry*(bottomzzz*5))
 end If
 If choicestbottom>3 Then
   blowbottomrx=blowbottomrx+(blowbottomrx+tripper96a+blowbottomrx*(bottomzzz*5))
   
   blowbottomry=blowbottomry-(blowbottomry+tripper96a+blowbottomry*(bottomzzz*5))
 end If
End If
if judgedtop>timer then
     '   rainbow=rgb(255*casual,255*casual,0)
  '  Dim As Integer choicestbottom,choicesttop
'EndIf
  '  rainbow=rgb(255*casual2,255*casual2,0)
 'choicestbottom=Int(Rnd*4)+1
 ' choicesttop=Int(Rnd*4)+1
' choicestbottom=1
'  choicesttop=3
     If choicesttop<2 Then
        blowtoprx=blowtoprx+(blowtoprx+tripper96b+blowtoprx*(topzzz*5))
   blowtopry=blowtopry-(blowtopry+tripper96b+blowtopry*(topzzz*5))
     End If
      If choicesttop=2 Then
        blowtoprx=blowtoprx-(blowtoprx+tripper96b+blowtoprx*(topzzz*5))
   blowtopry=blowtopry+(blowtopry+tripper96b+blowtopry*(topzzz*5))
      End If
       If choicesttop=3 Then
        blowtoprx=blowtoprx+(blowtoprx+tripper96b+blowtoprx*(topzzz*5))
   blowtopry=blowtopry+(blowtopry+tripper96b+blowtopry*(topzzz*5))
       End If
        If choicesttop>3 Then
        blowtoprx=blowtoprx-(blowtoprx+tripper96b+blowtoprx*(topzzz*5))
   blowtopry=blowtopry-(blowtopry+tripper96b+blowtopry*(topzzz*5))
        End If
End If
If judgedbottom>timer Then
	'Dim As Integer choicestbottom,choicesttop
'EndIf
   ' rainbow=rgb(255*casual2,255*casual2,0)
 'choicestbottom=Int(Rnd*4)+1
 ' choicesttop=Int(Rnd*4)+1
 
 'choicestbottom=1
 ' choicesttop=3
 
 
 If choicestbottom<2 Then
   blowbottomlx=blowbottomlx-(blowbottomlx+tripper96a+blowbottomlx*(bottomzzz*5))
   
   blowbottomly=blowbottomly-(blowbottomly+tripper96a+blowbottomly*(bottomzzz*5))
 end If
 If choicestbottom=2 Then
   blowbottomlx=blowbottomlx+(blowbottomlx+tripper96a+blowbottomlx*(bottomzzz*5))
   
   blowbottomly=blowbottomly+(blowbottomly+tripper96a+blowbottomly*(bottomzzz*5))
 end If
 If choicestbottom=3 Then
   blowbottomlx=blowbottomlx+(blowbottomlx+tripper96a+blowbottomlx*(bottomzzz*5))
   
   blowbottomly=blowbottomly-(blowbottomly+tripper96a+blowbottomly*(bottomzzz*5))
 end If
 If choicestbottom>3 Then
   blowbottomlx=blowbottomlx-(blowbottomlx+tripper96a+blowbottomlx*(bottomzzz*5))
   
   blowbottomly=blowbottomly+-(blowbottomly+tripper96a+blowbottomly*(bottomzzz*5))
 end If
End If
if judgedtop>timer then
     '   rainbow=rgb(255*casual,255*casual,0)
   ' If judgedbottom>timer Then
	'Dim As Integer choicestbottom,choicesttop
'EndIf
   ' rainbow=rgb(255*casual2,255*casual2,0)
 'choicestbottom=Int(Rnd*4)+1
 ' choicesttop=Int(Rnd*4)+1
 
' choicestbottom=1
'  choicesttop=3
 
 
     If choicesttop<2 Then
        blowtoplx=blowtoplx-(blowtoplx+tripper96b+blowtoplx*(topzzz*5))
   blowtoply=blowtoply+(blowtoply+tripper96b+blowtoply*(topzzz*5))
     End If
      If choicesttop=2 Then
        blowtoplx=blowtoplx+(blowtoplx+tripper96b+blowtoplx*(topzzz*5))
   blowtoply=blowtoply-(blowtoply+tripper96b+blowtoply*(topzzz*5))
      End If
       If choicesttop=3 Then
        blowtoplx=blowtoplx-(blowtoplx+tripper96b+blowtoplx*(topzzz*5))
   blowtoply=blowtoply-(blowtoply+tripper96b+blowtoply*(topzzz*5))
       End If
        If choicesttop>3 Then
        blowtoplx=blowtoplx+(blowtoplx+tripper96b+blowtoplx*(topzzz*5))
   blowtoply=blowtoply+(blowtoply+tripper96b+blowtoply*(topzzz*5))
        End If
End If
If judgedbottom>Timer Then 
'EndIf
tripper96a=tripper96a+8
Else
	choicedrivea=0
tripper96a=0
End If
If judgedtop>Timer Then 
'EndIf
tripper96b=tripper96b+8
Else
	choicedriveb=0
tripper96b=0
End If

  for pointidx2=1 to 2'pointidx2+
  	

 ' Next
      rem code1
'      if judgedtop>timer and judgedtop2<timer then judgedtop2=int(timer)+.33
'if judgedbottom>timer and judgedbottom2<timer then judgedbottom2=int(timer)+.33

           if (judgedtop2<timer and (pointidx2=1 and ship1rotatex<pieyedi)) or (judgedbottom2<timer and (pointidx2=2 and ship2rotatex>pieyedi)) then 

    		'if pointidx2=3 then

    FOR pointIdx = 1 TO PIXELAGENT2 step+int(rnd*32)+1
'	if pointidx>numberofpixels then pointidx=point.idx-numberofpixels
    if shiprightside(pointidx).c<>rgb(0,0,0) and shiprightside(pointidx).x+shiprightside(pointidx).y+shiprightside(pointidx).z<>0 then
   
dim as SINGLE flatz,newz,pimpz,topzzz3,bottomzzz3,shadea,shadeb,shade1,shade2
		if pointidx2=1 then
                   ' topzzz3=topzzz*5
topzzz3=topzzz
      '  if topzzz<0 then topzzz3=(topzzz-(topzzz*2))
               ' if topzzz>0 then topzzz3=(topzzz-(topzzz*2))

'ship(pointidx).z=1
'topzzz3=topzzz3*2
'if topzzz3<.25/2 then topzzz3=.25/2
'if topzzz3>.25 then topzzz3=.25
'if topzzz3<.5 then topzzz3=topzzz3*2  
'topzzz3=topzzz3/2
flatx=shiprightside(pointidx).x*((64*.55)*((topzzz3*5)))/(shiprightside(pointidx).z+zoomlevel)
'SHADEA=ship1rotatex*100/(pieyedi/2)/100
'IF SHIP1ROTATEX>=0 THEN 
    SHADE1=3+SHIPrightside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEX<0 THEN
'     SHADE1=3+SHIPrightside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'     END IF
':shade1=shade1-(shade1*2)
'shade1=.5-SHADEA*4
flaty=shiprightside(pointidx).y*((64*.55)*((topzzz3*5)))/(shiprightside(pointidx).z+zoomlevel)
'IF SHIP1ROTATEY>0 THEN  
'    SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEY<0 THEN
'     SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'     END IF
'if timer>judgedtop then toyota=0 else toyota=flat1:end if
flatz=shiprightside(pointidx).Z*((128*8)*.75)*(topzzz3*5)
If judgedtop<Timer Then
 newx = flatx'newx'
newy = (sin(ship1rotatey) * flatz) + (cos(ship1rotatey) * flaty)
newz = (cos(ship1rotatey) * flatz) - (sin(ship1rotatey) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship1rotatex) * flatx) - (sin(ship1rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship1rotatex) * flatx) + (cos(ship1rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy
End If

if judgedtop>Timer then 
 newx = flatx'newx'
newy = (sin(flat2) * flatz) + (cos(flat2) * flaty)
newz = (cos(flat2) * flatz) - (sin(flat2) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) - (sin(flat2) * flatz)
newy=flaty''y = y'
newz = (sin(flat2) * flatx) + (cos(flat2) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
end if




'/((720*2+(720/2)))'/shiprightside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).z+zoomlevel)'((3)
else
           ' bottomzzz=bottomzzz*5
bottomzzz3=bottomzzz
           ' if bottomzzz<0 then bottomzzz=1-(bottomzzz-(bottomzzz*2))
'if bottomzzz<0 then bottomzzz3=(bottomzzz-(bottomzzz*2))
'bottomzzz3=bottomzzz3*2
'if bottomzzz3<.25/2 then bottomzzz3=.25/2
'if bottomzzz3>.25 then bottomzzz3=.25'-.1
flatx=shiprightside(pointidx).x*((64*.55)*((bottomzzz3*5)))/(shiprightside(pointidx).z+zoomlevel)
'IF SHIP2ROTATEX>=0 THEN 
    SHADE2=3+SHIPrightside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEX<=0 THEN  
'    SHADE2=3+SHIPrightside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'     END IF
flaty=shiprightside(pointidx).y*((64*.55)*((bottomzzz3*5)))/(shiprightside(pointidx).z+zoomlevel)
'IF SHIP2ROTATEY>0 THEN 
'    SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEY<0 THEN
'     SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shiprightside(pointidx).Z*((128*8)*.75)*(bottomzzz3*5)'/((720*2+(720/2)))'/shiprightside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).Z+zoomlevel)'((3)
If judgedbottom<Timer Then
 newx = flatx'newx'
newy = (sin(ship2rotatey+pieyedi) * flatz) + (cos(ship2rotatey+pieyedi) * flaty)
newz = (cos(ship2rotatey+pieyedi) * flatz) - (sin(ship2rotatey+pieyedi) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship2rotatex) * flatx) - (sin(ship2rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship2rotatex) * flatx) + (cos(ship2rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy
End If
if judgedbottom>Timer then 
 newx = flatx'newx'
newy = (sin(flat1) * flatz) + (cos(flat1) * flaty)
newz = (cos(flat1) * flatz) - (sin(flat1) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
newy=flaty''y = y'
newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) + (sin(flat2) * flaty)
newy = (cos(flat2) * flaty) - (sin(flat2) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
 newy2=newy
 newx2=newx
end if











end if
'flatx=newx
' flaty=newy
' flatz=newz
 'newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz




'newz = (cos(ship1rotatex) * flatz) - (sin(ship1rotatex) * flaty)
 'flatx=newx
 'flaty=newy
 'flatz=newz
 'newy=flaty
 rem xyrotatecode
' alphyx=ship(pointidx).x*cos(ship1rotatex-pieyedi/4)
'  alphyy=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' if singletopy<1 and singletopy>0 then singletopy=0
 ' if singlebottomy<1 and singlebottomy>0 then singlebottomy=0

' x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')

'The second set of algorithms is for rotation about the y axis:

'x = (cos(angle) * x') - (sin(angle) * z')
'y = y'
'z = (sin(angle) * x') + (cos(angle) * z')

 'newx=flatx
 'newy2=newy
 'newx2=newx
' newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz
 
' newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
'newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
'newz = flatz'
' flatx=newx
' flaty=newy
' flatz=newz
 
' alphyx=ship(pointidx).x*cos(ship1rotatex)
'  alphyx=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' dim as SINGLE aly1,aly2
 ' aly1=ship(pointidx).x*cos(ship2rotatex)
   ' aly1=ship(pointidx).y*cos(ship2rotatey-pieyedi/4)
'casual=alphyx'y*(100/1/30)
'casual2=aly1'2*(100/1/30)
'casual=casual*4
'casual2=casual2*4
'if casual>1 then casual=1
'if casual<0 then casual=0
'if casual2>1 then casual2=1
'if casual2<0 then casual2=0

casual=1*(topzzz*5)'casual+5
casual2=1*(bottomzzz*5)'=casual2+5
if casual<1 then casual=1
if casual2<1 then casual2=1
casual=casual*shade1'*SHADEA
casual2=casual2*shade2'*SHADEB

'casual=1'-((1)*flatz*100/((1280)/2)/100)
'dim x as SINGLE
 
 'rernd2:
' x=rnd'(timered)
 'if x=0 then goto rernd2
'if x<.6 then casual=casual-.08'03333333
' if x>.5 then casual=casual+.08'03333333
'if (pieyedi*2/20)+slphouse'1*(pieyedi*2/20)-(slphouse-pieyedi/2)*100/(pieyedi*2/20-pieyedi/2)/100'(100/2/100)
          if casual>1 then casual=1
          if casual<0 then casual=0
if casual2>1 then casual2=1
if casual2<0 then casual2=0

    rainbow=shiprightside(pointidx).c'rgb(255,255,251)
   
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )

rainbow=rgb(d*casual,n*casual,e*casual)',127)
If pointidx2=2 and judgedbottom>timer then
    rainbow=rgb(255*casual2,255*casual2,0)
  ' blowbottomlx=blowbottomlx-1*(bottomzzz*5)
  ' blowbottomly=blowbottomly-1*(bottomzzz*5)
end if
if pointidx2=1 and judgedtop>timer then
        rainbow=rgb(255*casual,255*casual,0)
  '      blowtoplx=blowtoplx-1*(topzzz*5)' Else blowtoplx=blowtoplx+1*(topzzz*5):End If
  '  blowtoply=blowtoply+1*(topzzz*5)' Else blowtoply=blowtoply+.005*(topzzz*5):End If
End If

if pointidx2=1 then
 screenlock
   circle (newX+blowtoprx+(topxxx), newy+blowtopry+(topyyy)-(33+9)),1.5*(topzzz*5),rainbow,,,,F
   screenunlock
   elseif pointidx2=2 then
   if judgedbottom<timer then rainbow=rgb(d*casual2,n*casual2,e*casual2)',127)
screenlock
circle(newx2+blowbottomrx+(bottomxxx),newy2+blowbottomry+(bottomyyy)+(33+9)),1.5*(bottomzzz*5),rainbow,,,,F
screenunlock
end if
end if
NEXT
'next
'end if
drawship
'     if (ship1rotatex>0) or (ship2rotatex>0) or (pointidx2=3 and ship1rotatex=0) or (pointidx2=2 and ship2rotatex=0) then 
        ' asm
          '   call drawship
           '  end asm
 '   end if
'     if (ship1rotatex>0) or (ship2rotatex>0) or (pointidx2=3 and ship1rotatex=0) or (pointidx2=2 and ship2rotatex=0) then 

'end if
 ' for pointidx2=3 to 2'pointidx2+
    		'if pointidx2=2 then

   FOR pointIdx = 1 TO PIXELAGENT step+int(rnd*32)+1'.051'pixelagent1
'	if pointidx>numberofpixels then pointidx=point.idx-numberofpixels
    if shipleftside(pointidx).c<>rgb(0,0,0) and shipleftside(pointidx).x+shipleftside(pointidx).y+shipleftside(pointidx).Z<>0 then
   
dim as SINGLE flatz,newz,pimpz,topzzz3,bottomzzz3,shadea,shadeb,shade1,shade2
		if pointidx2=1 then
                   ' topzzz3=topzzz*5
topzzz3=topzzz
      '  if topzzz<0 then topzzz3=(topzzz-(topzzz*2))
               ' if topzzz>0 then topzzz3=(topzzz-(topzzz*2))

'ship(pointidx).Z=1
'topzzz3=topzzz3*2
'if topzzz3<.25/2 then topzzz3=.25/2
'if topzzz3>.25 then topzzz3=.25
'if topzzz3<.5 then topzzz3=topzzz3*2  
'topzzz3=topzzz3/2
flatx=shipleftside(pointidx).x*((64*.55)*((topzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'SHADEA=ship1rotatex*100/(pieyedi/2)/100
'IF SHIP1ROTATEX>=0 THEN 
    SHADE1=3+SHIPleftside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEX<0 THEN
'     SHADE1=3+SHIPleftside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'     END IF
   
':shade1=shade1-(shade1*2)
'shade1=.5-SHADEA*4
flaty=shipleftside(pointidx).y*((64*.55)*((topzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP1ROTATEY>0 THEN 
'    SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEY<0 THEN
'     SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shipleftside(pointidx).Z*((128*8)*.75)*(topzzz3*5)'/((720*2+(720/2)))'/shipleftside(pointidx).Z)/((720*2+(720/2))'/shipleftside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).Z+zoomlevel)'((3)
If judgedtop<Timer Then
 newx = flatx'newx'
newy = (sin(ship1rotatey) * flatz) + (cos(ship1rotatey) * flaty)
newz = (cos(ship1rotatey) * flatz) - (sin(ship1rotatey) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship1rotatex) * flatx) - (sin(ship1rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship1rotatex) * flatx) + (cos(ship1rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy
End If

if judgedtop>Timer then 
 newx = flatx'newx'
newy = (sin(flat2) * flatz) + (cos(flat2) * flaty)
newz = (cos(flat2) * flatz) - (sin(flat2) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) - (sin(flat2) * flatz)
newy=flaty''y = y'
newz = (sin(flat2) * flatx) + (cos(flat2) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
end If
















else
           ' bottomzzz=bottomzzz*5
bottomzzz3=bottomzzz
           ' if bottomzzz<0 then bottomzzz=1-(bottomzzz-(bottomzzz*2))
'if bottomzzz<0 then bottomzzz3=(bottomzzz-(bottomzzz*2))
'bottomzzz3=bottomzzz3*2
'if bottomzzz3<.25/2 then bottomzzz3=.25/2
'if bottomzzz3>.25 then bottomzzz3=.25'-.1
flatx=shipleftside(pointidx).x*((64*.55)*((bottomzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP2ROTATEX>=0 THEN 
    SHADE2=3+SHIPleftside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEX<=0 THEN  
'    SHADE2=3+SHIPleftside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'     END IF
flaty=shipleftside(pointidx).y*((64*.55)*((bottomzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP2ROTATEY>0 THEN 
'    SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEY<0 THEN
'     SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shipleftside(pointidx).Z*((128*8)*.75)*(bottomzzz3*5)'/((720*2+(720/2)))'/shipleftside(pointidx).Z)/((720*2+(720/2))'/shipleftside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).Z+zoomlevel)'((3)
If judgedbottom<Timer Then
 newx = flatx'newx'
newy = (sin(ship2rotatey+pieyedi) * flatz) + (cos(ship2rotatey+pieyedi) * flaty)
newz = (cos(ship2rotatey+pieyedi) * flatz) - (sin(ship2rotatey+pieyedi) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship2rotatex) * flatx) - (sin(ship2rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship2rotatex) * flatx) + (cos(ship2rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy
End If

if judgedbottom>Timer then 
 newx = flatx'newx'
newy = (sin(flat1) * flatz) + (cos(flat1) * flaty)
newz = (cos(flat1) * flatz) - (sin(flat1) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
newy=flaty''y = y'
newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) + (sin(flat2) * flaty)
newy = (cos(flat2) * flaty) - (sin(flat2) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
 newy2=newy
 newx2=newx
end If






end if
'flatx=newx
' flaty=newy
' flatz=newz
 'newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz




'newz = (cos(ship1rotatex) * flatz) - (sin(ship1rotatex) * flaty)
 'flatx=newx
 'flaty=newy
 'flatz=newz
 'newy=flaty
 rem xyrotatecode
' alphyx=ship(pointidx).x*cos(ship1rotatex-pieyedi/4)
'  alphyy=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' if singletopy<1 and singletopy>0 then singletopy=0
 ' if singlebottomy<1 and singlebottomy>0 then singlebottomy=0

' x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')

'The second set of algorithms is for rotation about the y axis:

'x = (cos(angle) * x') - (sin(angle) * z')
'y = y'
'z = (sin(angle) * x') + (cos(angle) * z')

 'newx=flatx
 'newy2=newy
 'newx2=newx
' newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz
 
' newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
'newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
'newz = flatz'
' flatx=newx
' flaty=newy
' flatz=newz
 
' alphyx=ship(pointidx).x*cos(ship1rotatex)
'  alphyx=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' dim as SINGLE aly1,aly2
 ' aly1=ship(pointidx).x*cos(ship2rotatex)
   ' aly1=ship(pointidx).y*cos(ship2rotatey-pieyedi/4)
'casual=alphyx'y*(100/1/30)
'casual2=aly1'2*(100/1/30)
'casual=casual*4
'casual2=casual2*4
'if casual>1 then casual=1
'if casual<0 then casual=0
'if casual2>1 then casual2=1
'if casual2<0 then casual2=0

casual=1*(topzzz*5)'casual+5
casual2=1*(bottomzzz*5)'=casual2+5
if casual<1 then casual=1
if casual2<1 then casual2=1
casual=casual*shade1'*SHADEA
casual2=casual2*shade2'*SHADEB

'casual=1'-((1)*flatz*100/((1280)/2)/100)
'dim x as SINGLE
 
 'rernd2:
' x=rnd'(timered)
 'if x=0 then goto rernd2
'if x<.6 then casual=casual-.08'03333333
' if x>.5 then casual=casual+.08'03333333
'if (pieyedi*2/20)+slphouse'1*(pieyedi*2/20)-(slphouse-pieyedi/2)*100/(pieyedi*2/20-pieyedi/2)/100'(100/2/100)
          if casual>1 then casual=1
          if casual<0 then casual=0
if casual2>1 then casual2=1
if casual2<0 then casual2=0
    rainbow=shipleftside(pointidx).c'rgb(255,255,251)
   
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )

rainbow=rgb(d*casual,n*casual,e*casual)',127)
If pointidx2=2 and judgedbottom>timer then
    rainbow=rgb(255*casual2,255*casual2,0)
  ' blowbottomlx=blowbottomlx-1*(bottomzzz*5)
  ' blowbottomly=blowbottomly-1*(bottomzzz*5)
end if
if pointidx2=1 and judgedtop>timer then
        rainbow=rgb(255*casual,255*casual,0)
  '      blowtoplx=blowtoplx-1*(topzzz*5)' Else blowtoplx=blowtoplx+1*(topzzz*5):End If
  '  blowtoply=blowtoply+1*(topzzz*5)' Else blowtoply=blowtoply+.005*(topzzz*5):End If
End If

if pointidx2=1 then
 ScreenLock
   circle (newX+blowtoplx+(topxxx), newy+blowtoply+(topyyy)-(33+9)),1.5*(topzzz*5),rainbow,,,,F
   screenunlock
   elseif pointidx2=2 then
   if judgedbottom<timer then rainbow=rgb(d*casual2,n*casual2,e*casual2)',127)
screenlock
circle(newx2+blowbottomrx+(bottomxxx),newy2+blowbottomry+(bottomyyy)+(33+9)),1.5*(bottomzzz*5),rainbow,,,,F
screenunlock
end if
end If
NEXT

'next
end if
      
      
    
rem code2 and code3
     if (judgedtop2<timer and (pointidx2=1 and ship1rotatex=0)) or (judgedbottom2<timer and (pointidx2=2 and ship2rotatex=0)) then 

    		'if pointidx2=2 then

   FOR pointIdx = 1 TO PIXELAGENT Step+int(rnd*32)+1
'	if pointidx>numberofpixels then pointidx=point.idx-numberofpixels
    if shiprightside(pointidx).c<>rgb(0,0,0) and shiprightside(pointidx).x+shiprightside(pointidx).y+shiprightside(pointidx).Z<>0 then
   
dim as SINGLE flatz,newz,pimpz,topzzz3,bottomzzz3,shadea,shadeb,shade1,shade2
		If pointidx2=1 then
                   ' topzzz3=topzzz*5
' topzzz3=topzzz*5
topzzz3=topzzz
      '  if topzzz<0 then topzzz3=(topzzz-(topzzz*2))
               ' if topzzz>0 then topzzz3=(topzzz-(topzzz*2))

'ship(pointidx).z=1
'topzzz3=topzzz3*2
'if topzzz3<.25/2 then topzzz3=.25/2
'if topzzz3>.25 then topzzz3=.25
'if topzzz3<.5 then topzzz3=topzzz3*2  
'topzzz3=topzzz3/2
flatx=shiprightside(pointidx).x*((64*.55)*((topzzz3*5)))/(shiprightside(pointidx).z+zoomlevel)
'SHADEA=ship1rotatex*100/(pieyedi/2)/100
'IF SHIP1ROTATEX>=0 THEN 
    SHADE1=3+SHIPrightside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEX<0 THEN
'     SHADE1=3+SHIPrightside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'     END IF
':shade1=shade1-(shade1*2)
'shade1=.5-SHADEA*4
flaty=shiprightside(pointidx).y*((64*.55)*((topzzz3*5)))/(shiprightside(pointidx).z+zoomlevel)
'IF SHIP1ROTATEY>0 THEN  
'    SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEY<0 THEN
'     SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'     END IF
'if timer>judgedtop then toyota=0 else toyota=flat1:end if
flatz=shiprightside(pointidx).Z*((128*8)*.75)*(topzzz3*5)
If judgedtop<Timer Then
 newx = flatx'newx'
newy = (sin(ship1rotatey) * flatz) + (cos(ship1rotatey) * flaty)
newz = (cos(ship1rotatey) * flatz) - (sin(ship1rotatey) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship1rotatex) * flatx) - (sin(ship1rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship1rotatex) * flatx) + (cos(ship1rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy
End If
if judgedtop>Timer then 
 newx = flatx'newx'
newy = (sin(flat2) * flatz) + (cos(flat2) * flaty)
newz = (cos(flat2) * flatz) - (sin(flat2) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) - (sin(flat2) * flatz)
newy=flaty''y = y'
newz = (sin(flat2) * flatx) + (cos(flat2) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
 
End if




'/((720*2+(720/2)))'/shiprightside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).z+zoomlevel)'((3)
else
           ' bottomzzz=bottomzzz*5
bottomzzz3=bottomzzz
           ' if bottomzzz<0 then bottomzzz=1-(bottomzzz-(bottomzzz*2))
'if bottomzzz<0 then bottomzzz3=(bottomzzz-(bottomzzz*2))
'bottomzzz3=bottomzzz3*2
'if bottomzzz3<.25/2 then bottomzzz3=.25/2
'if bottomzzz3>.25 then bottomzzz3=.25'-.1
flatx=shiprightside(pointidx).x*((64*.55)*((bottomzzz3*5)))/(shiprightside(pointidx).z+zoomlevel)
'IF SHIP2ROTATEX>=0 THEN 
    SHADE2=3+SHIPrightside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEX<=0 THEN  
'    SHADE2=3+SHIPrightside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'     END IF
flaty=shiprightside(pointidx).y*((64*.55)*((bottomzzz3*5)))/(shiprightside(pointidx).z+zoomlevel)
'IF SHIP2ROTATEY>0 THEN 
'    SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEY<0 THEN
'     SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shiprightside(pointidx).Z*((128*8)*.75)*(bottomzzz3*5)'/((720*2+(720/2)))'/shiprightside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).Z+zoomlevel)'((3)
If judgedbottom<Timer Then
 newx = flatx'newx'
newy = (sin(ship2rotatey+pieyedi) * flatz) + (cos(ship2rotatey+pieyedi) * flaty)
newz = (cos(ship2rotatey+pieyedi) * flatz) - (sin(ship2rotatey+pieyedi) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship2rotatex) * flatx) - (sin(ship2rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship2rotatex) * flatx) + (cos(ship2rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy
End If
if judgedbottom>Timer then 
 newx = flatx'newx'
newy = (sin(flat1) * flatz) + (cos(flat1) * flaty)
newz = (cos(flat1) * flatz) - (sin(flat1) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
newy=flaty''y = y'
newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) + (sin(flat2) * flaty)
newy = (cos(flat2) * flaty) - (sin(flat2) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
 newy2=newy
 newx2=newx
end if











end If
'flatx=newx
' flaty=newy
' flatz=newz
 'newx = (cos(flat3) * flatx) - (sin(flat3) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat3) * flatx) + (cos(flat3) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz





'newz = (cos(ship1rotatex) * flatz) - (sin(ship1rotatex) * flaty)
 'flatx=newx
 'flaty=newy
 'flatz=newz
 'newy=flaty
 rem xyrotatecode
' alphyx=ship(pointidx).x*cos(ship1rotatex-pieyedi/4)
'  alphyy=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' if singletopy<1 and singletopy>0 then singletopy=0
 ' if singlebottomy<1 and singlebottomy>0 then singlebottomy=0

' x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')

'The second set of algorithms is for rotation about the y axis:

'x = (cos(angle) * x') - (sin(angle) * z')
'y = y'
'z = (sin(angle) * x') + (cos(angle) * z')

 'newx=flatx
 'newy2=newy
 'newx2=newx
' newx = (cos(flat3) * flatx) - (sin(flat3) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat3) * flatx) + (cos(flat3) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz
 
' newx = (cos(flat3) * flatx) + (sin(flat3) * flaty)
'newy = (cos(flat3) * flaty) - (sin(flat3) * flatx)
'newz = flatz'
' flatx=newx
' flaty=newy
' flatz=newz
 
' alphyx=ship(pointidx).x*cos(ship1rotatex)
'  alphyx=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' dim as SINGLE aly1,aly2
 ' aly1=ship(pointidx).x*cos(ship2rotatex)
   ' aly1=ship(pointidx).y*cos(ship2rotatey-pieyedi/4)
'casual=alphyx'y*(100/1/30)
'casual2=aly1'2*(100/1/30)
'casual=casual*4
'casual2=casual2*4
'if casual>1 then casual=1
'if casual<0 then casual=0
'if casual2>1 then casual2=1
'if casual2<0 then casual2=0

casual=1*(topzzz*5)'casual+5
casual2=1*(bottomzzz*5)'=casual2+5
if casual<1 then casual=1
if casual2<1 then casual2=1
casual=casual*shade1'*SHADEA
casual2=casual2*shade2'*SHADEB

'casual=1'-((1)*flatz*100/((1280)/2)/100)
'dim x as SINGLE
 
 'rernd2:
' x=rnd'(timered)
 'if x=0 then goto rernd2
'if x<.6 then casual=casual-.08'03333333
' if x>.5 then casual=casual+.08'03333333
'if (pieyedi*2/20)+slphouse'1*(pieyedi*2/20)-(slphouse-pieyedi/2)*100/(pieyedi*2/20-pieyedi/2)/100'(100/2/100)
          if casual>1 then casual=1
          if casual<0 then casual=0
if casual2>1 then casual2=1
if casual2<0 then casual2=0
    rainbow=shiprightside(pointidx).c'rgb(255,255,251)
   
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )

rainbow=rgb(d*casual,n*casual,e*casual)',127)
If pointidx2=2 and judgedbottom>timer then
    rainbow=rgb(255*casual2,255*casual2,0)
  ' blowbottomlx=blowbottomlx-1*(bottomzzz*5)
  ' blowbottomly=blowbottomly-1*(bottomzzz*5)
end if
if pointidx2=1 and judgedtop>timer then
        rainbow=rgb(255*casual,255*casual,0)
  '      blowtoplx=blowtoplx-1*(topzzz*5)' Else blowtoplx=blowtoplx+1*(topzzz*5):End If
  '  blowtoply=blowtoply+1*(topzzz*5)' Else blowtoply=blowtoply+.005*(topzzz*5):End If
End If

if pointidx2=1 then
 ScreenLock
   circle (newX+blowtoprx+(topxxx), newy+blowtopry+(topyyy)-(33+9)),1.5*(topzzz*5),rainbow,,,,F
   screenunlock
   elseif pointidx2=2 then
   if judgedbottom<timer then rainbow=rgb(d*casual2,n*casual2,e*casual2)',127)
screenlock
circle(newx2+blowbottomrx+(bottomxxx),newy2+blowbottomry+(bottomyyy)+(33+9)),1.5*(bottomzzz*5),rainbow,,,,F
screenunlock
end if
end If
NEXT
'next
'end if

'     if (ship1rotatex>0) or (ship2rotatex>0) or (pointidx2=1 and ship1rotatex=0) or (pointidx2=2 and ship2rotatex=0) then 
       '  asm
             drawship
        '     end asm
 '   end if
'     if (ship1rotatex>0) or (ship2rotatex>0) or (pointidx2=1 and ship1rotatex=0) or (pointidx2=2 and ship2rotatex=0) then 

'end if
 ' for pointidx2=3 to 2'pointidx2+
    		'if pointidx2=1 then

   FOR pointIdx = 1 TO PIXELAGENT2 step+ int(rnd*32)+1' pointa TO pointb 'step +int(rnd*16)+.05'pixelagent1
'	if pointidx>numberofpixels then pointidx=point.idx-numberofpixels
    if shipleftside(pointidx).c<>rgb(0,0,0) and shipleftside(pointidx).x+shipleftside(pointidx).y+shipleftside(pointidx).Z<>0 then
   
dim as SINGLE flatz,newz,pimpz,topzzz3,bottomzzz3,shadea,shadeb,shade1,shade2
		if pointidx2=1 Then
  topzzz3=topzzz
      '  if topzzz<0 then topzzz3=(topzzz-(topzzz*2))
               ' if topzzz>0 then topzzz3=(topzzz-(topzzz*2))

'ship(pointidx).Z=1
'topzzz3=topzzz3*2
'if topzzz3<.25/2 then topzzz3=.25/2
'if topzzz3>.25 then topzzz3=.25
'if topzzz3<.5 then topzzz3=topzzz3*2  
'topzzz3=topzzz3/2
flatx=shipleftside(pointidx).x*((64*.55)*((topzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'SHADEA=ship1rotatex*100/(pieyedi/2)/100
'IF SHIP1ROTATEX>=0 THEN 
    SHADE1=3+SHIPleftside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEX<0 THEN
'     SHADE1=3+SHIPleftside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'     END IF
   
':shade1=shade1-(shade1*2)
'shade1=.5-SHADEA*4
flaty=shipleftside(pointidx).y*((64*.55)*((topzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP1ROTATEY>0 THEN 
'    SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEY<0 THEN
'     SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shipleftside(pointidx).Z*((128*8)*.75)*(topzzz3*5)'/((720*2+(720/2)))'/shipleftside(pointidx).Z)/((720*2+(720/2))'/shipleftside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).Z+zoomlevel)'((3)
If judgedtop<Timer Then
 newx = flatx'newx'
newy = (sin(ship1rotatey) * flatz) + (cos(ship1rotatey) * flaty)
newz = (cos(ship1rotatey) * flatz) - (sin(ship1rotatey) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship1rotatex) * flatx) - (sin(ship1rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship1rotatex) * flatx) + (cos(ship1rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy
End If

if judgedtop>Timer then 
 newx = flatx'newx'
newy = (sin(flat2) * flatz) + (cos(flat2) * flaty)
newz = (cos(flat2) * flatz) - (sin(flat2) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) - (sin(flat2) * flatz)
newy=flaty''y = y'
newz = (sin(flat2) * flatx) + (cos(flat2) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
end If
















else
           ' bottomzzz=bottomzzz*5
bottomzzz3=bottomzzz
           ' if bottomzzz<0 then bottomzzz=1-(bottomzzz-(bottomzzz*2))
'if bottomzzz<0 then bottomzzz3=(bottomzzz-(bottomzzz*2))
'bottomzzz3=bottomzzz3*2
'if bottomzzz3<.25/2 then bottomzzz3=.25/2
'if bottomzzz3>.25 then bottomzzz3=.25'-.1
flatx=shipleftside(pointidx).x*((64*.55)*((bottomzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP2ROTATEX>=0 THEN 
    SHADE2=3+SHIPleftside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEX<=0 THEN  
'    SHADE2=3+SHIPleftside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'     END IF
flaty=shipleftside(pointidx).y*((64*.55)*((bottomzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP2ROTATEY>0 THEN 
'    SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEY<0 THEN
'     SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shipleftside(pointidx).Z*((128*8)*.75)*(bottomzzz3*5)'/((720*2+(720/2)))'/shipleftside(pointidx).Z)/((720*2+(720/2))'/shipleftside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).Z+zoomlevel)'((3)
If judgedbottom<Timer Then
 newx = flatx'newx'
newy = (sin(ship2rotatey+pieyedi) * flatz) + (cos(ship2rotatey+pieyedi) * flaty)
newz = (cos(ship2rotatey+pieyedi) * flatz) - (sin(ship2rotatey+pieyedi) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship2rotatex) * flatx) - (sin(ship2rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship2rotatex) * flatx) + (cos(ship2rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy
End If

if judgedbottom>Timer then 
 newx = flatx'newx'
newy = (sin(flat1) * flatz) + (cos(flat1) * flaty)
newz = (cos(flat1) * flatz) - (sin(flat1) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
newy=flaty''y = y'
newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) + (sin(flat2) * flaty)
newy = (cos(flat2) * flaty) - (sin(flat2) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
 newy2=newy
 newx2=newx
end If






end if

'flatx=newx
' flaty=newy
' flatz=newz
 'newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat3) * flatx) + (cos(flat3) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz





'newz = (cos(ship1rotatex) * flatz) - (sin(ship1rotatex) * flaty)
 'flatx=newx
 'flaty=newy
 'flatz=newz
 'newy=flaty
 rem xyrotatecode
' alphyx=ship(pointidx).x*cos(ship1rotatex-pieyedi/4)
'  alphyy=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' if singletopy<1 and singletopy>0 then singletopy=0
 ' if singlebottomy<1 and singlebottomy>0 then singlebottomy=0

' x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')

'The second set of algorithms is for rotation about the y axis:

'x = (cos(angle) * x') - (sin(angle) * z')
'y = y'
'z = (sin(angle) * x') + (cos(angle) * z')

 'newx=flatx
 'newy2=newy
 'newx2=newx
' newx = (cos(flat3) * flatx) - (sin(flat3) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat3) * flatx) + (cos(flat3) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz
 
' newx = (cos(flat3) * flatx) + (sin(flat3) * flaty)
'newy = (cos(flat3) * flaty) - (sin(flat3) * flatx)
'newz = flatz'
' flatx=newx
' flaty=newy
' flatz=newz
 
' alphyx=ship(pointidx).x*cos(ship1rotatex)
'  alphyx=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' dim as SINGLE aly1,aly2
 ' aly1=ship(pointidx).x*cos(ship2rotatex)
   ' aly1=ship(pointidx).y*cos(ship2rotatey-pieyedi/4)
'casual=alphyx'y*(100/1/30)
'casual2=aly1'2*(100/1/30)
'casual=casual*4
'casual2=casual2*4
'if casual>1 then casual=1
'if casual<0 then casual=0
'if casual2>1 then casual2=1
'if casual2<0 then casual2=0

casual=1*(topzzz*5)'casual+5
casual2=1*(bottomzzz*5)'=casual2+5
if casual<1 then casual=1
if casual2<1 then casual2=1
casual=casual*shade1'*SHADEA
casual2=casual2*shade2'*SHADEB

'casual=1'-((1)*flatz*100/((1280)/2)/100)
'dim x as SINGLE
 
 'rernd2:
' x=rnd'(timered)
 'if x=0 then goto rernd2
'if x<.6 then casual=casual-.08'03333333
' if x>.5 then casual=casual+.08'03333333
'if (pieyedi*2/20)+slphouse'1*(pieyedi*2/20)-(slphouse-pieyedi/2)*100/(pieyedi*2/20-pieyedi/2)/100'(100/2/100)
          if casual>1 then casual=1
          if casual<0 then casual=0
if casual2>1 then casual2=1
if casual2<0 then casual2=0
    rainbow=shipleftside(pointidx).c'rgb(255,255,251)
   
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )

rainbow=rgb(d*casual,n*casual,e*casual)',127)
If pointidx2=2 and judgedbottom>timer then
    rainbow=rgb(255*casual2,255*casual2,0)
  ' blowbottomlx=blowbottomlx-1*(bottomzzz*5)
  ' blowbottomly=blowbottomly-1*(bottomzzz*5)
end if
if pointidx2=1 and judgedtop>timer then
        rainbow=rgb(255*casual,255*casual,0)
  '      blowtoplx=blowtoplx-1*(topzzz*5)' Else blowtoplx=blowtoplx+1*(topzzz*5):End If
  '  blowtoply=blowtoply+1*(topzzz*5)' Else blowtoply=blowtoply+.005*(topzzz*5):End If
End If

if pointidx2=1 then
 ScreenLock
   circle (newX+blowtoplx+(topxxx), newy+blowtoply+(topyyy)-(33+9)),1.5*(topzzz*5),rainbow,,,,F
   screenunlock
   elseif pointidx2=2 then
   if judgedbottom<timer then rainbow=rgb(d*casual2,n*casual2,e*casual2)',127)
screenlock
circle(newx2+blowbottomlx+(bottomxxx),newy2+blowbottomly+(bottomyyy)+(33+9)),1.5*(bottomzzz*5),rainbow,,,,F
screenunlock
end if
end If
NEXT

'next
end if





     'if (ship1rotatex<0) or (ship2rotatex>0) or ship1rotatex=0 or ship2rotatex=0 then 
    ' if (pointidx2=1 and ship1rotatex<0) or (pointidx2=2 and ship2rotatex>0) then 
     'if (ship1rotatex<0) or (ship2rotatex<0) or (pointidx2=1 and ship1rotatex=0 and ship1rotatey=0) or (pointidx2=2 and ship2rotatex=0 and ship2rotatey=0) then 
     if (judgedtop2<timer and (pointidx2=1 and ship1rotatex>pieyedi)) or (judgedbottom2<timer and (pointidx2=2 and ship2rotatex<pieyedi)) then 

 ' for pointidx2=3 to 2'pointidx2+
    		'if pointidx2=1 then

   FOR pointIdx = 1 To PIXELAGENT2 step+ int(rnd*32)+11' pointa TO pointb'pixelagent2'step+int(rnd*16)+.05
'	if pointidx>numberofpixels then pointidx=point.idx-numberofpixels
    if shipleftside(pointidx).c<>rgb(0,0,0) and shipleftside(pointidx).x+shipleftside(pointidx).y+shipleftside(pointidx).Z<>0 then
   
dim as SINGLE flatz,newz,pimpz,topzzz3,bottomzzz3,shadea,shadeb,shade1,shade2
		if pointidx2=1 then
      topzzz3=topzzz
      '  if topzzz<0 then topzzz3=(topzzz-(topzzz*2))
               ' if topzzz>0 then topzzz3=(topzzz-(topzzz*2))

'ship(pointidx).Z=1
'topzzz3=topzzz3*2
'if topzzz3<.25/2 then topzzz3=.25/2
'if topzzz3>.25 then topzzz3=.25
'if topzzz3<.5 then topzzz3=topzzz3*2  
'topzzz3=topzzz3/2
flatx=shipleftside(pointidx).x*((64*.55)*((topzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'SHADEA=ship1rotatex*100/(pieyedi/2)/100
'IF SHIP1ROTATEX>=0 THEN 
    SHADE1=3+SHIPleftside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEX<0 THEN
'     SHADE1=3+SHIPleftside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'     END IF
   
':shade1=shade1-(shade1*2)
'shade1=.5-SHADEA*4
flaty=shipleftside(pointidx).y*((64*.55)*((topzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP1ROTATEY>0 THEN 
'    SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEY<0 THEN
'     SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shipleftside(pointidx).Z*((128*8)*.75)*(topzzz3*5)'/((720*2+(720/2)))'/shipleftside(pointidx).Z)/((720*2+(720/2))'/shipleftside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).Z+zoomlevel)'((3)
If judgedtop<Timer Then
 newx = flatx'newx'
newy = (sin(ship1rotatey) * flatz) + (cos(ship1rotatey) * flaty)
newz = (cos(ship1rotatey) * flatz) - (sin(ship1rotatey) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship1rotatex) * flatx) - (sin(ship1rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship1rotatex) * flatx) + (cos(ship1rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy
End If


if judgedtop>Timer then 
 newx = flatx'newx'
newy = (sin(flat2) * flatz) + (cos(flat2) * flaty)
newz = (cos(flat2) * flatz) - (sin(flat2) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) - (sin(flat2) * flatz)
newy=flaty''y = y'
newz = (sin(flat2) * flatx) + (cos(flat2) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
end If
















else
           ' bottomzzz=bottomzzz*5
bottomzzz3=bottomzzz
           ' if bottomzzz<0 then bottomzzz=1-(bottomzzz-(bottomzzz*2))
'if bottomzzz<0 then bottomzzz3=(bottomzzz-(bottomzzz*2))
'bottomzzz3=bottomzzz3*2
'if bottomzzz3<.25/2 then bottomzzz3=.25/2
'if bottomzzz3>.25 then bottomzzz3=.25'-.1
flatx=shipleftside(pointidx).x*((64*.55)*((bottomzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP2ROTATEX>=0 THEN 
    SHADE2=3+SHIPleftside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEX<=0 THEN  
'    SHADE2=3+SHIPleftside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'     END IF
flaty=shipleftside(pointidx).y*((64*.55)*((bottomzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP2ROTATEY>0 THEN 
'    SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEY<0 THEN
'     SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shipleftside(pointidx).Z*((128*8)*.75)*(bottomzzz3*5)'/((720*2+(720/2)))'/shipleftside(pointidx).Z)/((720*2+(720/2))'/shipleftside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).Z+zoomlevel)'((3)
If judgedbottom<Timer Then
 newx = flatx'newx'
newy = (sin(ship2rotatey+pieyedi) * flatz) + (cos(ship2rotatey+pieyedi) * flaty)
newz = (cos(ship2rotatey+pieyedi) * flatz) - (sin(ship2rotatey+pieyedi) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship2rotatex) * flatx) - (sin(ship2rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship2rotatex) * flatx) + (cos(ship2rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy
End If

if judgedbottom>Timer then 
 newx = flatx'newx'
newy = (sin(flat1) * flatz) + (cos(flat1) * flaty)
newz = (cos(flat1) * flatz) - (sin(flat1) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
newy=flaty''y = y'
newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) + (sin(flat2) * flaty)
newy = (cos(flat2) * flaty) - (sin(flat2) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
 newy2=newy
 newx2=newx
end If






end if

'flatx=newx
' flaty=newy
' flatz=newz
 'newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz







'end if
'newz = (cos(ship1rotatex) * flatz) - (sin(ship1rotatex) * flaty)
 'flatx=newx
 'flaty=newy
 'flatz=newz
 'newy=flaty
' rem xyrotatecode
' alphyx=ship(pointidx).x*cos(ship1rotatex-pieyedi/4)
'  alphyy=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
  'if singletopy<1 and singletopy>0 then singletopy=0
  'if singlebottomy<1 and singlebottomy>0 then singlebottomy=0

' x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')

'The second set of algorithms is for rotation about the y axis:

'x = (cos(angle) * x') - (sin(angle) * z')
'y = y'
'z = (sin(angle) * x') + (cos(angle) * z')

 'newx=flatx
 'newy2=newy
 'newx2=newx
' newx = (cos(flat3) * flatx) - (sin(flat3) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat3) * flatx) + (cos(flat3) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz
 
' newx = (cos(flat3) * flatx) + (sin(flat3) * flaty)
'newy = (cos(flat3) * flaty) - (sin(flat3) * flatx)
'newz = flatz'
' flatx=newx
' flaty=newy
' flatz=newz
 
' alphyx=ship(pointidx).x*cos(ship1rotatex)
'  alphyx=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' dim as SINGLE aly1,aly2
 ' aly1=ship(pointidx).x*cos(ship2rotatex)
   ' aly1=ship(pointidx).y*cos(ship2rotatey-pieyedi/4)
'casual=alphyx'y*(100/1/30)
'casual2=aly1'2*(100/1/30)
'casual=casual*4
'casual2=casual2*4
'if casual>1 then casual=1
'if casual<0 then casual=0
'if casual2>1 then casual2=1
'if casual2<0 then casual2=0

casual=1*(topzzz*5)'casual+5
casual2=1*(bottomzzz*5)'=casual2+5
if casual<1 then casual=1
if casual2<1 then casual2=1
casual=casual*shade1'*SHADEA
casual2=casual2*shade2'*SHADEB

'casual=1'-((1)*flatz*100/((1280)/2)/100)
'dim x as SINGLE
 
 'rernd2:
' x=rnd'(timered)
 'if x=0 then goto rernd2
'if x<.6 then casual=casual-.08'03333333
' if x>.5 then casual=casual+.08'03333333
'if (pieyedi*2/20)+slphouse'1*(pieyedi*2/20)-(slphouse-pieyedi/2)*100/(pieyedi*2/20-pieyedi/2)/100'(100/2/100)
          if casual>1 then casual=1
          if casual<0 then casual=0
if casual2>1 then casual2=1
if casual2<0 then casual2=0
    rainbow=shipleftside(pointidx).c'rgb(255,255,251)
   
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )

rainbow=rgb(d*casual,n*casual,e*casual)',127)
If pointidx2=2 and judgedbottom>timer then
    rainbow=rgb(255*casual2,255*casual2,0)
  ' blowbottomlx=blowbottomlx-1*(bottomzzz*5)
  ' blowbottomly=blowbottomly-1*(bottomzzz*5)
end if
if pointidx2=1 and judgedtop>timer then
        rainbow=rgb(255*casual,255*casual,0)
  '      blowtoplx=blowtoplx-1*(topzzz*5)' Else blowtoplx=blowtoplx+1*(topzzz*5):End If
  '  blowtoply=blowtoply+1*(topzzz*5)' Else blowtoply=blowtoply+.005*(topzzz*5):End If
End If

if pointidx2=1 then
 screenlock
   circle (newX+blowtoplx+(topxxx), newy+blowtoply+(topyyy)-(33+9)),1.5*(topzzz*5),rainbow,,,,F
   screenunlock
   elseif pointidx2=2 then
   if judgedbottom<timer then rainbow=rgb(d*casual2,n*casual2,e*casual2)',127)
screenlock
circle(newx2+blowbottomlx+(bottomxxx),newy2+blowbottomly+(bottomyyy)+(33+9)),1.5*(bottomzzz*5),rainbow,,,,F
screenunlock
end if
end If
NEXT
'next
'end if

'     if (ship1rotatex<0) or (ship2rotatex<0) then 
     'asm
      drawship
     'end asm
 '        end if
     
    
 '    if (ship1rotatex<0) or (ship2rotatex<0) then 

'end if
  'for pointidx2=3 to 2'pointidx2+
    		'if pointidx2=1 then

   FOR pointIdx = 1 To PIXELAGENT2 step+ int(rnd*32)+11' pointa To pointb'1 TO pixelagent1'step+int(rnd*16)+.05
'	if pointidx>numberofpixels then pointidx=point.idx-numberofpixels
    if shiprightside(pointidx).c<>rgb(0,0,0) and shiprightside(pointidx).x+shiprightside(pointidx).y+shiprightside(pointidx).Z<>0 then
   
dim as SINGLE flatz,newz,pimpz,topzzz3,bottomzzz3,shadea,shadeb,shade1,shade2
		if pointidx2=1 then
  ' topzzz3=topzzz*5
topzzz3=topzzz
      '  if topzzz<0 then topzzz3=(topzzz-(topzzz*2))
               ' if topzzz>0 then topzzz3=(topzzz-(topzzz*2))

'ship(pointidx).z=1
'topzzz3=topzzz3*2
'if topzzz3<.25/2 then topzzz3=.25/2
'if topzzz3>.25 then topzzz3=.25
'if topzzz3<.5 then topzzz3=topzzz3*2  
'topzzz3=topzzz3/2
flatx=shiprightside(pointidx).x*((64*.55)*((topzzz3*5)))/(shiprightside(pointidx).z+zoomlevel)
'SHADEA=ship1rotatex*100/(pieyedi/2)/100
'IF SHIP1ROTATEX>=0 THEN 
    SHADE1=3+SHIPrightside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEX<0 THEN
'     SHADE1=3+SHIPrightside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'     END IF
':shade1=shade1-(shade1*2)
'shade1=.5-SHADEA*4
flaty=shiprightside(pointidx).y*((64*.55)*((topzzz3*5)))/(shiprightside(pointidx).z+zoomlevel)
'IF SHIP1ROTATEY>0 THEN  
'    SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEY<0 THEN
'     SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'     END IF
'if timer>judgedtop then toyota=0 else toyota=flat3:end if
flatz=shiprightside(pointidx).Z*((128*8)*.75)*(topzzz3*5)
If judgedtop<Timer Then
 newx = flatx'newx'
newy = (sin(ship1rotatey) * flatz) + (cos(ship1rotatey) * flaty)
newz = (cos(ship1rotatey) * flatz) - (sin(ship1rotatey) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship1rotatex) * flatx) - (sin(ship1rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship1rotatex) * flatx) + (cos(ship1rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy
End If
if judgedtop>Timer then 
 newx = flatx'newx'
newy = (sin(flat2) * flatz) + (cos(flat2) * flaty)
newz = (cos(flat2) * flatz) - (sin(flat2) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) - (sin(flat2) * flatz)
newy=flaty''y = y'
newz = (sin(flat2) * flatx) + (cos(flat2) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
end if




'/((720*2+(720/2)))'/shiprightside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).z+zoomlevel)'((3)
else
           ' bottomzzz=bottomzzz*5
bottomzzz3=bottomzzz
           ' if bottomzzz<0 then bottomzzz=1-(bottomzzz-(bottomzzz*2))
'if bottomzzz<0 then bottomzzz3=(bottomzzz-(bottomzzz*2))
'bottomzzz3=bottomzzz3*2
'if bottomzzz3<.25/2 then bottomzzz3=.25/2
'if bottomzzz3>.25 then bottomzzz3=.25'-.1
flatx=shiprightside(pointidx).x*((64*.55)*((bottomzzz3*5)))/(shiprightside(pointidx).z+zoomlevel)
'IF SHIP2ROTATEX>=0 THEN 
    SHADE2=3+SHIPrightside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEX<=0 THEN  
'    SHADE2=3+SHIPrightside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'     END IF
flaty=shiprightside(pointidx).y*((64*.55)*((bottomzzz3*5)))/(shiprightside(pointidx).z+zoomlevel)
'IF SHIP2ROTATEY>0 THEN 
'    SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEY<0 THEN
'     SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shiprightside(pointidx).Z*((128*8)*.75)*(bottomzzz3*5)'/((720*2+(720/2)))'/shiprightside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).Z+zoomlevel)'((3)
If judgedbottom<Timer then
 newx = flatx'newx'
newy = (sin(ship2rotatey+pieyedi) * flatz) + (cos(ship2rotatey+pieyedi) * flaty)
newz = (cos(ship2rotatey+pieyedi) * flatz) - (sin(ship2rotatey+pieyedi) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship2rotatex) * flatx) - (sin(ship2rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship2rotatex) * flatx) + (cos(ship2rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy
End if
if judgedbottom>Timer then 
 newx = flatx'newx'
newy = (sin(flat1) * flatz) + (cos(flat1) * flaty)
newz = (cos(flat1) * flatz) - (sin(flat1) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
newy=flaty''y = y'
newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) + (sin(flat2) * flaty)
newy = (cos(flat2) * flaty) - (sin(flat2) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
 newy2=newy
 newx2=newx
 newy2=newy
 newx2=newx
end if











end If
'flatx=newx
' flaty=newy
' flatz=newz
 'newx = (cos(flat3) * flatx) - (sin(flat3) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat3) * flatx) + (cos(flat3) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz





'end if
'newz = (cos(ship1rotatex) * flatz) - (sin(ship1rotatex) * flaty)
 'flatx=newx
 'flaty=newy
 'flatz=newz
 'newy=flaty
 rem xyrotatecode
' alphyx=ship(pointidx).x*cos(ship1rotatex-pieyedi/4)
'  alphyy=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' if singletopy<1 and singletopy>0 then singletopy=0
 ' if singlebottomy<1 and singlebottomy>0 then singlebottomy=0

' x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')

'The second set of algorithms is for rotation about the y axis:

'x = (cos(angle) * x') - (sin(angle) * z')
'y = y'
'z = (sin(angle) * x') + (cos(angle) * z')

 'newx=flatx
 'newy2=newy
 'newx2=newx
' newx = (cos(flat3) * flatx) - (sin(flat3) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat3) * flatx) + (cos(flat3) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz
 
' newx = (cos(flat3) * flatx) + (sin(flat3) * flaty)
'newy = (cos(flat3) * flaty) - (sin(flat3) * flatx)
'newz = flatz'
' flatx=newx
' flaty=newy
' flatz=newz
 
' alphyx=ship(pointidx).x*cos(ship1rotatex)
'  alphyx=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' dim as SINGLE aly1,aly2
 ' aly1=ship(pointidx).x*cos(ship2rotatex)
   ' aly1=ship(pointidx).y*cos(ship2rotatey-pieyedi/4)
'casual=alphyx'y*(100/1/30)
'casual2=aly1'2*(100/1/30)
'casual=casual*4
'casual2=casual2*4
'if casual>1 then casual=1
'if casual<0 then casual=0
'if casual2>1 then casual2=1
'if casual2<0 then casual2=0

casual=1*(topzzz*5)'casual+5
casual2=1*(bottomzzz*5)'=casual2+5
if casual<1 then casual=1
if casual2<1 then casual2=1
casual=casual*shade1'*SHADEA
casual2=casual2*shade2'*SHADEB

'casual=1'-((1)*flatz*100/((1280)/2)/100)
'dim x as SINGLE
 
 'rernd2:
' x=rnd'(timered)
 'if x=0 then goto rernd2
'if x<.6 then casual=casual-.08'03333333
' if x>.5 then casual=casual+.08'03333333
'if (pieyedi*2/20)+slphouse'1*(pieyedi*2/20)-(slphouse-pieyedi/2)*100/(pieyedi*2/20-pieyedi/2)/100'(100/2/100)
          if casual>1 then casual=1
          if casual<0 then casual=0
if casual2>1 then casual2=1
if casual2<0 then casual2=0
    rainbow=shiprightside(pointidx).c'rgb(255,255,251)
   
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )

rainbow=rgb(d*casual,n*casual,e*casual)',127)
If pointidx2=2 and judgedbottom>timer then
    rainbow=rgb(255*casual2,255*casual2,0)
  ' blowbottomlx=blowbottomlx-1*(bottomzzz*5)
  ' blowbottomly=blowbottomly-1*(bottomzzz*5)
end if
if pointidx2=1 and judgedtop>timer then
        rainbow=rgb(255*casual,255*casual,0)
  '      blowtoplx=blowtoplx-1*(topzzz*5)' Else blowtoplx=blowtoplx+1*(topzzz*5):End If
  '  blowtoply=blowtoply+1*(topzzz*5)' Else blowtoply=blowtoply+.005*(topzzz*5):End If
End If

if pointidx2=1 then
 ScreenLock
   circle (newX+blowtoprx+(topxxx), newy+blowtopry+(topyyy)-(33+9)),1.5*(topzzz*5),rainbow,,,,F
   screenunlock
   elseif pointidx2=2 then
   if judgedbottom<timer then rainbow=rgb(d*casual2,n*casual2,e*casual2)',127)
screenlock
circle(newx2+blowbottomrx+(bottomxxx),newy2+blowbottomry+(bottomyyy)+(33+9)),1.5*(bottomzzz*5),rainbow,,,,F
screenunlock
end if
end If
NEXT

'next
end if
GoTo runtocode
 if (ship1rotatex=0) then 

 ' for pointidx2=3 to 2'pointidx2+
    		'if pointidx2=1 then

   FOR pointIdx = 1 To PIXELAGENT2 step+ int(rnd*32)+11' pointa to pointb''step+int(rnd*16)+.125
'	if pointidx>numberofpixels then pointidx=point.idx-numberofpixels
    if shipleftside(pointidx).c<>rgb(0,0,0) and shipleftside(pointidx).x+shipleftside(pointidx).y+shipleftside(pointidx).Z<>0 then
   
dim as SINGLE flatz,newz,pimpz,topzzz3,bottomzzz3,shadea,shadeb,shade1,shade2
		if pointidx2=1 then
     topzzz3=topzzz
      '  if topzzz<0 then topzzz3=(topzzz-(topzzz*2))
               ' if topzzz>0 then topzzz3=(topzzz-(topzzz*2))

'ship(pointidx).Z=1
'topzzz3=topzzz3*2
'if topzzz3<.25/2 then topzzz3=.25/2
'if topzzz3>.25 then topzzz3=.25
'if topzzz3<.5 then topzzz3=topzzz3*2  
'topzzz3=topzzz3/2
flatx=shipleftside(pointidx).x*((64*.55)*((topzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'SHADEA=ship1rotatex*100/(pieyedi/2)/100
'IF SHIP1ROTATEX>=0 THEN 
    SHADE1=3+SHIPleftside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEX<0 THEN
'     SHADE1=3+SHIPleftside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'     END IF
   
':shade1=shade1-(shade1*2)
'shade1=.5-SHADEA*4
flaty=shipleftside(pointidx).y*((64*.55)*((topzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP1ROTATEY>0 THEN 
'    SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEY<0 THEN
'     SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shipleftside(pointidx).Z*((128*8)*.75)*(topzzz3*5)'/((720*2+(720/2)))'/shipleftside(pointidx).Z)/((720*2+(720/2))'/shipleftside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).Z+zoomlevel)'((3)

 newx = flatx'newx'
newy = (sin(ship1rotatey) * flatz) + (cos(ship1rotatey) * flaty)
newz = (cos(ship1rotatey) * flatz) - (sin(ship1rotatey) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship1rotatex) * flatx) - (sin(ship1rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship1rotatex) * flatx) + (cos(ship1rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy


if judgedtop>Timer then 
 newx = flatx'newx'
newy = (sin(flat2) * flatz) + (cos(flat2) * flaty)
newz = (cos(flat2) * flatz) - (sin(flat2) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) - (sin(flat2) * flatz)
newy=flaty''y = y'
newz = (sin(flat2) * flatx) + (cos(flat2) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
end If
















else
           ' bottomzzz=bottomzzz*5
bottomzzz3=bottomzzz
           ' if bottomzzz<0 then bottomzzz=1-(bottomzzz-(bottomzzz*2))
'if bottomzzz<0 then bottomzzz3=(bottomzzz-(bottomzzz*2))
'bottomzzz3=bottomzzz3*2
'if bottomzzz3<.25/2 then bottomzzz3=.25/2
'if bottomzzz3>.25 then bottomzzz3=.25'-.1
flatx=shipleftside(pointidx).x*((64*.55)*((bottomzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP2ROTATEX>=0 THEN 
    SHADE2=3+SHIPleftside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEX<=0 THEN  
'    SHADE2=3+SHIPleftside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'     END IF
flaty=shipleftside(pointidx).y*((64*.55)*((bottomzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP2ROTATEY>0 THEN 
'    SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEY<0 THEN
'     SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shipleftside(pointidx).Z*((128*8)*.75)*(bottomzzz3*5)'/((720*2+(720/2)))'/shipleftside(pointidx).Z)/((720*2+(720/2))'/shipleftside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))'/(ship(pointidx).Z+zoomlevel)'((3)

 newx = flatx'newx'
newy = (sin(ship2rotatey+pieyedi) * flatz) + (cos(ship2rotatey+pieyedi) * flaty)
newz = (cos(ship2rotatey+pieyedi) * flatz) - (sin(ship2rotatey+pieyedi) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship2rotatex) * flatx) - (sin(ship2rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship2rotatex) * flatx) + (cos(ship2rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy


if judgedbottom>Timer then 
 newx = flatx'newx'
newy = (sin(flat1) * flatz) + (cos(flat1) * flaty)
newz = (cos(flat1) * flatz) - (sin(flat1) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
newy=flaty''y = y'
newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flat2) * flatx) + (sin(flat2) * flaty)
newy = (cos(flat2) * flaty) - (sin(flat2) * flatx)
newz = flatz'
'end if
 flatx=newx
 flaty=newy
 flatz=newz
 newy2=newy
 newx2=newx
end If






end if

'flatx=newx
' flaty=newy
' flatz=newz
 'newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz





'end if
'newz = (cos(ship1rotatex) * flatz) - (sin(ship1rotatex) * flaty)
 'flatx=newx
 'flaty=newy
 'flatz=newz
 'newy=flaty
 rem xyrotatecode
' alphyx=ship(pointidx).x*cos(ship1rotatex-pieyedi/4)
'  alphyy=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
  'if singletopy<1 and singletopy>0 then singletopy=0
  'if singlebottomy<1 and singlebottomy>0 then singlebottomy=0

' x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')

'The second set of algorithms is for rotation about the y axis:

'x = (cos(angle) * x') - (sin(angle) * z')
'y = y'
'z = (sin(angle) * x') + (cos(angle) * z')

 'newx=flatx
 'newy2=newy
 'newx2=newx
' newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz
 
' newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
'newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
'newz = flatz'
' flatx=newx
' flaty=newy
' flatz=newz
 
' alphyx=ship(pointidx).x*cos(ship1rotatex)
'  alphyx=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' dim as SINGLE aly1,aly2
 ' aly1=ship(pointidx).x*cos(ship2rotatex)
   ' aly1=ship(pointidx).y*cos(ship2rotatey-pieyedi/4)
'casual=alphyx'y*(100/1/30)
'casual2=aly1'2*(100/1/30)
'casual=casual*4
'casual2=casual2*4
'if casual>1 then casual=1
'if casual<0 then casual=0
'if casual2>1 then casual2=1
'if casual2<0 then casual2=0

casual=1*(topzzz*5)'casual+5
casual2=1*(bottomzzz*5)'=casual2+5
if casual<1 then casual=1
if casual2<1 then casual2=1
casual=casual*shade1'*SHADEA
casual2=casual2*shade2'*SHADEB

'casual=1'-((1)*flatz*100/((1280)/2)/100)
'dim x as SINGLE
 
 'rernd2:
' x=rnd'(timered)
 'if x=0 then goto rernd2
'if x<.6 then casual=casual-.08'03333333
' if x>.5 then casual=casual+.08'03333333
'if (pieyedi*2/20)+slphouse'1*(pieyedi*2/20)-(slphouse-pieyedi/2)*100/(pieyedi*2/20-pieyedi/2)/100'(100/2/100)
          if casual>1 then casual=1
          if casual<0 then casual=0
if casual2>1 then casual2=1
if casual2<0 then casual2=0
    rainbow=shipleftside(pointidx).c'rgb(255,255,251)
   
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )

rainbow=rgb(d*casual,n*casual,e*casual)',127)
If pointidx2=2 and judgedbottom>timer then
    rainbow=rgb(255*casual2,255*casual2,0)
  ' blowbottomlx=blowbottomlx-1*(bottomzzz*5)
  ' blowbottomly=blowbottomly-1*(bottomzzz*5)
end if
if pointidx2=1 and judgedtop>timer then
        rainbow=rgb(255*casual,255*casual,0)
  '      blowtoplx=blowtoplx-1*(topzzz*5)' Else blowtoplx=blowtoplx+1*(topzzz*5):End If
  '  blowtoply=blowtoply+1*(topzzz*5)' Else blowtoply=blowtoply+.005*(topzzz*5):End If
End If

if pointidx2=1 then
 screenlock
   circle (newX+blowtoplx+(topxxx), newy+blowtoply+(topyyy)-(33+9)),1.5*(topzzz*5),rainbow,,,,F
   screenunlock
   elseif pointidx2=2 then
   if judgedbottom<timer then rainbow=rgb(d*casual2,n*casual2,e*casual2)',127)
screenlock
circle(newx2+blowbottomlx+(bottomxxx),newy2+blowbottomly+(bottomyyy)+(33+9)),1.5*(bottomzzz*5),rainbow,,,,F
screenunlock
end if
end If
NEXT
'next
'end if

'     if (ship1rotatex<0) or (ship2rotatex<0) then 
     drawship
 '        end if
     
    
 '    if (ship1rotatex<0) or (ship2rotatex<0) then 

'end if
  'for pointidx2=3 to 2'pointidx2+
    		'if pointidx2=1 then

  FOR pointIdx = 1 To PIXELAGENT2 step+ int(rnd*32)+11' pointa to pointb'1 'step+int(rnd*6)+1
'	if pointidx>numberofpixels then pointidx=point.idx-numberofpixels
    if shiprightside(pointidx).c<>rgb(0,0,0) and shiprightside(pointidx).x+shiprightside(pointidx).y+shiprightside(pointidx).Z<>0 then
   
dim as SINGLE flatz,newz,pimpz,topzzz3,bottomzzz3,shadea,shadeb,shade1,shade2
		if pointidx2=1 then
                   ' topzzz3=topzzz*5
topzzz3=topzzz
      '  if topzzz<0 then topzzz3=(topzzz-(topzzz*2))
               ' if topzzz>0 then topzzz3=(topzzz-(topzzz*2))

'ship(pointidx).Z=1
'topzzz3=topzzz3*2
'if topzzz3<.25/2 then topzzz3=.25/2
'if topzzz3>.25 then topzzz3=.25
'if topzzz3<.5 then topzzz3=topzzz3*2  
'topzzz3=topzzz3/2
flatx=shiprightside(pointidx).x*((64*.55)*((topzzz3*5)))/(shiprightside(pointidx).Z+zoomlevel)
'SHADEA=ship1rotatex*100/(pieyedi/2)/100
'IF SHIP1ROTATEX>=0 THEN 
    SHADE1=3+1+SHIPrightside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEX<0 THEN
'     SHADE1=3+1+SHIPrightside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'     END IF
   
':SHADE1=3+1+shade1-(shade1*2)
'SHADE1=3+1+.5-SHADEA*4
flaty=shiprightside(pointidx).y*((64*.55)*((topzzz3*5)))/(shiprightside(pointidx).Z+zoomlevel)
'IF SHIP1ROTATEY>0 THEN 
'    SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEY<0 THEN
'     SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shiprightside(pointidx).Z*((128*8)*.75)*(topzzz3*5)
 newx = flatx'newx'
newy = (sin(ship1rotatey) * flatz) + (cos(ship1rotatey) * flaty)
newz = (cos(ship1rotatey) * flatz) - (sin(ship1rotatey) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship1rotatex) * flatx) - (sin(ship1rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship1rotatex) * flatx) + (cos(ship1rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy



'/((720*2+(720/2)))'/shiprightside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))''((3)
else
           ' bottomzzz=bottomzzz*5
bottomzzz3=bottomzzz
           ' if bottomzzz<0 then bottomzzz=1-(bottomzzz-(bottomzzz*2))
'if bottomzzz<0 then bottomzzz3=(bottomzzz-(bottomzzz*2))
'bottomzzz3=bottomzzz3*2
'if bottomzzz3<.25/2 then bottomzzz3=.25/2
'if bottomzzz3>.25 then bottomzzz3=.25'-.1
flatx=shiprightside(pointidx).x*((64*.55)*((bottomzzz3*5)))/(shiprightside(pointidx).Z+zoomlevel)
'IF SHIP2ROTATEX>0 THEN 
    SHADE2=3+1+SHIPrightside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEX<=0 THEN     SHADE2=3+1+SHIPrightside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'     END IF
flaty=shiprightside(pointidx).y*((64*.55)*((bottomzzz3*5)))/(shiprightside(pointidx).Z+zoomlevel)
'IF SHIP2ROTATEY>0 THEN 
'    SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEY<0 THEN
'     SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shiprightside(pointidx).Z*((128*8)*.75)*(bottomzzz3*5)

 newx = flatx'newx'
newy = (sin(ship2rotatey+pieyedi) * flatz) + (cos(ship2rotatey+pieyedi) * flaty)
newz = (cos(ship2rotatey+pieyedi) * flatz) - (sin(ship2rotatey+pieyedi) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship2rotatex) * flatx) - (sin(ship2rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship2rotatex) * flatx) + (cos(ship2rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy

'/((720*2+(720/2)))'/shiprightside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))''((3)
end If
'flatx=newx
' flaty=newy
' flatz=newz
 'newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz






'end if
'newz = (cos(ship1rotatex) * flatz) - (sin(ship1rotatex) * flaty)
 'flatx=newx
 'flaty=newy
 'flatz=newz
 'newy=flaty
 rem xyrotatecode
' alphyx=ship(pointidx).x*cos(ship1rotatex-pieyedi/4)
'  alphyy=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' if singletopy<1 and singletopy>0 then singletopy=0
 ' if singlebottomy<1 and singlebottomy>0 then singlebottomy=0

' x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')

'The second set of algorithms is for rotation about the y axis:

'x = (cos(angle) * x') - (sin(angle) * z')
'y = y'
'z = (sin(angle) * x') + (cos(angle) * z')

 'newx=flatx
 'newy2=newy
 'newx2=newx
' newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz
 
' newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
'newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
'newz = flatz'
' flatx=newx
' flaty=newy
' flatz=newz
 
' alphyx=ship(pointidx).x*cos(ship1rotatex)
'  alphyx=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' dim as SINGLE aly1,aly2
 ' aly1=ship(pointidx).x*cos(ship2rotatex)
   ' aly1=ship(pointidx).y*cos(ship2rotatey-pieyedi/4)
'casual=alphyx'y*(100/1/30)
'casual2=aly1'2*(100/1/30)
'casual=casual*4
'casual2=casual2*4
'if casual>1 then casual=1
'if casual<0 then casual=0
'if casual2>1 then casual2=1
'if casual2<0 then casual2=0

casual=1*(topzzz*5)'casual+5
casual2=1*(bottomzzz*5)'=casual2+5
if casual<1 then casual=1
if casual2<1 then casual2=1
casual=casual*shade1'*SHADEA
casual2=casual2*shade2'*SHADEB

'casual=1'-((1)*flatz*100/((1280)/2)/100)
'dim x as SINGLE
 
 'rernd2:
' x=rnd'(timered)
 'if x=0 then goto rernd2
'if x<.6 then casual=casual-.08'03333333
' if x>.5 then casual=casual+.08'03333333
'if (pieyedi*2/20)+slphouse'1*(pieyedi*2/20)-(slphouse-pieyedi/2)*100/(pieyedi*2/20-pieyedi/2)/100'(100/2/100)
          if casual>1 then casual=1
          if casual<0 then casual=0
if casual2>1 then casual2=1
if casual2<0 then casual2=0
    rainbow=shiprightside(pointidx).c'rgb(255,255,251)
   
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )

rainbow=rgb(d*casual,n*casual,e*casual)',127)
If pointidx2=2 and judgedbottom>timer then
    rainbow=rgb(255*casual2,255*casual2,0)
  ' blowbottomlx=blowbottomlx-1*(bottomzzz*5)
  ' blowbottomly=blowbottomly-1*(bottomzzz*5)
end if
if pointidx2=1 and judgedtop>timer then
        rainbow=rgb(255*casual,255*casual,0)
  '      blowtoplx=blowtoplx-1*(topzzz*5)' Else blowtoplx=blowtoplx+1*(topzzz*5):End If
  '  blowtoply=blowtoply+1*(topzzz*5)' Else blowtoply=blowtoply+.005*(topzzz*5):End If
End If


if pointidx2=1 then
 screenlock
   circle (newX+blowtoprx+(topxxx), newy+blowtopry+(topyyy)-(33+9)),1.5*(topzzz*5),rainbow,,,,F
   screenunlock
elseif pointidx2=2 Then
   if judgedbottom<timer then rainbow=rgb(d*casual2,n*casual2,e*casual2)',127)
screenlock
circle(newx2+blowbottomrx+(bottomxxx),newy2+blowbottomry+(bottomyyy)+(33+9)),1.5*(bottomzzz*5),rainbow,,,,F
screenunlock
end if
end If
NEXT

'next
end if
 if (ship2rotatex=0) then 

 ' for pointidx2=3 to 2'pointidx2+
    		'if pointidx2=1 then

  FOR pointIdx = 1 To PIXELAGENT2 step+ int(rnd*32)+11' pointa to pointb'1 step+int(rnd*6)+1
'	if pointidx>numberofpixels then pointidx=point.idx-numberofpixels
    if shipleftside(pointidx).c<>rgb(0,0,0) and shipleftside(pointidx).x+shipleftside(pointidx).y+shipleftside(pointidx).Z<>0 then
   
dim as SINGLE flatz,newz,pimpz,topzzz3,bottomzzz3,shadea,shadeb,shade1,shade2
		if pointidx2=1 then
                   ' topzzz3=topzzz*5
topzzz3=topzzz
      '  if topzzz<0 then topzzz3=(topzzz-(topzzz*2))
               ' if topzzz>0 then topzzz3=(topzzz-(topzzz*2))

'ship(pointidx).Z=1
'topzzz3=topzzz3*2
'if topzzz3<.25/2 then topzzz3=.25/2
'if topzzz3>.25 then topzzz3=.25
'if topzzz3<.5 then topzzz3=topzzz3*2  
'topzzz3=topzzz3/2
flatx=shipleftside(pointidx).x*((64*.55)*((topzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'SHADEA=ship1rotatex*100/(pieyedi/2)/100
'IF SHIP1ROTATEX>=0 THEN 
    SHADE1=3+1+SHIPleftside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEX<0 THEN
'     SHADE1=3+1+SHIPleftside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'     END IF
   
':SHADE1=3+1+shade1-(shade1*2)
'SHADE1=3+1+.5-SHADEA*4
flaty=shipleftside(pointidx).y*((64*.55)*((topzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP1ROTATEY>0 THEN 
'    SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEY<0 THEN
'     SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shipleftside(pointidx).Z*((128*8)*.75)*(topzzz3*5)'/((720*2+(720/2)))'/shipleftside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))''((3)

 newx = flatx'newx'
newy = (sin(ship1rotatey) * flatz) + (cos(ship1rotatey) * flaty)
newz = (cos(ship1rotatey) * flatz) - (sin(ship1rotatey) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship1rotatex) * flatx) - (sin(ship1rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship1rotatex) * flatx) + (cos(ship1rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy

else
           ' bottomzzz=bottomzzz*5
bottomzzz3=bottomzzz
           ' if bottomzzz<0 then bottomzzz=1-(bottomzzz-(bottomzzz*2))
'if bottomzzz<0 then bottomzzz3=(bottomzzz-(bottomzzz*2))
'bottomzzz3=bottomzzz3*2
'if bottomzzz3<.25/2 then bottomzzz3=.25/2
'if bottomzzz3>.25 then bottomzzz3=.25'-.1
flatx=shipleftside(pointidx).x*((64*.55)*((bottomzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP2ROTATEX>0 THEN 
    SHADE2=3+1+SHIPleftside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEX<=0 THEN     SHADE2=3+1+SHIPleftside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'     END IF
flaty=shipleftside(pointidx).y*((64*.55)*((bottomzzz3*5)))/(shipleftside(pointidx).Z+zoomlevel)
'IF SHIP2ROTATEY>0 THEN 
'    SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEY<0 THEN
'     SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shipleftside(pointidx).Z*((128*8)*.75)*(bottomzzz3*5)'/((720*2+(720/2)))'/shipleftside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))''((3)
 newx = flatx'newx'
newy = (sin(ship2rotatey+pieyedi) * flatz) + (cos(ship2rotatey+pieyedi) * flaty)
newz = (cos(ship2rotatey+pieyedi) * flatz) - (sin(ship2rotatey+pieyedi) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship2rotatex) * flatx) - (sin(ship2rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship2rotatex) * flatx) + (cos(ship2rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy

end if
'flatx=newx
' flaty=newy
' flatz=newz
 'newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz





'end if
'newz = (cos(ship1rotatex) * flatz) - (sin(ship1rotatex) * flaty)
 'flatx=newx
 'flaty=newy
 'flatz=newz
 'newy=flaty
 rem xyrotatecode
' alphyx=ship(pointidx).x*cos(ship1rotatex-pieyedi/4)
'  alphyy=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
  'if singletopy<1 and singletopy>0 then singletopy=0
  'if singlebottomy<1 and singlebottomy>0 then singlebottomy=0

' x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')

'The second set of algorithms is for rotation about the y axis:

'x = (cos(angle) * x') - (sin(angle) * z')
'y = y'
'z = (sin(angle) * x') + (cos(angle) * z')

 'newx=flatx
 'newy2=newy
 'newx2=newx
' newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz
 
' newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
'newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
'newz = flatz'
' flatx=newx
' flaty=newy
' flatz=newz
 
' alphyx=ship(pointidx).x*cos(ship1rotatex)
'  alphyx=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' dim as SINGLE aly1,aly2
 ' aly1=ship(pointidx).x*cos(ship2rotatex)
   ' aly1=ship(pointidx).y*cos(ship2rotatey-pieyedi/4)
'casual=alphyx'y*(100/1/30)
'casual2=aly1'2*(100/1/30)
'casual=casual*4
'casual2=casual2*4
'if casual>1 then casual=1
'if casual<0 then casual=0
'if casual2>1 then casual2=1
'if casual2<0 then casual2=0

casual=1*(topzzz*5)'casual+5
casual2=1*(bottomzzz*5)'=casual2+5
if casual<1 then casual=1
if casual2<1 then casual2=1
casual=casual*shade1'*SHADEA
casual2=casual2*shade2'*SHADEB

'casual=1'-((1)*flatz*100/((1280)/2)/100)
'dim x as SINGLE
 
 'rernd2:
' x=rnd'(timered)
 'if x=0 then goto rernd2
'if x<.6 then casual=casual-.08'03333333
' if x>.5 then casual=casual+.08'03333333
'if (pieyedi*2/20)+slphouse'1*(pieyedi*2/20)-(slphouse-pieyedi/2)*100/(pieyedi*2/20-pieyedi/2)/100'(100/2/100)
          if casual>1 then casual=1
          if casual<0 then casual=0
if casual2>1 then casual2=1
if casual2<0 then casual2=0
    rainbow=shipleftside(pointidx).c'rgb(255,255,251)
   
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )

rainbow=rgb(d*casual,n*casual,e*casual)',127)
If pointidx2=2 and judgedbottom>timer then
    rainbow=rgb(255*casual2,255*casual2,0)
  ' blowbottomlx=blowbottomlx-1*(bottomzzz*5)
  ' blowbottomly=blowbottomly-1*(bottomzzz*5)
end if
if pointidx2=1 and judgedtop>timer then
        rainbow=rgb(255*casual,255*casual,0)
  '      blowtoplx=blowtoplx-1*(topzzz*5)' Else blowtoplx=blowtoplx+1*(topzzz*5):End If
  '  blowtoply=blowtoply+1*(topzzz*5)' Else blowtoply=blowtoply+.005*(topzzz*5):End If
End If

if pointidx2=1 then
 screenlock
   circle (newX+blowtoplx+(topxxx), newy+blowtoply+(topyyy)-(33+9)),1.5*(topzzz*5),rainbow,,,,F
   screenunlock
   elseif pointidx2=2 then
   if judgedbottom<timer then rainbow=rgb(d*casual2,n*casual2,e*casual2)',127)
screenlock
circle(newx2+blowbottomlx+(bottomxxx),newy2+blowbottomly+(bottomyyy)+(33+9)),1.5*(bottomzzz*5),rainbow,,,,F
screenunlock
end if
end If
NEXT
'next
'end if

'     if (ship1rotatex<0) or (ship2rotatex<0) then 
     drawship
 '        end if
     
    
 '    if (ship1rotatex<0) or (ship2rotatex<0) then 

'end if
  'for pointidx2=3 to 2'pointidx2+
    		'if pointidx2=1 then

  FOR pointIdx = 1 TO PIXELAGENT2 Step+ int(rnd*32)+1' pointa to pointb'1 step+int(rnd*6)+1
'	if pointidx>numberofpixels then pointidx=point.idx-numberofpixels
    if shiprightside(pointidx).c<>rgb(0,0,0) and shiprightside(pointidx).x+shiprightside(pointidx).y+shiprightside(pointidx).Z<>0 then
   
dim as SINGLE flatz,newz,pimpz,topzzz3,bottomzzz3,shadea,shadeb,shade1,shade2
		if pointidx2=1 then
                   ' topzzz3=topzzz*5
topzzz3=topzzz
      '  if topzzz<0 then topzzz3=(topzzz-(topzzz*2))
               ' if topzzz>0 then topzzz3=(topzzz-(topzzz*2))

'ship(pointidx).Z=1
'topzzz3=topzzz3*2
'if topzzz3<.25/2 then topzzz3=.25/2
'if topzzz3>.25 then topzzz3=.25
'if topzzz3<.5 then topzzz3=topzzz3*2  
'topzzz3=topzzz3/2
flatx=shiprightside(pointidx).x*((64*.55)*((topzzz3*5)))/(shiprightside(pointidx).Z+zoomlevel)
'SHADEA=ship1rotatex*100/(pieyedi/2)/100
'IF SHIP1ROTATEX>=0 THEN 
    SHADE1=3+1+SHIPrightside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEX<0 THEN
'     SHADE1=3+1+SHIPrightside(POINTIDX).X*COS(SHIP1ROTATEX+(PIEYEDI/2))
'     END IF
   
':SHADE1=3+1+shade1-(shade1*2)
'SHADE1=3+1+.5-SHADEA*4
flaty=shiprightside(pointidx).y*((64*.55)*((topzzz3*5)))/(shiprightside(pointidx).Z+zoomlevel)
'IF SHIP1ROTATEY>0 THEN 
'    SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP1ROTATEY<0 THEN
'     SHADEA=SHIP(POINTIDX).X*COS(SHIP1ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shiprightside(pointidx).Z*((128*8)*.75)*(topzzz3*5)'/((720*2+(720/2)))'/shiprightside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))''((3)


 newx = flatx'newx'
newy = (sin(ship1rotatey) * flatz) + (cos(ship1rotatey) * flaty)
newz = (cos(ship1rotatey) * flatz) - (sin(ship1rotatey) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship1rotatex) * flatx) - (sin(ship1rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship1rotatex) * flatx) + (cos(ship1rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
'newx2=newx
'newy2=newy

else
           ' bottomzzz=bottomzzz*5
bottomzzz3=bottomzzz
           ' if bottomzzz<0 then bottomzzz=1-(bottomzzz-(bottomzzz*2))
'if bottomzzz<0 then bottomzzz3=(bottomzzz-(bottomzzz*2))
'bottomzzz3=bottomzzz3*2
'if bottomzzz3<.25/2 then bottomzzz3=.25/2
'if bottomzzz3>.25 then bottomzzz3=.25'-.1
flatx=shiprightside(pointidx).x*((64*.55)*((bottomzzz3*5)))/(shiprightside(pointidx).Z+zoomlevel)
'IF SHIP2ROTATEX>0 THEN 
    SHADE2=3+1+SHIPrightside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEX<=0 THEN     SHADE2=3+1+SHIPrightside(POINTIDX).X*COS(SHIP2ROTATEX+(PIEYEDI/2))
'     END IF
flaty=shiprightside(pointidx).y*((64*.55)*((bottomzzz3*5)))/(shiprightside(pointidx).Z+zoomlevel)
'IF SHIP2ROTATEY>0 THEN 
'    SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'ELSEIF SHIP2ROTATEY<0 THEN
'     SHADEB=SHIP(POINTIDX).X*COS(SHIP2ROTATEY+(PIEYEDI/2))
'     END IF
flatz=shiprightside(pointidx).Z*((128*8)*.75)*(bottomzzz3*5)'/((720*2+(720/2)))'/shiprightside(pointidx).Z)'*1000'*1'*(.25-(.25/3)+(topzzz3*3))''((3)

 newx = flatx'newx'
newy = (sin(ship2rotatey+pieyedi) * flatz) + (cos(ship2rotatey+pieyedi) * flaty)
newz = (cos(ship2rotatey+pieyedi) * flatz) - (sin(ship2rotatey+pieyedi) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(ship2rotatex) * flatx) - (sin(ship2rotatex) * flatz)
newy=flaty''y = y'
newz = (sin(ship2rotatex) * flatx) + (cos(ship2rotatex) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
newx2=newx
newy2=newy


end if
'flatx=newx
' flaty=newy
' flatz=newz
 'newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz





'flatx2=flatx
'flaty2=flaty
'flatz2=flatz'*((1280)/2)


'end if
'newz = (cos(ship1rotatex) * flatz) - (sin(ship1rotatex) * flaty)
 'flatx=newx
 'flaty=newy
 'flatz=newz
 'newy=flaty
 rem xyrotatecode
' alphyx=ship(pointidx).x*cos(ship1rotatex-pieyedi/4)
'  alphyy=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' if singletopy<1 and singletopy>0 then singletopy=0
 ' if singlebottomy<1 and singlebottomy>0 then singlebottomy=0

' x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')

'The second set of algorithms is for rotation about the y axis:

'x = (cos(angle) * x') - (sin(angle) * z')
'y = y'
'z = (sin(angle) * x') + (cos(angle) * z')

 'newx=flatx
 'newy2=newy
 'newx2=newx
' newx = (cos(flat1) * flatx) - (sin(flat1) * flatz)
'newy=flaty''y = y'
'newz = (sin(flat1) * flatx) + (cos(flat1) * flatz)
' flatx=newx
' flaty=newy
' flatz=newz
 
' newx = (cos(flat1) * flatx) + (sin(flat1) * flaty)
'newy = (cos(flat1) * flaty) - (sin(flat1) * flatx)
'newz = flatz'
' flatx=newx
' flaty=newy
' flatz=newz
 
' alphyx=ship(pointidx).x*cos(ship1rotatex)
'  alphyx=ship(pointidx).y*cos(ship1rotatey-pieyedi/4)
 ' dim as SINGLE aly1,aly2
 ' aly1=ship(pointidx).x*cos(ship2rotatex)
   ' aly1=ship(pointidx).y*cos(ship2rotatey-pieyedi/4)
'casual=alphyx'y*(100/1/30)
'casual2=aly1'2*(100/1/30)
'casual=casual*4
'casual2=casual2*4
'if casual>1 then casual=1
'if casual<0 then casual=0
'if casual2>1 then casual2=1
'if casual2<0 then casual2=0

casual=1*(topzzz*5)'casual+5
casual2=1*(bottomzzz*5)'=casual2+5
if casual<1 then casual=1
if casual2<1 then casual2=1
casual=casual*shade1'*SHADEA
casual2=casual2*shade2'*SHADEB

'casual=1'-((1)*flatz*100/((1280)/2)/100)
'dim x as SINGLE
 
 'rernd2:
' x=rnd'(timered)
 'if x=0 then goto rernd2
'if x<.6 then casual=casual-.08'03333333
' if x>.5 then casual=casual+.08'03333333
'if (pieyedi*2/20)+slphouse'1*(pieyedi*2/20)-(slphouse-pieyedi/2)*100/(pieyedi*2/20-pieyedi/2)/100'(100/2/100)
          if casual>1 then casual=1
          if casual<0 then casual=0
if casual2>1 then casual2=1
if casual2<0 then casual2=0
    rainbow=shiprightside(pointidx).c'rgb(255,255,251)
   
d = RGBA_R( rainbow )
n = RGBA_G( rainbow )
e = RGBA_B( rainbow )
rainbow=rgb(d*casual,n*casual,e*casual)',127)
If pointidx2=2 and judgedbottom>timer then
    rainbow=rgb(255*casual2,255*casual2,0)
  ' blowbottomlx=blowbottomlx-1*(bottomzzz*5)
  ' blowbottomly=blowbottomly-1*(bottomzzz*5)
end if
if pointidx2=1 and judgedtop>timer then
        rainbow=rgb(255*casual,255*casual,0)
  '      blowtoplx=blowtoplx-1*(topzzz*5)' Else blowtoplx=blowtoplx+1*(topzzz*5):End If
  '  blowtoply=blowtoply+1*(topzzz*5)' Else blowtoply=blowtoply+.005*(topzzz*5):End If
End If

if pointidx2=1 then
 ScreenLock
   circle (newX+blowtoprx+(topxxx), newy+blowtopry+(topyyy)-(33+9)),1.5*(topzzz*5),rainbow,,,,F
   screenunlock
   elseif pointidx2=2 then
   if judgedbottom<timer then rainbow=rgb(d*casual2,n*casual2,e*casual2)',127)
screenlock
circle(newx2+blowbottomrx+(bottomxxx),newy2+blowbottomry+(bottomyyy)+(33+9)),1.5*(bottomzzz*5),rainbow,,,,F
screenunlock
end if
end If
NEXT

'next
end if
runtocode:
Next
'shipsflag=1
end if
'if   (game2=1 or game2=12) then shipsflag=1
'dim as SINGLE temphemp,countercyc
'  end if
'  temphemp=(1000)*(counter_cycles*100/totalcycles/100)
  'if temphemp>0 and temphemp<=(1000/30) then 
  '    sleep (temphemp,1)
  'else
  '    sleep (1,1)
 ' end if
 'sleep (1,1)
 'counter_end()
 ' dim as SINGLE temphemp',countercyc
'  countercyc=counter_cycles
 ' counter_cycles=0
 ' 
 ' temphemp=1000*(counter_cycles*100/totalcycles/100)
' print temphemp:sleep
 'if (temphemp>0 and temphemp<(1000/60)) then 
 '     sleep ((1000/60)-(temphemp),1)
 ' else
     ' sleep (1000/60,1)
 ' end if
 'counter_begin(1,0,0)
 'GAME2=11
    for astrocnt=3 to 1 step -1

if game2=1 or game2=12 or game2=2 AND ASTROZ(ASTROCNT)>.20 then
        zoomlevel=-1.5'.5'6.33'9'6.5'4'3.5'4.66'10'2.5

if astroz(astrocnt)>.20 then
 ' astrocnt=1  


        'if astroz(astrocnt)<-1.5 then
         if astroz(astrocnt)>1.6 or ((astrot(astrocnt)=4 and astrox(astrocnt)<0) or (astrot(astrocnt)=2 and astrox(astrocnt)>1280)) then'.7125 then
         '    timer7=int(timer+16)
        ' if rnd<5 then
       ' randomize timer
      ' 1stagain:
      screenset 2,0
Cls
astroh(astrocnt)=0
pixela(astrocnt)=0
'circle(16/2,16/2),16,rgb(192,192,192),,,,F
'screencopy 2,0
dim randa as integer
'dim as single flatter(8)
dim as integer randax,randay,randaz,sunday
'dim t8t8 as integer
'for t8t8=1 to 3
'RANDOMIZE RND+TIMER+Rnd+timer
Dim As Integer rta =Int(Rnd*3.6)+1
If rta<1 Then rta=1

  'if astrocount<>160 then
  '           astrocount=160
'    for astrocnt=1 to 3
 '        screenset 2,0
 
  dim as single scooby,sizey,sizey2
 sizey=int(16*rnd)+1
  if sizey>16 then sizey=16        

 if sizey<8 then sizey=8
 sizey2=sizey
paint(0,0),rgb(0,0,0)
If rta=1 Then
'Circle(SIZEY,sizey),sizey*2,rgb(0,255,0),,,,F
Line(1,1)-(sizey*2,sizey*2),RGB(0,255,0),bf	
EndIf
If rta>1 Then circle(SIZEY,sizey),sizey*2,rgb(192,192,192),,,,F
'screencopy 2,0
'dim randa as integer
'dim as single flatter(8)
'dim as integer randax,randay,randaz,sunday
If rta>1 Then
for randa=5 to int(rnd*12)
    randax=int(rnd*(sizey*2))+1
    sunday=int(rnd*(sizey*2/3))+1
   ' if sunday<5
    randay=int(rnd*(sizey*2))+1
    for randaz=sunday to 1 step -1'-sunday to sunday step+1
        'sunday=int(rnd*9)+1
        dim as integer sun=192*(randaz*100/sunday/100)
        if sun<16 then sun=16
        
    ' then
'    If rta<2 Then  circle(randax,randay),randaz,rgb(0,sun,0)
'     If rta=1 Then   
'      circle(randax,randay),randaz,rgb(0,sun,0)
'      Else
      circle(randax,randay),randaz,rgb(sun,sun,sun)
'     End If
     next randaz
next randa
End If
If rta=1 Then 
Line(1,sizey*2/2+(sizey/4))-(sizey*2,sizey*2/2-(sizey/4)),RGB(255,0,0),bf'7,7)
Line(sizey*2/2-(sizey/4),1)-(sizey*2/2+(sizey/4),sizey*2),RGB(255,0,0),bf'7,7)	
EndIf
'Draw String(sizey/4,sizey/4),"LIFE",RGB(255,0,0)

' screencopy 2,0
' sleep
' dim as single scooby,sizey
' sizey=int(8*rnd)+1
' if sizey<3 then sizey=3
'sizey=1
'for xagent=1 to 17':for yagent=1 to 17
'        for yagent=1 to 64
 'for randa=1 to 32 step +1
'    for scooby=-sizey to sizey step +1
dim as single ray
'retry:
If rta>1 Then
ray=int(rnd*16)+1
if ray>16 then ray=16
if ray<8 then ray=8'goto retry
 for randaz=-sizey to sizey step +1

  For randa = 0 To 360 step +1
      if randaz>sizey/2 then
        xagent = Cos(randa) * (randaz-int(rnd*(sizey/2))+1)
        yagent = Sin(randa) * (randaz-int(rnd*(sizey/2))+1)
   else
      xagent = Cos(randa) * (randaz+int(rnd*(sizey/2))+1)
        yagent = Sin(randa) * (randaz+int(rnd*(sizey/2))+1)  
       
       
       end if
   ' elseif randaz>0 then
      '  xagent = Cos(randa) * randaz
       ' yagent = Sin(randa) * randaz'-int(rnd*8)+1
        
        
    '    end if
        
       ' x1=cos(a) * radcountlinein
       ' y1=sin(a) * radcountlinein
       ' screenlock
       if point(sizey-1+xagent,sizey-1+yagent)<>rgb(0,0,0) then
       pixela(astrocnt)=pixela(astrocnt)+1
  astroidsx(astrocnt,pixela(astrocnt))=-(3*((xagent)*100/(sizey2*2)/100))
     ' for ccxtmp=1 to 17                   txtxxyy(ccx*6-6+1+ccx2,ccy*6-6+1+ccy2)=colr'rgb(255,255,255)

     astroidsy(astrocnt,pixela(astrocnt))=-(3*((yagent)*100/(sizey2*2)/100))
      '  circle (ballx + x, bally + y),.75,rgb(0,255,0),,,,f'point(hammerx2+x,hammery2+y)
 '      IF randaz<0 THEN
 ' SHIPZCOUNT=8*((xx)*100/(18/2)/100)
'ELSEIF randaz>0 THEN
'    SHIPZCOUNT=-8*((Xx-(65/2))*100/(65/2)/100)
'END IF
'SHIPZCOUNT=(SHIPPERZ-SHIPZCOUNT)

 astroidsz(astrocnt,pixela(astrocnt))=-(ray/2)+(ray*(randaz*100/100/(720*2+(720/2))))    
    'astroids(pixela(astrocnt)).Z=(randaz*100/1/30/((720*4)))'/(128*8)'1000'0
' If tiding<>5 Then
 	
 		
 	
' EndIf
   astroidsc(astrocnt,pixela(astrocnt))=point(sizey-1+xagent,sizey-1+yagent)
' Else
' 	astroidsc(astrocnt,pixela(astrocnt))=RGB(0,255,0)
end if
'   screenunlock
next
Next
Else
Dim As Single zray,yray,xray
For zray=-sizey To sizey
	For yray=1 To sizey*2
		For xray=1 To sizey*2
		
		  pixela(astrocnt)=pixela(astrocnt)+1
  astroidsx(astrocnt,pixela(astrocnt))=-1+((xray)*100/(sizey*2)/100)
     ' for ccxtmp=1 to 17                   txtxxyy(ccx*6-6+1+ccx2,ccy*6-6+1+ccy2)=colr'rgb(255,255,255)

     astroidsy(astrocnt,pixela(astrocnt))=-1+((yray)*100/(sizey*2)/100)
      '  circle (ballx + x, bally + y),.75,rgb(0,255,0),,,,f'point(hammerx2+x,hammery2+y)
 '      IF randaz<0 THEN
 ' SHIPZCOUNT=8*((xx)*100/(18/2)/100)
'ELSEIF randaz>0 THEN
'    SHIPZCOUNT=-8*((Xx-(65/2))*100/(65/2)/100)
'END IF
'SHIPZCOUNT=(SHIPPERZ-SHIPZCOUNT)

 astroidsz(astrocnt,pixela(astrocnt))=((zray)*100/sizey*2/(720*2+(720/2)))    
    'astroids(pixela(astrocnt)).Z=(randaz*100/1/30/((720*4)))'/(128*8)'1000'0
' If tiding<>5 Then
 	
 		
 	
' EndIf
   astroidsc(astrocnt,pixela(astrocnt))=point(xray,yray)
' Else
' 	astroidsc(astrocnt,pixela(astrocnt))=RGB(0,255,0)

'   screenunlock
next
Next
Next
end if		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
			
'		Next
'	Next
'Next 
'next
screenset 1,0
        flatter(astrocnt)=pieyedi*2*rnd
        astroz(astrocnt)=0.0125'(-1.5*rnd)
        
againy:
tiding=int(rnd*6)+1

       if tiding=0 then goto againy
     '  If rta<2 Then tiding=6 
astrot(astrocnt)=tiding
If rta=1 Then 
	
	
	astroh(astrocnt)=1
againy2:
astrot(astrocnt)=Int(Rnd*4)+1
If topfired>255/2 Then astrot(astrocnt)=4
If bottomfired>255/2 Then astrot(astrocnt)=2
If astrot(astrocnt)<>2 And astrot(astrocnt)<>4 Then GoTo againy2

End if
if astrot(astrocnt)=1 then
       astrox(astrocnt)=int(rnd*1280)+1
       astroy(astrocnt)=int(rnd*720)+1
   elseif astrot(astrocnt)=3 then
       astrox(astrocnt)=int(rnd*1280)+1
       astroy(astrocnt)=int(rnd*720)+1
   elseif astrot(astrocnt)=2 or astrot(astrocnt)=4 then
      
if astrot(astrocnt)=4 then 
    astrox(astrocnt)=(1280+(1280/3)):astroz(astrocnt)=topzzz:astroy(astrocnt)=topyyy-15'int(720*rnd)+1
'(.20*rnd)       
else
     astrox(astrocnt)=(0-(1280/3)):astroz(astrocnt)=bottomzzz:astroy(astrocnt)=bottomyyy+15'int(720*rnd)+1
'(.20*rnd)       
end if   
    'if rnd>.49 and astrot(astrocnt)=4 then 
elseif astrot(astrocnt)>4 Then
     astrox(astrocnt)=int(1280*rnd)+1
       astroy(astrocnt)=int(720*rnd)+1
               astroz(astrocnt)=(1.5)

       end if
       
       
   end if
   if astrot(astrocnt)=4 then astrox(astrocnt)=astrox(astrocnt)-16
      if astrot(astrocnt)=2 then astrox(astrocnt)=astrox(astrocnt)+16
if (astrot(astrocnt)=4 or astrot(astrocnt)=2)  and astroy(astrocnt)>720 then astroy(astrocnt)=astroy(astrocnt)-6
      if (astrot(astrocnt)=4 Or astrot(astrocnt)=2) and astroy(astrocnt)<0 then astroy(astrocnt)=astroy(astrocnt)+6


  '   if (astrot(astrocnt)=1 and topxxx>astrox(astrocnt)) or (astrot(astrocnt)=3 and bottomxxx>astrox(astrocnt)) then 
'       astrox(astrocnt)=astrox(astrocnt)+4
'   elseif (astrot(astrocnt)=1 and topxxx<astrox(astrocnt)) or (astrot(astrocnt)=3 and bottomxxx<astrox(astrocnt)) then
'          astrox(astrocnt)=astrox(astrocnt)-4
'   end if 
'   if (astrot(astrocnt)=1 and topyyy>astroy(astrocnt)) or (astrot(astrocnt)=2 and bottomyyy>astroy(astrocnt)) then 
'       astroy(astrocnt)=astroy(astrocnt)+4
'   elseif (astrot(astrocnt)=1 and topyyy<astroy(astrocnt)) or (astrot(astrocnt)=2 and bottomyyy<astroy(astrocnt)) then
'          astroy(astrocnt)=astroy(astrocnt)-4
'   end if 
      ' astrocnt=97 game2=1 or game2
    dim as single topzzz2=int(topzzz*5*12),bottomzzz2=int(bottomzzz*5*12)
   ' if astrocnt=97 and topzzz<astroz(astrocnt) then astroz(astrocnt)=astroz(astrocnt)-.0125/(3)
    'if astrocnt=98 and bottomzzz>astroz(astrocnt) then astroz(astrocnt)=astroz(astrocnt)+.0125/(3)
'    if astrocnt=97 and bottomzzz<astroz(astrocnt) then astroz(astrocnt)=astroz(astrocnt)-.0125/(3)

    
    
    for pointidx=1 to pixela(astrocnt) step +int(rnd*16)+1
        'if astroids(pointidx).c<>rgb(0,0,0) then
       ' IF astroids(pointIdx).Z+zoomlevel>0 then 
      '  if rnd<.50 then
    if flatter(astrocnt)>pieyedi then
flatx=astroidsx(astrocnt,pointidx)*cos((flatter(astrocnt)+flat1))*(32*(astroz(astrocnt)*5))/(astroidsz(astrocnt,pointidx)+zoomlevel)
flaty=astroidsy(astrocnt,pointidx)*sin((flatter(astrocnt)+flat1))*(32*(astroz(astrocnt)*5))/(astroidsz(astrocnt,pointidx)+zoomlevel)
flatz=astroidsz(astrocnt,pointidx)*sin((flatter(astrocnt)+flat1))*(32*(astroz(astrocnt)*5))/((720*2+(720/2)*8)+zoomlevel)
      '  flatter(astrocnt)=pieyedi*2*rnd
      
else
flatx=astroidsx(astrocnt,pointidx)*cos((flatter(astrocnt)+flat2))*(32*(astroz(astrocnt)*5))/(astroidsz(astrocnt,pointidx)+zoomlevel)
flaty=astroidsy(astrocnt,pointidx)*sin((flatter(astrocnt)+flat2))*(32*(astroz(astrocnt)*5))/(astroidsz(astrocnt,pointidx)+zoomlevel)
flatz=astroidsz(astrocnt,pointidx)*sin((flatter(astrocnt)+flat2))*(32*(astroz(astrocnt)*5))/((720*2+(720/2)*8)+zoomlevel)
     
      end if  
    
    
    
      '  flatter(astrocnt)=pieyedi*2*rnd
'else
'    flatx=astroids(pointidx).x*cos((flatter(astrocnt)+flat2))*(96*(astroz(astrocnt)*5))/(astroids(pointidx).Z+zoomlevel)
'flaty=astroids(pointidx).y*sin((flatter(astrocnt)+flat2))*(96*(astroz(astrocnt)*5))/(astroids(pointidx).Z+zoomlevel)
'flatz=astroids(pointidx).Z*sin((flatter(astrocnt)+flat2))*(96)*(720*2+(720/2))*(astroz(astrocnt)*5)
'end if
if astrocnt<0 then
 newx = flatx'newx'
newy = (sin((flatter(astrocnt)+flat1)) * flatz) + (cos((flatter(astrocnt)+flat1)) * flaty)
newz = (cos((flatter(astrocnt)+flat1)) * flatz) - (sin((flatter(astrocnt)+flat1)) * flaty)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos((flatter(astrocnt)+flat1)) * flatx) - (sin((flatter(astrocnt)+flat1)) * flatz)
newy=flaty''y = y'
newz = (sin((flatter(astrocnt)+flat1)) * flatx) + (cos((flatter(astrocnt)+flat1)) * flatz)
 flatx=newx
 flaty=newy
 flatz=newz
 newx = (cos(flatter(astrocnt)+(flat1)) * flatx) + (sin((flatter(astrocnt)+flat1)) * flaty)
newy = (cos((flatter(astrocnt)+flat1)) * flaty) - (sin((flatter(astrocnt)+flat1)) * flatx)
newz = flatz
'end if
 flatx=newx
 flaty=newy
 flatz=newz
'end if
   end if
    
    newx=flatx
        newy=flaty

'end if    
rem dream come true
 'newx=flatx
 'newy=flaty
' if newx>bottomxxx-48 and newx<bottomxxx+48 and newy>bottomyyy and newy<bottomyyy+48 and bottomzzz2>=int(astroz(astrocnt)*5*12)-6 and bottomzzz2<=int(astroz(astrocnt)*5*12)+6 then
       'color ,rgb(255,0,0)
'goto dobo '      topgn3=0
If timer>judgedbottom and newx+astrox(astrocnt)>bottomxxx-(30*(BOTTOMZZZ*5)) and newx+astrox(astrocnt)<bottomxxx+(30*(BOTTOMZZZ*5)) and newy+astroy(astrocnt)>bottomyyy and newy+astroy(astrocnt)<bottomyyy+((45)*(BOTTOMZZZ*5)) and bottomzzz2>=int(astroz(astrocnt)*5*12)-8 and bottomzzz2<=int(astroz(astrocnt)*5*12)+8 then
     'RANDOMIZE RND+TIMER+Rnd
       
       ' FSOUND_Stream_Close(stream6)
         '      FSOUND_Stream_Close(stream3)

        ' stream=FSOUND_Stream_Open(SOUND_FILE6, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
       ' FSOUND_Stream_Stop(stream)
        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream6)
    '   bottomxxx=1280/2
     '  bottomyyy=720-(720/4)
       If astroh(astrocnt)=1 Then
       	energybottom=100
       	livesbottom=livesbottom+5
       	bottomfired=0
      ' End If 	
       	
       EndIf
       ' screenlock
        'cls
        If astroh(astrocnt)<>1 Then
         IF RND>.49 THEN 
                   paint(1,1),rgb(255,0,0)
                   screencopy:flip',rgb(255,5,5)
              ' else
              end if
              ' IF RND>2 THEN 
              '     paint(1,1),rgb(255,0,0)',rgb(255,5,5)
              ' else
                  for hondal=1 to (25)*(25/2)
                  	''RANDOMIZE RND+TIMER+Rnd
                       hondax=rnd*128
                       honday=rnd*128
                       screenlock
                       circle(bottomxxx+64-hondax,bottomyyy+32+64-honday),(rnd*256)*(bottomzzz*5),rgb(255,(255*rnd),(127*rnd)),,,,F
                               screenunlock
judgedbottom=timer+6
                   screencopy
                   flip
                   sleep 0,1
                   next hondal
              '     end if',rgb(
                ' paint(1,1),rgb(255,0,0)',rgb(255,5,5)

  ' circle(bottomxxx,bottomyyy+32),64,rgb(255,192,0),,,,F
       ' screenunlock
     '   screencopy
        'color ,rgb(0,0,0)
       'LINE(BOTTOMXXX-32,BOTTOMYYY+32)-(BOTTOMXXX+32,BOTTOMYYY),rgb(255,7,7),BF',rgb(255,3,3)
                cup9top=cup9top+50
                 energybottom=0'energybottom-5.0
                if energybottom<1 then 
                    energybottom=100
                    livesbottom=livesbottom-1
                    END IF
                    if livesbottom<1 then livesbottom=0
       '             ; if rnd<5 then
      '    ;  astroz(astrocnt)=(topzzz*5)*(bottomzzz*5)
      
      ' astrocnt=99
        End If
astroz(astrocnt)=5
ASTROT(ASTROCNT)=0
astroh(astrocnt)=0
bottomfired=0
' ;else
      ' ;     astroz(astrocnt)=(bottomzzz*5)*(topzzz*5)
      ' ;     astrocnt=98
      ' ;     end if
     '  astrox(astrocnt)=int(rnd*(1280/2))+1
     '  astroy(astrocnt)=int(rnd*(720/2))+1
        '   flatter(astrocnt)=pieyedi*2*rnd
                  '  astrocnt=99
               ' for rannycount=1 to 12
               '         sound pulsewave(notes(rannycount)),1/16
               '         next rannycount
'topgun(tempgunx,tempguny,1)=0
'topgn3=topgun3(tempgunx,tempguny)
'topgun3(tempgunx,tempguny)=0
'topgunx(calca)=0
'topguny(calca)=0
end if
'draw string (1,8),str(bottomzzz2),rgb(255,255,255)
'draw string (1,16),str(bottomyyy),rgb(255,255,255)
'rem discotech
'dim as integer bottomzzz2,topzzz2
'if bottomgun(tempgunx,tempguny,1)>0 and tempguny<topyyy then sleep




'if singletopz<0 and topzzz>.25/2 then topzzz=topzzz-.0125:singletopz=singletopz+1:end if 'else singletopz=singletopz+.5:end if
'if singletopz>0 and topzzz<.25 then topzzz=topzzz+.0125:singletopz=singletopz-1:end if  'else singletopz=singletopz-.5:end if
'if singlebottomz<0 and bottomzzz>.25/2 then bottomzzz=bottomzzz-.0125:singlebottomz=singlebottomz+1:end if 'else singlebottomz=singlebottomz+.5:end if
'if singlebottomz>0 and bottomzzz<.25 then bottomzzz=bottomzzz+.0125:singlebottomz=singlebottomz-1:end if 'else singlebottomz=singlebottomz-.5:end if



 if timer>judgedtop and newy+astrox(astrocnt)>topxxx-(30*(TOPZZZ*5)) and newy+astrox(astrocnt)<topxxx+(30*(TOPZZZ*5)) and newy+astroy(astrocnt)<topyyy and newy+astroy(astrocnt)>topyyy-((45)*(TOPZZZ*5)) and topzzz2>=int(astroz(astrocnt)*5*12)-8 and topzzz2<=int(astroz(astrocnt)*5*12)+8 then
      '  bottomgn3=0
        
        'RANDOMIZE RND+TIMER+Rnd
   '  if newx+astrox(astrocnt)>topxxx-48 and newx+astrox(astrocnt)<topxxx+48 and newy+astroy(astrocnt)<topyyy and newy+astroy(astrocnt)>topyyy-48 and topzzz2>=int(astroz(astrocnt)*5*12)-6 and topzzz2<=int(astroz(astrocnt)*5*12)+6 then
    
        
       '  FSOUND_Stream_Close(stream6)
         '      FSOUND_Stream_Close(stream3)

       '  stream=FSOUND_Stream_Open(SOUND_FILE6, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
       ' FSOUND_Stream_Stop(stream)
        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream6)
        'screenlock
       ' cls
      ' topxxx=1280/2
       'topyyy=720/4
        If astroh(astrocnt)=1 Then
       	energytop=100
      ' End If 	
       	livestop=livestop+5
   topfired=0
       EndIf
       ' screenlock
        'cls
        If astroh(astrocnt)<>1 Then
               'IF RND>2 THEN 
               '    paint(1,1),rgb(255,0,0)',rgb(255,5,5)
               'else
                IF RND>.49 THEN 
                   paint(1,1),rgb(255,0,0)
                   screencopy:flip',rgb(255,5,5)
              ' else
              end if
                 '  dim as integer hondax.honday,hondar,hondal
                  for hondal=1 to (25)*(25/2)
                  '	'RANDOMIZE RND+TIMER+Rnd
                     hondax=rnd*128
                       honday=rnd*128
                       screenlock
                       circle(topxxx+64-hondax,topyyy-32+64-honday),rnd*256*(topzzz*5),rgb(255,(255*rnd),(127*rnd)),,,,F
                               screenunlock
judgedtop=timer+6
                   screencopy
                   flip
                   sleep 0,1
                   next hondal
                '   end if',rgb(
 ' paint(1,1),rgb(255,0,0)
 ' circle(topxxx,topyyy-32),64,rgb(255,0,0),,,,F
       ' screenunlock
      '  dim as integer astrayx as integer
 '       screencopy
 '       sleep 500,1
    '    screencopy
        'LINE(TOPXXX-32,TOPYYY-32)-(TOPXXX+32,TOPYYY),rgb(255,7,7),BF',rgb(255,3,3)
                cup9bottom=cup9bottom+50
                energytop=0
                if energytop<1 then 
                    energytop=100
                    livestop=livestop-1
                    END IF
                    if livestop<1 then livestop=0
    '    astrocnt=99            
                ' if rnd<5 then
        End If
astroz(astrocnt)=5
ASTROT(ASTROCNT)=0
astroh(astrocnt)=0
topfired=0
    '   astrocnt=97
    '   else
    '        astroz(astrocnt)=(bottomzzz*5)*(topzzz*5)
       '     astrocnt=99
     '       end if
       ' astrox(astrocnt)=int(rnd*(1280/2))+1
       ' astroy(astrocnt)=int(rnd*(720/2))+1
       '    flatter(astrocnt)=pieyedi*2*rnd
             ' cup9top=cup9top+1
              '  for rannycount=12 to 1 step -1' to 12
              '          sound pulsewave(notes(rannycount)),1/16'1000/360
'next rannycount
'bottomgun(tempgunx2,tempguny2,1)=0
'bottomgn3=bottomgun3(tempgunx,tempguny)
'bottomgun3(tempgunx2,tempguny2)=0
'bottomgunx(calca)=0
'bottomguny(calca)=0
end If

dobo:
 
'end if
if newx+astrox(astrocnt)>0 and newx+astrox(astrocnt)<1280 and newy+astroy(astrocnt)>0 and newy+astroy(astrocnt)<720 then

'if +astrox(astrocnt)>0 and astrox(astrocnt)<1280 and astroy(astrocnt)>0 and astroy(astrocnt)<720 then
 screenlock
   circle (newX+astrox(astrocnt), newy+astroy(astrocnt)),5.5*(astroz(astrocnt)*5),astroidsc(astrocnt,pointidx),,,,f

screenunlock
end if
'if pointidx=int(pixela/2) then
'    astx=newx
'    asty=newy
'    astz=int(astroz(astrocnt)*5)'newz/(astroz(astrocnt)*5)/(64)
 '   end if
'end if
'end if
if pointidx=int(pixela(astrocnt)/2) then
    astx(astrocnt)=newx
    asty(astrocnt)=newy
    astz(astrocnt)=int(astroz(astrocnt)*5*12)'newz/(astroz(astrocnt)*5)/(64)
   end if
'astroz(astrocnt)=astroz(astrocnt)+.0125/(7)
next
if (astrot(astrocnt)<>2 or astrot(astrocnt)<>4) and astrot(astrocnt)<5 then astroz(astrocnt)=astroz(astrocnt)+(((1/3)/(60)-(((1/3/(30))*(-.5*(ASTROZ(ASTROCNT)*5))))))
'if astrot(astrocnt)>4 then astroz(astrocnt)=astroz(astrocnt)-((1/3)/(16)*(.5*(ASTROZ(ASTROCNT)*5)))
if astrot(astrocnt)>4 then astroz(astrocnt)=astroz(astrocnt)-((((1/3)/(60)+(((1/3/(15))*(.525*(ASTROZ(ASTROCNT)*5)))))))
if astrot(astrocnt)=4 then  astroz(astrocnt)=1.5*((topzzz*5)*(bottomzzz*5)/5)
if astrot(astrocnt)=2 then  astroz(astrocnt)=1.5*((bottomzzz*5)*(topzzz*5)/5)



end if

'end if
'next
   ' astroz(astrocnt)=astroz(astrocnt)+.0125/3
end If
'End if
 next
 end if
' NEXT
 
 
 
 
 
 
 
 
 
 
 if game2=13 and timer>tmr then 
     tmr=timer+30
     if game14=0 then
         game14=1
         pressed=0
     else
         game14=0
         pressed=0
     end if
 end if
''mutexlock mutex
'  game=11
'''mutexunlock mutex
'game2=11
dim AS integer scores
 DIM AS STRING SCOREY
 if game2=2 or (game2=13 and game14=0) then
     if game2<>13 then
             '          drawdraw(0,720/2-(10*8*2.5/2+64),"STARSHIP SHOOTERS RANK, NAME, AND SCORE",rgb(0,255,255))

              drawdraw(1280/2-(39*8*3/2),720/2-(10*8*2.5/2+64),"STARSHIP SHOOTERS RANK, NAME, AND SCORE",rgb(0,255,255),3)
        line(1280/2-(39*8*3/2),720/2-(10*8*2.5/2+64-32))-(1280/2+(39*8*3/2),720/2-(10*8*2.5/2+64-32)),rgb(0,255,255)
'                line(0,720/2-(10*8*2.5/2+64-32))-(1280/2+(39*8*3/2),720/2-(10*8*2.5/2+64-32)),rgb(0,255,255)

    for scores=10 to 1 step -1
         SCOREY=str(scores)+".  "+starname(scores)+"   "+str(starscore(scores))

        ' draw string(1280/2-,(720-(12*8*2))+(scores*2*8)),str(scores),rgb(255,15,7)
       '  drawdraw(0,(720/2)-(10*8*2.5/2)+(scores*8*2.5),SCOREY,rgb(0,255,251))
                  drawdraw((1280/2)-(len(SCOREY)*8*3/2),(720/2)-(10*8*2.5/2)+(scores*12*2.5),SCOREY,rgb(0,255,251),3)

     next scores
 else
  '  drawdraw(0,720/2-(10*8*2.5/2+64),"STARSHIP SHOOTERS RANK, NAME, AND SCORE",rgb(0,255,255))

              drawdraw(1280/2-(39*8*3/2),720/2-(10*8*2.5/2+64),"STARSHIP SHOOTERS RANK, NAME, AND SCORE",rgb(0,255,255),3)
        line(1280/2-(39*8*3/2),720/2-(10*8*2.5/2+64-32))-(1280/2+(39*8*3/2),720/2-(10*8*2.5/2+64-32)),rgb(0,255,255)
       '         line(0,720/2-(10*8*2.5/2+64-32))-(1280/2+(39*8*3/2),720/2-(10*8*2.5/2+64-32)),rgb(0,255,255)

      for scores=10 to 1 step -1
         SCOREY=str(scores)+".  "+starname(scores)+"   "+str(starscore(scores))
        ' draw string(1280/2-,(720-(12*8*2))+(scores*2*8)),str(scores),rgb(255,15,7)
        ' drawdraw(0,(720/2)-(10*8*2.5/2)+(scores*8*2.5),SCOREY,rgb(0,255,255))
                  drawdraw((1280/2)-(len(SCOREY)*8*3/2),(720/2)-(10*8*2.5/2)+(scores*12*2.5),SCOREY,rgb(0,255,255),3)

     next scores
     end if
 elseif game2=4 or game2=6 or game2=11 or (game2=13 and game14=1) then
     if game2=13 then
 '     drawdraw(0,720/2-(10*8*2.5/2+64),"PING PONG TENNIS RANK, NAME, AND SCORE",rgb(0,255,0))
  '      line(0,720/2-(10*8*2.5/2+64-32))-(1280/2+(38*8*3/2),720/2-(10*8*2.5/2+64-32)),rgb(0,255,0)      
 drawdraw(1280/2-(38*8*3/2),720/2-(10*8*2.5/2+64),"PING PONG TENNIS RANK, NAME, AND SCORE",rgb(0,255,0),3)
        line(1280/2-(38*8*3/2),720/2-(10*8*2.5/2+64-32))-(1280/2+(38*8*3/2),720/2-(10*8*2.5/2+64-32)),rgb(0,255,0)    
     for scores=10 to 1 step -1
         SCOREY=str(scores)+".  "+pongname(scores)+"   "+str(pongscore(scores))

         drawdraw((1280/2)-(len(SCOREY)*8*3/2),(720/2)-(10*8*2.5/2)+(scores*12*2.5),SCOREY,rgb(0,255,0),3)

        ' draw string(1280/2-,(720-(12*8*2))+(scores*2*8)),str(scores),rgb(255,15,7)
        ' drawdraw(0,(720/2)-(10*8*2.5/2)+(scores*8*2.5),SCOREY,rgb(0,255,0))
     next scores
 else
    '  drawdraw(0,720/2-(10*8*2.5/2+64),"PING PONG TENNIS RANK, NAME, AND SCORE",rgb(0,1,0))
     '   line(0,720/2-(10*8*2.5/2+64-32))-(1280/2+(38*8*3/2),720/2-(10*8*2.5/2+64-32)),rgb(0,1,0)      
 drawdraw(1280/2-(38*8*3/2),720/2-(10*8*2.5/2+64),"PING PONG TENNIS RANK, NAME, AND SCORE",rgb(255,255,255),3)
        line(1280/2-(38*8*3/2),720/2-(10*8*2.5/2+64-32))-(1280/2+(38*8*3/2),720/2-(10*8*2.5/2+64-32)),rgb(255,255,255)      

    for scores=10 to 1 step -1
         SCOREY=str(scores)+".  "+pongname(scores)+"   "+str(pongscore(scores))

         drawdraw((1280/2)-(len(SCOREY)*8*3/2),(720/2)-(10*8*2.5/2)+(scores*12*2.5),SCOREY,rgb(255,255,255),3)

        ' draw string(1280/2-,(720-(12*8*2))+(scores*2*8)),str(scores),rgb(255,15,7)
        ' drawdraw(0,(720/2)-(10*8*2.5/2)+(scores*8*2.5),SCOREY,rgb(0,1,0))
     next scores
     
     
     
     end if
 end if

 if game2=2 or game2=4 or game2=6 or game2=11 or game2=13 then
 '    pressaflg=0

     screenset 2,0
     '     color rgb(255,255,255),rgb(0,0,0)

    ' cls
     dim as SINGLE pressax,pressay,PRESSAZ,pressaz2
     if pressed=0 then
         pressed=1
    ' elseif pressed=0 then
    '     pressed=4
    ' end if
    ' if pressed>0 and pressed<100 then
     
    ' paint(1,1),rgb(0,0,0)
    ' SCREENLOCK
     if game2<>13 then
         cls
color RGB(255,127,63),rgb(0,0,0)
     SCREENLOCK
     'draw string (0,0),"TO CONTINUE PRESS A BUTTON",RGB(255,127,63)
print:print "TO CONTINUE PRESS A BUTTON"

     SCREENUNLOCK
 elseif game2=13 then
     '     screenset 2,0
     cls
color RGB(255,127,63),rgb(0,0,0)
    ' cls:color rgb(255,255,255),rgb(0,0,0)
     SCREENLOCK
'     draw string (0,0),"TO EXIT SCREEN PRESS L KEY",RGB(255,127,63)
print:print "HIT L OR A ON GAME CONTROL"

  SCREENUNLOCK
 end if
' DIM TOXIC AS INTEGER
' IF GAME2<>13 THEN
'     TOXIC=26
' ELSEIF GAME2=13 THEN
'     TOXIC=27
'     END IF
     for ccx=-((26*8/2)) to ((26*8/2))'64
      '   if ccx=0 then ccx=ccx+1
    for ccy=-4 to 4
      '  if ccy=0 then ccy=ccy+1
        if (ccx<>0 or ccy<>0) and point(ccx+(26*8/2),ccy+4+8)=rgb(255,127,63) then
          ' for ccx2=1 to 6
          '     for ccy2=1 to 6
          if game2=2 or (game2=13 and game14=0) then
          if 255*((ccy+5)*100/10/100)<128 then display2(ccx,ccy,rgb((63+15)+((127-(63+15))*((ccy+5)*100/8/100)),(63+15)+((127-(63+15))*((ccy+5)*100/8/100)),(63+15)+((127-(63+15))*((ccy+5)*100/8/100))),0)
                    if 255*((ccy+5)*100/10/100)>127 then display2(ccx,ccy,rgb((127)+((127)*((ccy+5)*100/8/100)),(127)+((127)*((ccy+5)*100/8/100)),(127)+((127)*((ccy+5)*100/8/100))),0)
end if 
if game2=4 or game2=6 or game2=11 or (game2=13 and game14=1) then
          if 255*((ccy+5)*100/10/100)<128 then display2(ccx,ccy,rgb((63+15)+((127-(63+15))*((ccy+5)*100/8/100)),(63+15)+((127-(63+15))*((ccy+5)*100/8/100)),127),0)
                    if 255*((ccy+5)*100/10/100)>127 then display2(ccx,ccy,rgb((127)+((127)*((ccy+5)*100/8/100)),(127)+((127)*((ccy+5)*100/8/100)),127),0)
end if 

                   ' txtyy()=rgb(255,255,255)
            '   next ccy2
            'next ccx2
        elseif point(ccx+(26*8/2),ccy+5)=rgb(0,0,0) then
          'for ccx2=1 to 6
          '     for ccy2=1 to 6
                   display2(ccx,ccy,rgb(0,0,0),0)
                   ' txtyy()=rgb(255,255,255)
      '   end if   
           ' next ccy2
           ' next ccx2  
        end if
     '   end if
    next ccy
next ccx
end if
       SCREENSET 1,0

'if pressaflg=0 then
'TXTXXYY2(27*8*6/2,5*6)=rgb(0,0,0)
     dim AS integer ccz=1,stp=2,stp2=2' as integer
       for ccz=24 TO 1 STEP -3
       for ccx=-((26*8*6/2)) to ((26*8*6/2)) step +stp
         '   if ccx=0 then ccx=ccx+1
   ' for ccy=-5 to 5
          ' stp=stp+1
          ' if stp=3 then stp=1
        ' if ccx=(13*8*6)-1 then STP=1
           for ccy=-(4*6) to (2*6) step +stp2
          '  stp2=stp2+1
          ' if stp2=3 then stp2=1   
         ' if ccy=3*6-1 then STP2=1
          '            if ccy=0 then ccy=ccy+1
 if ccx=0 or ccy=0 then goto nxtime
 '   if ccy>8*6-1 then txtxxyy(ccx,ccy)=rgb(255,255,0)
 '   if ccx>136*6-1 then txtxxyy(ccx,ccy)=rgb(255,255,0)
rem if ccy=1 or ccy=8*6 or ccx=1 or ccx=136*6 then txtxxyy(ccx,ccy)=rgb(0,0,0)
 'if game2<1 then
 dim as SINGLE xccx,yccy,flatzz=ccz,newz
     flatx=ccx'(1280)/2-((19*8*6)/2)+ccx
     flaty=ccy'(720*4)/2-(8*6/2)+ccy
     newX = flatx'newx'
newy = (sin(flat33) * flatzz) + (cos(flat33) * flaty)
newz = (cos(flat33) * flatzz) - (sin(flat33) * flaty)
 flatx=newx
 flaty=newy
 flatzz=newz
 newx = (cos((pieyedi*2)+flat33) * flatx) - (sin((pieyedi*2)+flat33) * flatzz)
newy=flaty''y = y'
newz = (sin((pieyedi*2)+flat33) * flatx) + (cos((pieyedi*2)+flat33) * flatzz)
 flatx=newx
 flaty=newy
 flatzz=newz
 
 newx = (cos(-(pieyedi*2)-flat33) * flatx) + (sin(-(pieyedi*2)-flat33) * flaty)
newy = (cos(-(pieyedi*2)-flat33) * flaty) - (sin(-(pieyedi*2)-flat33) * flatx)
newz = flatzz'
 flatx=newx
 flaty=newy
 flatzz=newz
 screenlock

 '    pset pic,(ccx,ccy),txtxxyy(ccx,ccy)
     IF TXTXXYY2(CCX,CCY)<>RGB(0,0,0) THEN CIRCLE(newx+(1280/2),newy+(720/2)),1,TXTXXYY2(CCX,CCY),,,,F'pset ((1280)/2-((19*8*6)/2)+ccx,(720)/2-(8*6/2)+ccy),RGB(255,0,0)'txtxxyy2(ccx,ccy)
 'elseif game2>0 then
 '       pset((1280)/2-((21*8*6)/2)+ccx,ccy),txtxxyy(ccx,ccy)
 ' end if
  screenunlock
  nxtime:

'end if

 'if ccy=4*6 then stp2=2
next ccy
'if ccx=(26*8*6)/2 then stp=2
next ccx

next ccz
END IF
IF GAME2=13 AND HELPFLAG=101 THEN
 GetJoystick(0,buttons,xxx,yyy)
    GetJoystick(1,buttons2,xxx2,yyy2)
IF (BUTTONS=1 OR BUTTONS2=1) AND HELPFLAG=101 THEN 
    'mutexlock mutex
    GAME=GAME13
    ''mutexunlock mutex
    GAME2=GAME13
    
    HELPFLAG=1
       END IF

     END IF
    ' END IF
     'if game2<>3 and game2<>4 and game2<>5 and game2<>6 and game2<>10 and game2<>11 then fsound_stream_stop(stream)
flat33=flat33-(pieyedi*2/360)
if flat33<-(pieyedi*2)+(pieyedi*2/360) then flat33=0
If vol255=255 and (FSOUND_Stream_GetPosition(stream) <= FSOUND_Stream_GetLength(stream)) and (game2=31 or game2=312 ) Then
   '             Exit While
   '         End If
   '         Sleep 50, 1
   '     Wend
 if  FSOUND_Stream_GetPosition(stream) <= FSOUND_Stream_GetLength(stream) then     FSOUND_Stream_stop(stream)
 if  FSOUND_Stream_GetPosition(stream2) <= FSOUND_Stream_GetLength(stream2) then        FSOUND_Stream_stop(stream2)
 if  FSOUND_Stream_GetPosition(stream3) <= FSOUND_Stream_GetLength(stream3) then         FSOUND_Stream_stop(stream3)
 if  FSOUND_Stream_GetPosition(stream4) <= FSOUND_Stream_GetLength(stream4) then          FSOUND_Stream_stop(stream4)
 if  FSOUND_Stream_GetPosition(stream5) <= FSOUND_Stream_GetLength(stream5) then           FSOUND_Stream_stop(stream5)
 if  FSOUND_Stream_GetPosition(stream6) <= FSOUND_Stream_GetLength(stream6) then            FSOUND_Stream_stop(stream6)
 if  FSOUND_Stream_GetPosition(stream7) <= FSOUND_Stream_GetLength(stream7) then             FSOUND_Stream_stop(stream7)
  if  FSOUND_Stream_GetPosition(stream8) <= FSOUND_Stream_GetLength(stream8) then             FSOUND_Stream_stop(stream8)
if  FSOUND_Stream_GetPosition(stream9) <= FSOUND_Stream_GetLength(stream9) then                FSOUND_Stream_stop(stream9)
if  FSOUND_Stream_GetPosition(stream10) <= FSOUND_Stream_GetLength(stream10) then                 FSOUND_Stream_stop(stream10)
if  FSOUND_Stream_GetPosition(stream11) <= FSOUND_Stream_GetLength(stream11) then                  FSOUND_Stream_stop(stream11)
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
        
    end if
     if (game2=1 or game2=12) and vol255=255 and muting=0 and fmusic_isplaying(starsong1)=0 and fmusic_isplaying(starsong2)=0 and fmusic_isplaying(starsong3)=0 and fmusic_isplaying(starsong4)=0 and fmusic_isplaying(starsong5)=0 and fmusic_isplaying(starsong6)=0 and fmusic_isplaying(starsong7)=0 and fmusic_isplaying(starsong8)=0 then
    FMUSIC_StopAllSongs
   ' FMUSIC_FreeSong(starsong)
RANDIT=RANDIT-1
IF RANDIT<1 THEN RANDIT=8
   ' FMUSIC_FreeSong(pongsong)
'randomize INT(RND)+1+timer
'dim randitsum2 as SINGLE
' sum1:
' randitsum2=rnd
' randit=int(rnd*6)+1
 'randit=int(randit/8)
' if randit=0 or randit>6 then goto sum1
'if randit2=0 then randit2=1
'if randit2>3 then randit2=1
'  RANDIT2=int(rnd*3)+1
'if randit=0 then randit=int(rnd*3)+1
'if randit=0 then randit=1
if randit=8 then 
'if starsong1=0 then starSong1 = FMUSIC_LoadSong("\starshipshooterssong\12th.mod")
'if starsong1=0 then end
if vol255=255 then fmusic_playsong(starsong1)
FMUSIC_SetLooping(STARSONG1, 0)

'randit2=randit2+1
elseif randit=7 then
'if starsong2=0 then starSong2 = FMUSIC_LoadSong("\starshipshooterssong\dark.mod")
'if starsong2=0 then end

if vol255=255 then fmusic_playsong(starsong2)
FMUSIC_SetLooping(STARSONG2, 0)

'randit2=randit2+1
elseif randit=6 then 

'if starsong1=0 then starSong1 = FMUSIC_LoadSong("\starshipshooterssong\12th.mod")
'if starsong1=0 then end
if vol255=255 then fmusic_playsong(starsong3)
    FMUSIC_SetLooping(STARSONG3, 0)

'randit2=randit2+1
elseif randit=5 then
'if starsong2=0 then starSong2 = FMUSIC_LoadSong("\starshipshooterssong\dark.mod")
'if starsong2=0 then end

if vol255=255 then fmusic_playsong(starsong4)
FMUSIC_SetLooping(STARSONG4, 0)

'randit2=randit2+1
elseif randit=4 then
'if starsong3=0 then   starSong3 = FMUSIC_LoadSong("\starshipshooterssong\hea&hell.mod")

'if starsong3=0 then end
if vol255=255 then fmusic_playsong(starsong5)
FMUSIC_SetLooping(STARSONG5, 0)

'randit2=randit2+1
elseif randit=3 then 
'if starsong1=0 then starSong1 = FMUSIC_LoadSong("\starshipshooterssong\12th.mod")
'if starsong1=0 then end

if vol255=255 then fmusic_playsong(starsong6)
FMUSIC_SetLooping(STARSONG6, 0)

'randit2=randit2+1
elseif randit=2 then
'if starsong2=0 then starSong2 = FMUSIC_LoadSong("\starshipshooterssong\dark.mod")
'if starsong2=0 then end

if vol255=255 then fmusic_playsong(starsong7)
FMUSIC_SetLooping(STARSONG7, 0)

'randit2=randit2+1
elseif randit=1 then
'if starsong3=0 then   starSong3 = FMUSIC_LoadSong("\starshipshooterssong\hea&hell.mod")

'if starsong3=0 then end
if vol255=255 then fmusic_playsong(starsong8)
FMUSIC_SetLooping(STARSONG8, 0)

end if

'FMUSIC_SetLooping(pongSong, 1)
'CHDIR(CDIR) 
end if
if (game2=3 or game2=5 or game2=10) and vol255=255 and muting=0 and fmusic_isplaying(pongsong1)=0 and fmusic_isplaying(pongsong2)=0 and fmusic_isplaying(pongsong3)=0 and fmusic_isplaying(pongsong4)=0 and fmusic_isplaying(pongsong5)=0 and fmusic_isplaying(pongsong6)=0 and fmusic_isplaying(pongsong7)=0 and fmusic_isplaying(pongsong8)=0 then 
    FMUSIC_StopAllSongs
   ' FMUSIC_FreeSong(starsong)
'randomize INT(RND)+1+timer
    'FMUSIC_FreeSong(pongsong)
 '   RANDIT=int(rnd*3)+1
 'dim randitsum as SINGLE
' sum2:
 'randitsum=rnd
' randit2=int(rnd*6)+1
' randit2=int(randit2/8)
' if randit2=0 or randit2>6 then goto sum2
 RANDIT2=RANDIT2-1
 IF RANDIT2<1 THEN RANDIT2=8
'if randit=0 then randit=int(rnd*3)+1
'if randit=0 then randit=1
'if randit>6 then randit=6
if randit2=8  then 

'if pongsong1=0 then pongSong1 = FMUSIC_LoadSong("\pingpongtennissong\bachmix.mod")
'if pongsong1=0 then end
if vol255=255 then fmusic_playsong(pongsong8)
    FMUSIC_SetLooping(PONGSONG8, 0)

'randit=randit+1
elseif randit2=7 then

'if pongsong2=0 then pongSong2 = FMUSIC_LoadSong("\pingpongtennissong\jarre.mod")
'if pongsong2=0 then end
if vol255=255 then fmusic_playsong(pongsong7)
        FMUSIC_SetLooping(PONGSONG7, 0)

elseif randit2=6  then 
'if pongsong1=0 then pongSong1 = FMUSIC_LoadSong("\pingpongtennissong\bachmix.mod")
'if pongsong1=0 then end

if vol255=255 then fmusic_playsong(pongsong1)
    FMUSIC_SetLooping(PONGSONG1, 0)

'randit=randit+1
elseif randit2=5 then
'if pongsong2=0 then pongSong2 = FMUSIC_LoadSong("\pingpongtennissong\jarre.mod")
'if pongsong2=0 then end

if vol255=255 then fmusic_playsong(pongsong2)
    FMUSIC_SetLooping(PONGSONG2, 0)

'randit=randit+1
elseif randit2=4 then
'if pongsong3=0 then   pongSong3 = FMUSIC_LoadSong("\pingpongtennissong\mourning.mod")
'if pongsong3=0 then end

if vol255=255 then fmusic_playsong(pongsong3)
    FMUSIC_SetLooping(PONGSONG3, 0)

'randit=randit+1
elseif randit2=3  then 
'if pongsong1=0 then pongSong1 = FMUSIC_LoadSong("\pingpongtennissong\bachmix.mod")
'if pongsong1=0 then end

if vol255=255 then fmusic_playsong(pongsong4)
    FMUSIC_SetLooping(PONGSONG4, 0)

'randit=randit+1
elseif randit2=2 then
'if pongsong2=0 then pongSong2 = FMUSIC_LoadSong("\pingpongtennissong\jarre.mod")
'if pongsong2=0 then end

if vol255=255 then fmusic_playsong(pongsong5)
    FMUSIC_SetLooping(PONGSONG5, 0)

'randit=randit+1
elseif randit2=1 then
'if pongsong3=0 then   pongSong3 = FMUSIC_LoadSong("\pingpongtennissong\mourning.mod")
'if pongsong3=0 then end

if vol255=255 then fmusic_playsong(pongsong6)
    FMUSIC_SetLooping(PONGSONG6, 0)

'randit=randit+1
end if
'FMUSIC_SetLooping(pongSong, 1)
'CHDIR(CDIR) 
end if
 if game2=3 or game2=5 or game2=10 or game2=0 then
    
If (FSOUND_Stream_GetPosition(stream) >= FSOUND_Stream_GetLength(stream)) Then
   '             Exit While
   '         End If
   '         Sleep 50, 1
   '     Wend
        FSOUND_Stream_stop(stream)
         '      FSOUND_Stream_Close(stream3)

        ' stream=FSOUND_Stream_Open(SOUND_FILE, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
       ' FSOUND_Stream_Stop(stream)
        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream)       'sleep
       end if
       end if
         if game2=4 or game2=6 or game2=11 then
             if savecapture=0 then
                 savecapture=1
                 kill("scores.dat"):sleep (1000)
             open "scores.dat" for OUTPUT as #1

    for score=1 to 10
        print #1,starname(score)
        print #1,starscore(score)
    next score
    for score=1 to 10
        print #1,pongname(score)
        print #1,pongscore(score)
    next score
    print #1,timeflag
    print #1,tiesaucer:print #1,vol255:print #1,muting
    print #1,fps:Print #1,fullscreentoggle
    close #1
    end if
fsound_stream_stop(stream)
    FMUSIC_StopAllSongs

    ' sound pulsewave(220),3/(2*24)
    GetJoystick(0,buttons,xxx,yyy)
    GetJoystick(1,buttons2,xxx2,yyy2)
   ' if game2=11 then sleep 1000,1
if buttons=1 or buttons2=1 or (game2=11 and timer>timered2) then
   ' timered2=timer+60*5
'game2=game3
'mutexlock mutex
if game2=11 then game=10:game2=10
if game2=6 then game=5:game2=5
if game2=4 then game=3:game2=3
'game=game2-1
'game3=0
'game3=0
    ''mutexunlock mutex
   ' cup9top=0
   ' if game2=4 then
  'if cup9bottom>64 or cup9bottom>64 then
  savecapture=0
  if gameonflag<>2 then
      cup9top=0:cup9bottom=0
       pingpongtop=0
 pingpongbottom=0
  'end if
  end if
 ' if game2=10 then
      'FSOUND_Stream_Close(stream)
         '      FSOUND_Stream_Close(stream3)
        FSOUND_Stream_stop(stream)

      '   stream=FSOUND_Stream_Open(SOUND_FILE, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
       ' FSOUND_Stream_Stop(stream)
        if vol255=255 then fsound_Stream_Play(fsound_free, stream) 
      
      
 '     end if
    gameonflag=0
'end if
GetJoystick(0,buttons,xxx,yyy)
    GetJoystick(1,buttons2,xxx2,yyy2)
    buttons=0
    buttons2=0
'sleep 750,1
'game2=11
    'cls
   ' GAME3=GAME
    'screencopy
    'sleep 2000,1
end if
end if
 if game2=2 then
      if savecapture=0 then
                 savecapture=1
                 kill("scores.dat"):sleep (1000)
             open "scores.dat" for OUTPUT as #1

    for score=1 to 10
        print #1,starname(score)
        print #1,starscore(score)
    next score
    for score=1 to 10
        print #1,pongname(score)
        print #1,pongscore(score)
    next score
    print #1,timeflag
    print #1,tiesaucer:print #1,vol255:print #1,muting
    print #1,fps:Print #1,fullscreentoggle
    close #1
    end if
         FMUSIC_StopAllSongs

    ' sleep 750,1
    ' sound pulsewave(220),3/(2*24)
    'buttons=0:buttons2=0
    if GetJoystick(0,buttons,xxx,yyy) then
      '  buttons=1
        end if
    GetJoystick(1,buttons2,xxx2,yyy2)
   ' if  then timered3=timer-3
    
if ((buttons=1 or buttons2=1) or timered2<timer) then
savecapture=0
' timered3=0

    timered2=timer+60*7
    If Int(Rnd*4)>2 Then mercy=2 Else mercy=1:End if
 '   'mutexlock mutex
   ' game=1-1
    if game3=12 then 
        game=12
       ' game2=12
  '      game3=0
elseif game3=1 then 
    game=1
   ' game2=1
   ' game3=0
end if
'GAME3=GAME
 '   ''mutexunlock mutex
    cup9top=0
    cup9bottom=0
    'cls
     energytop=100
     energybottom=100 
                 
     livestop=99
     livesbottom=99
    'screencopy
    'sleep 2000,1
   ' game2=11
    cup9top=0
    cup9bottom=0
    singletopx=0
   singletopy=0
   singletopz=0
  singlebottomx=0
  singlebottomy=0 
  singlebottomz=0
  topxxx=1280/2
  bottomxxx=1280/2
  topyyy=+64+32+64
  bottomyyy=720-(64+32+64)
  topzzz=.20
  bottomzzz=.20
  for calca=1 to (720/2)
    tempgunx=0
    topgunx(calca)=0
    tempguny=0
    topguny(calca)=0
    tempgunx2=0
    bottomgunx(calca)=0
    tempguny2=0
    bottomguny(calca)=0
next calca
for tempgunx=0 to 1280
    for tempguny=0 to 720
        topgun(tempgunx,tempguny,1)=0
        topgun3(tempgunx,tempguny)=0
        bottomgun(tempgunx,tempguny,1)=0
        bottomgun3(tempgunx,tempguny)=0
        
        
    next
    next
        
           ' timered2=timer+120*.5*10
zcount=0
pressed=0
end if
end if
'din sound7 AS integer
if (game2>0 and timeflag=1) or game2=0 then
    screenlock
'for straightx=1 to (1280)/2:for straighty=1 to 720 'to 1 step -1
  put((1280)/2-((21*8*6)/2),0),pic,ALPHA,INT(255/2)'setalpha
  if val(left(time,2))<12 then
      draw string ((1280)/2-24,8*6/3-4),"AM",rgb(255,255,0)
  else
          draw string ((1280)/2-24,8*6/3*2-4),"PM",rgb(255,255,0)
  
      end if
      screenunlock
  end if
  
if   game2=1 or game2=12 or game2=3 or game2=5 or game2=10 or game2=12 then
 '   IF RND>.67 THEN randomize (Rnd+timer)/(Rnd+Timer):mercy=Int(Rnd*4)+1:If mercy=0 Then mercy=1:End if
 
  if   game2=1 or game2=12  then
'    IF SINGLETOPX>0 THEN SINGLETOPX=SINGLETOPX-(pieyedi*2/360)*4.5
'    IF SINGLETOPX<0 THEN SINGLETOPX=SINGLETOPX+(pieyedi*2/360)*4.5
'     IF SINGLEBOTTOMX>0 THEN SINGLEBOTTOMX=SINGLEBOTTOMX-(pieyedi*2/360)*4.5
'    IF SINGLEBOTTOMX<0 THEN SINGLEBOTTOMX=SINGLEBOTTOMX+(pieyedi*2/360)*4.5
'     IF SINGLETOPY>0 THEN SINGLETOPY=SINGLETOPY-(pieyedi*2/360)*4.5
'    IF SINGLETOPY<0 THEN SINGLETOPY=SINGLETOPY+(pieyedi*2/360)*4.5
'     IF SINGLEBOTTOMY>0 THEN SINGLEBOTTOMY=SINGLEBOTTOMY-(pieyedi*2/360)*4.5
'    IF SINGLEBOTTOMY<0 THEN SINGLEBOTTOMY=SINGLEBOTTOMY+(pieyedi*2/360)*4.5
 if singletopx<0 and topxxx>64+16 then topxxx=topxxx+(100*(singletopx*100/(pieyedi*2*3)/100)):ship1rotatex=ship1rotatex+(pieyedi/(360/4))
if singletopx>0 and topxxx<1280-64 then topxxx=topxxx+(100*(singletopx*100/(pieyedi*2*3)/100)):ship1rotatex=ship1rotatex-(pieyedi/(360/4))
if singlebottomx<0 and bottomxxx>64+16 then bottomxxx=bottomxxx+(100*(singlebottomx*100/(pieyedi*2*3)/100)):ship2rotatex=ship2rotatex+(pieyedi/(360/4))
if singlebottomx>0 and bottomxxx<1280-64 then bottomxxx=bottomxxx+(100*(singlebottomx*100/(pieyedi*2*3)/100)):ship2rotatex=ship2rotatex-(pieyedi/(360/4))
if singletopy<0 and topyyy>(64+32+32+32) then topyyy=topyyy+(100*(singletopy*100/(pieyedi*2)/100)):ship1rotatey=ship1rotatey+(pieyedi/(360/4))
if singletopy>0 and topyyy<720/2-(33) then topyyy=topyyy+(100*(singletopy*100/(pieyedi*2)/100)):ship1rotatey=ship1rotatey-(pieyedi/(360/4))
if singlebottomy<0 and bottomyyy>720/2+(33) then bottomyyy=bottomyyy+(100*(singlebottomy*100/(pieyedi*2)/100)):ship2rotatey=ship2rotatey+(pieyedi/(360/4))
if singlebottomy>0 and bottomyyy<720-(64+32+32+32) then bottomyyy=bottomyyy+(100*(singlebottomy*100/(pieyedi*2)/100)):ship2rotatey=ship2rotatey-(pieyedi/(360/4))
'if topyyy<64+16 then topyyy=64+16 
'if topyyy>720-(64+16) then topyyy=720-(64+16) 
'if bottomyyy<64+16 then bottomyyy=64+16 
'if bottomyyy>720+(64+16) then bottomyyy=720+(64+16) 
'if singletopx<1.5 and singletopx>=-1.65 then singletopx=0
'if singlebottomx<=1.65 and singlebottomx>=-1.65 then singlebottomx=0
IF SINGLETOPX>0 THEN SINGLETOPX=singletopx-(pieyedi*2/(360))*3
IF SINGLETOPX<0 THEN SINGLETOPX=SINGLETOPX+(pieyedi*2/(360))*3
IF SINGLEBOTTOMX<0 THEN SINGLEBOTTOMX=SINGLEBOTTOMX+(pieyedi*2/(360))*3
IF SINGLEBOTTOMX>0 THEN SINGLEBOTTOMX=SINGLEBOTTOMX-(pieyedi*2/(360))*3
IF SINGLETOPY>0 THEN SINGLETOPY=singletopy-(pieyedi*2/(360))*3
IF SINGLETOPY<0 THEN SINGLETOPY=SINGLETOPy+(pieyedi*2/(360))*3
IF SINGLEBOTTOMY<0 THEN SINGLEBOTTOMY=SINGLEBOTTOMy+(pieyedi*2/(360))*3
IF SINGLEBOTTOMY>0 THEN SINGLEBOTTOMY=SINGLEBOTTOMy-(pieyedi*2/(360))*3
'if singlebottomy<0-(pieyedi/pieyedi) then singlebottomy=0-(pieyedi/pieyedi)
'if singlebottomy>0+(pieyedi/pieyedi) then singlebottomy=0+(pieyedi/pieyedi)
'if singletopy<0-(pieyedi/pieyedi) then singletopy=0-(pieyedi/pieyedi)
'if singletopy>0+(pieyedi/pieyedi) then singletopy=0+(pieyedi/pieyedi)



'if ship1rotatex>0 then ship1rotatex=ship1rotatex-(pieyedi/(360/128))
'if ship1rotatex<0 then ship1rotatex=ship1rotatex+(pieyedi/(360/128))
'if ship2rotatex>0 then ship2rotatex=ship2rotatex-(pieyedi/(360/128))
'if ship2rotatex<0 then ship2rotatex=ship2rotatex+(pieyedi/(360/128))
'if ship1rotatex>(pieyedi/(360/(256/2)))*1 and singletopx<then ship1rotatex=0

'if ship1rotatex=0 then singletopx=0
'if  ship1rotatex=0 then singlebottomx=0
'if  ship2rotatey=0 then singletopy=0
'if  ship2rotatey=0 then singlebottomy=0

'if topzzz<.025 or topzzz>1-.025 then singletopz=0
'if bottomzzz<.025 or bottomzzz>1-.025 then singlebottomz=0

if singletopz>0 then topzzz=topzzz-.0125/3:end if 'else singletopz=singletopz+.5:end if
if singletopz<0 then topzzz=topzzz+.0125/3:end if  'else singletopz=singletopz-.5:end if
if singlebottomz>0 then bottomzzz=bottomzzz-.0125/3:end if 'else singlebottomz=singlebottomz+.5:end if
if singlebottomz<0 then bottomzzz=bottomzzz+.0125/3:end if 'else singlebottomz=singlebottomz-.5:end if
'if singletopx<.5 and singletopx>-.5 then singletopx=0
singletopz=0
singlebottomz=0
'SINGLEBOTTOMY=0
'SINGLETOPY=0
'if singletopz>0 and topyyy<=(720)/2 then topyyy=topyyy+singletopy:singletopy=singletopy-1
'if singlebottomy>0 and bottomyyy<=720-64 then bottomyyy=bottomyyy+singlebottomy:singlebottomy=singlebottomy-1
'if singlebottomy<0 and bottomyyy>=(720)/2+16 then bottomyyy=bottomyyy+singlebottomy:singlebottomy=singlebottomy+1

elseif game2=3 or game2=5 or game2=10 then
   ' if singletopx<0 and singletopx>-2 then singletopx=-2
   '     if singletopx>0 and singletopx<2 then singletopx=2
'if singlebottomx<0 and singlebottomx>-2 then singlebottomx=-2
'        if singlebottomx>0 and singlebotomx<2 then singlebottomx=2
'9if gam2=10 or game2=5 or game2=3 then
  if singletopx<0 and topxxx-(1)>(128*1.5) then topxxx=topxxx+singletopx:singletopx=singletopx+.5
  if singletopx>0 and topxxx+(1)<1280-(128*1.5) then topxxx=topxxx+singletopx:singletopx=singletopx-.5
  if singlebottomx<0 and bottomxxx-(1)>(128*1.5) then bottomxxx=bottomxxx+singlebottomx:singlebottomx=singlebottomx+.5
  if singlebottomx>0 and bottomxxx+(1)<1280-(128*1.5) then bottomxxx=bottomxxx+singlebottomx:singlebottomx=singlebottomx-.5
'end if
'if singletopx<0 and topxxx-(128)>(128*1.5) then topxxx=topxxx-33
' if singletopx>0 and topxxx+(128)<1280-(128*1.5) then topxxx=topxxx+33'singletopx:singletopx=singletopx-3
'if singlebottomx<0 and bottomxxx-(128)>(128*1.5) then bottomxxx=bottomxxx-33'+singlebottomx:singlebottomx=singlebottomx+3
'if singlebottomx>0 and bottomxxx+(128)<1280-(128*1.5) then bottomxxx=bottomxxx+33'singlebottomx:singlebottomx=singlebottomx-3
'if singletopx<.75 and singletopx>-.75 then singletopx=0
'if singlebottomx<.75 and singlebottomx>-.75 then singlebottomx=0

'if singletopy<0 and topyyy>=64+16 then topyyy=topyyy+singletopy:singletopy=singletopy+1
'if singletopy>0 and topyyy<=720-64 then topyyy=topyyy+singletopy:singletopy=singletopy-1
'if singlebottomy>0 and bottomyyy<=720-64 then bottomyyy=bottomyyy+singlebottomy:singlebottomy=singlebottomy-1
'if singlebottomy<0 and bottomyyy>=64+16 then bottomyyy=bottomyyy+singlebottomy:singlebottomy=singlebottomy+1 
'if game2=3 then
'if topxxx-32-24<131 then singletopx=0'singletopx-(singletopx*2)
'  if topxxx+32+24>128031 then singletopx=0'singletopx-(singletopx*2)
'end if
'if game2=3 or game2=5 then 
'if bottomxxx-32-24<131 then singlebottomx=0'singlebottomx-(singlebottomx*2)
'  if bottomxxx+32+24>128031 then singlebottomx=0'singlebottomx-(singlebottomx*2)
'end if
dim hx as SINGLE'integer
'dim as single toplayerx,bottomlayerx
'toplayerx=singletopx
'bottomlayer=singlebottomx
'if game2=5 or game2=10 then
    hx=1'+(1*(ballspeed*100/12/100))
   if game2=5 then 
 if topxxx-(128*1.5)<(3) then singletopx=singletopx-(singletopx*hx)
  if topxxx+(128*1.5)>1280-(3) then singletopx=singletopx-(singletopx*hx)
  if bottomxxx-(128*1.5)<(3) then singlebottomx=singlebottomx-(singlebottomx*(hx+(.25)))
  if bottomxxx+(128*1.5)>1280-(3) then singlebottomx=singlebottomx-(singlebottomx*(hx+(.25)))
end if
'end if
if game2=10 then
'if game2=3 then
if topxxx-(128*1.5)<(3) then singletopx=singletopx-(singletopx*hx)
  if topxxx+(128*1.5)>1280-(3) then singletopx=singletopx-(singletopx*hx)
  if bottomxxx-(128*1.5)<(3) then singlebottomx=singlebottomx-(singlebottomx*hx)
  if bottomxxx+(128*1.5)>1280-(3) then singlebottomx=singlebottomx-(singlebottomx*hx)
 ' end if
end if
rem rem rem
'if game2=5 then
'  singletopx==singletopx-(singletopx*2)  
'end if
if game2=3 then
'if game2=3 then
if topxxx-(128*1.5)<(3) then singletopx=singletopx-(singletopx*(hx+(.25)))
  if topxxx+(128*1.5)>1280-(3) then singletopx=singletopx-(singletopx*(hx+(.25)))
  if bottomxxx-(128*1.5)<(3) then singlebottomx=singlebottomx-(singlebottomx*(hx+(.25)))
  if bottomxxx+(128*1.5)>1280-(3) then singlebottomx=singlebottomx-(singlebottomx*(hx+(.25)))
 ' end if
end if

end if
'if ankle=0 then
'if joy1=1 and (topxxx<(128*1.5)+3 or topxxx>1280-(128*3)) then singletopx=0
'if joy2=1 and (bottomxxx<(128*1.5)+3 or bottomxxx>1280-(128*3)) then singlebottomx=0
'end if
'dim as SINGLE tx,ty,bx,by
if   (game=1 or game=12)  then 
if shipsflag=0 then shipsflag=1
end if    
'if singletopx<0 then 
'    tx=singletopx-(singletopx*2)
'elseif singletopx>0 then
'    tx=singletopx'-(singletopx+2)
'end if
'if singletopy<0 then 
'    ty=singletopy-(singletopy*2)
'elseif singletopy>0 then
'    ty=singletopy
'end if
' if singlebottomx<0 then 
'    bx=singlebottomx-(singlebottomx*2)
'elseif singlebottomx>0 then
'    bx=singlebottomx
'end if 
'if singlebottomy<0 then 
'    by=singlebottomy-(singlebottomy*2)
'elseif singlebottomy>0 then
'    by=singlebottomy
'end if
'end if
'IF SINGLEBOTTOMZ>0 THEN SINGLEBOTTOMZ=SINGLEBOTTOMZ-6
'IF SINGLEBOTTOMZ<0 THEN SINGLEBOTTOMZ=SINGLEBOTTOMZ+6
'IF SINGLETOPZ>0 THEN SINGLETOPZ=SINGLETOPZ-6
'IF SINGLETOPZ<0 THEN SINGLETOPZ=SINGLETOPZ+6

'if singlebottomx<0 and game2=3 then singlebottomx=singlebottomx+.333
'if singlebottomx>0 and game2=3 then singlebottomx=singlebottomx-.333
 dim as SINGLE ballyx=1280-(129*2),singled=6'2+129
if GetJoystick(1,buttons2,xxx2,yyy2,xxx,yyy) or game2=12 or game2=10 or game2=5 then
 '      if rnd>.97 then xxx=0:yyy=0:xxx2=0:yyy2=0:buttons=0:joy1=0:singletopx=0:goto jumpy1
Randomize RND+TIMER+Rnd+timer
               if int(rnd*100)+1<50 then xxx2=+.90 else xxx2=-.90:end if
joy1=0
   dim rndey as SINGLE
  rndey=rnd
 '  rndey=int(rndey)
        
       if int(rndey*100)+1>49 then
           yyy=+.90
       else
           
        yyy=-.90
        end if
jumpyx1:
rndey=rnd
 if int(topzzz*5*12)<int(bottomzzz*5*12) and rndey*100>49 then yyy=+.90
           if int(topzzz*5*12)>int(bottomzzz*5*12) and rndey*100<50 then yyy=-.90
 if topxxx<bottomxxx and rnd>.49 then xxx2=+.90
    if topxxx>bottomxxx and rnd<.5 then xxx2=-.90
'if int(bottomzzz*5*12)<>int(topzzz*5*12) then'and topxxx>bottomxxx-512 and topxxx<bottomxxx+512 then
          
           
  '     if topzzz<bottomzzz then yyy2=+.90
 '      if topzzz>bottomzzz then yyy2=-.90
           ' yyy2=-.90
'                           jumpjifi1:
rndey=rnd
       if rndey>.49 then 
           yyy2=-.90
       else
           yyy2=+.90
           end if
'                  end if
'end if
singled=6
    '   if topzzz<bottomzzz
         '  xxx2=rnd:yyy2=rnd
        ' 'RANDOMIZE RND+TIMER+Rnd+timer
        ' if topxxx>bottomxxx-128 and topxxx<bottomxxx+128 AND RND>.49 then buttons2=16'(1  And (1 Shl 3))
'if Rnd<.50 then buttons2=32 Else buttons2=16:End If
if Rnd<.50 And topfired<255/2+1 then 
	
'EndIf
'buttons2=32 
'ElseIf Rnd<.50 And topfired<255/2+1 Then  
	buttons2=16
	End If

'for astrocnt=3 to 1 STEP -1
'    IF ASTX(ASTROCNT)<TOPXXX+96 AND ASTX(ASTROCNT)>TOPXXX-96 AND RND<.34 THEN BUTTONS2=16
'    NEXT ASTROCNT
 ' end if
 jumpy1:
else
    'if yyy2<-1 then yyy2=-1
     '   if yyy2>1 then yyy2=1
joy1=1
' yyy=yyy2
end if
if judgedtop>timer then buttons2=0:end if
  ' yyy2=yyy2-(yyy2*2)
  'if singletopz<>0 then
  
if yyy<-.75 and yyy>-1.5 then singletopz=singletopz+singled

if yyy>.75 and yyy<1.5 then singletopz=singletopz-singled
if topzzz<(.20/5) then topzzz=(.20/5)':topzzz=topzzz-.25/4'.125

if topzzz>.20 then topzzz=.20':topzzz=topzzz-.25/4
'if topzzz<0 then topzzz=0
'end if
'if yyy>+.90 then topzzz=.90
'if yyy<-.90 then topzzz=-.90
dim as integer ballyx2
ballyx2=ballx

    if (game2=5 or game2=10) then
                if balldirectiony>0 or (balldirectiony<0 and bally>(720/3)*2+(720/3/2)) then ballx=1280/2:if topxxx>ballx-(128*1.5/3) and topxxx<ballx+(128*1.5/3) then singletopx=0
'if balldirectiony<0 and bally<(720/3)*2 then ballx=ballyx2
if ballx>topxxx and topxxx<(1280-129)-(ballyx/17) then xxx2=+.90
if ballx>topxxx and topxxx>(1280-129)-(ballyx/17) then xxx2=-.90
if ballx<topxxx and topxxx>+129+(ballyx/17) then xxx2=-.90
if ballx<topxxx and topxxx<+129+(ballyx/17) then xxx2=+.90
'singletopx=singletopx+.333
ballx=ballyx2
end if
' GetJoystick(1,buttons2,xxx2,yyy2)
   
   if xxx2>-.75 and xxx2<.75 then
    if (singletopx<0) and (game2=1 or game2=12) then singletopx=singletopx+(pieyedi*2/(360))*1'4.5'333
if (singletopx>0) and (game2=1 or game2=12) then singletopx=singletopx-(pieyedi*2/(360))*1
if singletopx<4 and singletopx>-4 then singletopx=0
'elseif xxx2>-.75 and xxx2<.75 and (game2<>1 and game2<>12) then
   if (singletopx<0) and (game2=3 or game2=5 or game2=10) then singletopx=singletopx+.125'333
if (singletopx>0) and (game2=3 or game2=5 or game2=10) then singletopx=singletopx-.125'333 
'singletopx=0
'end if
 'if yyy2>-.75 and yyy2<.75 and (game2=1 or game2=12) then
    if (singletopy<0) and (game2=1 or game2=12) then singletopy=singletopy+(pieyedi*2/(360))*1
if (singletopy>0) and (game2=1 or game2=12) then singletopy=singletopy-(pieyedi*2/(360))*1
'singletopz=0
'singletopy=0
'elseif yyy2>-.75 and yyy2<.75 and (game2<>1 and game2<>12) then
   if (singletopy<0) and (game2=3 or game2=5 or game2=10) then singletopy=singletopy+.125'333
if (singletopy>0) and (game2=3 or game2=5 or game2=10) then singletopy=singletopy-.125'333 
end if
   ' xxx=int(xxx):yyy=int(yyy):xxx2=int(xxx2):yyy2=int(yyy2)
' if xxx2>-2 and xxx2<2 and yyy2>-2 and yyy2<2 then 
if topxxx<(128+8) and (game2=1 or game2=12) then 
        topxxx=(128+8)'topxxx
                singletopx=0

 ' singletopx=-64+singletopx
 ' sound pulsewave(notes(2)),3/(2*24)
end if

if xxx2<-.75 and xxx2>-1.5 then 
 '      if ankle=1 then ankle=0:singletopx=-1.5

             '   sound pulsewave(notes(1)),3/(2*24)
            ' if smoothtopxxx2=2 and smoothtopxxx>0 then
             '  smoothtopxxx=smoothtopxxx-64
              '                if smoothtopxxx<0 then smoothtopxxx=0
'if tiesaucer=0 and vol255=255 and (game2=1 or game2=12) and singletopx<0 then FSOUND_Stream_Play(FSOUND_FREE, stream11)
               '    topxxx=topxxx+64

         '      else
'smoothtopxxx=smoothtopxxx+64
'smoothtopxxx2=1corey READ THIS WORKING ON REMOVING SMOOTHXXX AND FILTHY CODE RIGHT HERE
'smoothtopxxx2-1
'end if

'if singletopx-8>-(33+8) then 
    
 '   singletopx=0
    if   (game2=1 or game2=12) and singletopx>-(pieyedi) then 
        IF TIESAUCER=0 THEN singletopx=singletopx-(pieyedi*2/(360))*9
    
    IF TIESAUCER=1 THEN    SINGLETOPX=SINGLETOPX-(pieyedi*2/(360))*9
    
    END IF
        
        
 ' END IF      
        
        ':FSOUND_Stream_Play(FSOUND_FREE, stream10)
if game2=5 or game2=10 then 
    singletopx=singletopx-2*2.25'5'int(.5*(60-tt4*100/(120)/100))'1.5
elseif game2=3 then
    IF TOPXXX+(128*1.5)>1280-(3) THEN
        SINGLETOPX=SINGLETOPX-1.625*.95'*1.25
        ELSE
    singletopx=singletopx-1.625*.95'*1.25'90'int(.5*(60-tt4*100/(120)/100))
    END IF
    end if
    'topxxx=topxxx+singletopx
'if smoothtopxxx2<1 then smoothtopxxx2=0
 ' if singletopx<=-32 then singletopx=singletopx-4
'ELSE
'    SINGLETOPX=0
'    END IF
  
  
end if
 if topxxx>1280-(128+8) and (game2=1 or game2=12) then 
        topxxx=1280-(128+8)
                singletopx=0
  '      singletopx=64+singletopx
  '      sound pulsewave(notes(2)),3/(2*24)
 '''''''''''''''''''''''''   'sound pulsewave(notes(2)/4),3/(2*24)
end if
if xxx2>.75 and xxx2<1.5  then 
   ' topxxx=topxxx+(64*(smoothtopxxx2*100/64/100))
'if ankle=1 then ankle=0:singletopx=1.5

'  sound pulsewave(notes(1)),3/(2*24)
'if tiesaucer=0 and vol255=255 and (game2=1 or game2=12) and singletopx>0  then FSOUND_Stream_Play(FSOUND_FREE, stream11)
'singletopx=0
'if singletopx+8<+(33+8) then 
    if   (game2=1 or game2=12) and singletopx<(pieyedi) then 
        IF TIESAUCER=0 THEN singletopx=singletopx+(pieyedi*2/(360))*9    
IF TIESAUCER=1 THEN singletopx=singletopx+(pieyedi*2/(360))*9
'END IF
END IF
    if game2=10 or game2=5 then 
        singletopx=singletopx+2*2.25'int(.5*(60-tt4*100/(120)/100))
    elseif game2=3 then
IF TOPXXX-(128*1.5)<3 THEN
        SINGLETOPX=SINGLETOPX+1.625*.95'*1.25
        ELSE
    singletopx=singletopx+1.625*.95'*1.25'int(.5*(60-tt4*100/(120)/100))
END IF   
end if
        
        '11.5
    'topxxx=topxxx+singletopx
   ' topxxx=topxxx+64



 '  ELSE
 '   SINGLETOPX=0
 '   END IF
    end if
if yyy2<-.75 and yyy2>-1.5  and (game2=1 or game2=12) then 
    
   ' if game2=5 or game2=10 then 
    'topyyy=topyyy+singletopy

                 '  topyyy=topyyy-64'(64*(smoothtopyyy*100/64/100))
            'sound pulsewave(notes(1)),3/(2*2.254)

'if singletopy-7>-(33+7) then
if game2=1 or game2=12 then
  '   IF TIESAUCER=0 THEN
     if singletopy>-(pieyedi) then     singleTOPy=singleTOPy-(pieyedi*2/(360))*9
'ELSE
'  if singletopy>pieyedi*2   SINGLETOPY=SINGLETOPy-(pieyedi*2/(360))*9
'    END IF
    
    end if
   ' singletopy=singletopy-5
'END IF


end if
if yyy2>.75 and yyy2<1.5 and (game2=1 or game2=12) then
      '  FSOUND_Stream_Play(FSOUND_FREE, stream9)
'if tiesaucer=0 and vol255=255 and (game2=1 or game2=12) and singletopy>0 then FSOUND_Stream_Play(FSOUND_FREE, stream10)
     ' if singletopy+7<(33+7) then 'singletopy=singletopy+5
     if game2=1 or game2=12 then
 IF  singletopy<(pieyedi) THEN singleTOPy=singleTOPy+(pieyedi*2/(360))*9
'ELSE
'IF TIESAUCER=1 THEN    SINGLETOPY=SINGLETOPY+(pieyedi*2/(360))*9
    END IF
    
    
   ' singletopy=singletopy-5

   ' topyyy=topyyy+singletopy

END IF
'end if
'if   (game2=1 or game2=12) then
' IF TOPFIRED>255*3-1 THEN
'        TOPFIRED=0
'    END IF
'END IF    
    IF GAME2=1 OR GAME2=12 THEN
' if xxx>-2 and xxx<2 and yyy>-2 and yyy<2 then 
if (buttons2=16 or buttons2=32) And mercy=2 and topfired<255/2+1 then
 '  if buttons2=1 then buttons2=5
' if tiesaucer=1 then
   If buttons2=16 Then
    topgun(topxxx-INT(34*(TOPZZZ*5)),topyyy-33,1)=1
        topgun3(topxxx-INT(34*(TOPZZZ*5)),topyyy-33)=int(topzzz*5*12)
         topgun(topxxx+INT(34*(TOPZZZ*5)),topyyy-33,1)=1
        topgun3(topxxx+INT(34*(TOPZZZ*5)),topyyy-33)=int(topzzz*5*12)
 
shots1=3
topgunx(topfired)=topxxx-INT(34*(TOPZZZ*5))
        topguny(topfired)=topyyy-33
    topfired=topfired+1
topgunx(topfired)=topxxx+INT(34*(TOPZZZ*5))
        topguny(topfired)=topyyy-33
    topfired=topfired+1
 '  ElseIf buttons2=32 And topxxx>bottomxxx-Int(32*(bottomzzz*5)) And topxxx<bottomxxx+Int(32*(bottomzzz*5)) And Int(topzzz*5)<=Int(bottomzzz*5)+3 And Int(topzzz*5)>=Int(bottomzzz*5)-3 then
 '  	 FSOUND_Stream_Stop(stream13)
 '       if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream13)
 '  	Line(topxxx,topyyy-16)-(topxxx,bottomyyy+16),RGB(127,63,255)
 '[  	 for hondal=1 to int(rnd*25/3)+1*(25/3)
  '                    hondax=rnd*64
  '                     honday=rnd*64
  '                     screenlock
  '                     circle(bottomxxx+32-hondax,bottomyyy+32-honday),rnd*256/4*1.5*(bottomzzz*5),rgb(255,(255*rnd),0),,,,F
  '                             screenunlock
'judgedbottom=timer+1

  '                 screencopy
  '                 flip
                  ' screenunlock
   '                sleep 0,1
   '                next hondal
  ' circle(bottomxxx,bottomyyy+32),64,rgb(255,192,0),,,,F
  '      screenunlock    
   '   BOTTOMgun(tempgunx2,tempguny2,1)=0':bottomfired=bottomfired-1
   '             BOTTOMgun3(tempgunx2,tempguny2)=0':bottomfired=bottomfired-1
'BOTTOMgunx(calca)=0
'BOTTOMguny(calca)=0
'astrox(astrocnt)=0
'astroy(astrocnt)=0
'int(astroz(astrocnt)*5*12)=0
'astroz(astrocnt)=365
 '[cup9top=cup9top+5
  '               energybottom=energybottom-3
  '              if energybottom<1 then
  '                  judgedbottom=timer+3
  '                  energybottom=100
  '                  livesbottom=livesbottom-1
  '                  END IF
  '                  if livesbottom<1 then livesbottom=0
   End If
 ' FSOUND_Stream_Close(stream5)
         '      FSOUND_Stream_Close(stream3)

  '       stream=FSOUND_Stream_Open(SOUND_FILE5, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
       
'ElseIf buttons2=32 Then
'		Line(topxxx,topyyy-16)-(topxxx,720),RGB(127,63,255)
'		 FSOUND_Stream_Stop(stream5)
'        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream5)
   ElseIf (buttons2=16 or buttons2=32) and topfired>255*4.97 AND TIMER>TOPTIME THEN
TOPTIME=TIMER+16
END IF

end if
dim as SINGLE singled2=6
    if GetJoystick(0,buttons,xxx,yyy,xxx2,yyy2) or game2=12 or game2=10 then
      'if'RANDOMIZE RND+TIMER+Rnd+timer rnd>.97 then xxx=0:yyy=0:xxx2=0:yyy2=0:buttons=0:joy2=0:singlebottomx=0:goto jumpy2
Randomize RND+TIMER+Rnd+Timer
     if int(rnd*100)+1<50 then xxx=+.90 else xxx=-.90:end if
        '    if int(rnd*100)+1<50 then xxx=+.90 else xxx=-.90:end if
joy2=0
   dim rndey as SINGLE
   rndey=rnd       
       if int(rnd*100)+1>49 then
           
          yyy2=-.90
         else
             yyy2=+.90
             end if
 '   jumpyx2:
    rndey=rnd
     if int(bottomzzz*5*12)<int(topzzz*5*12) and rndey*100>49 then yyy2=+.90
           if int(bottomzzz*5*12)>int(topzzz*5*12) and rndey*100<50 then yyy2=-.90
    if bottomxxx<topxxx and rnd>.49 then xxx=+.90
    if bottomxxx>topxxx and rnd<.5 then xxx=-.90
          ' if int(topzzz*5*12)<>int(bottomzzz*5*12) then'and bottomxxx>topxxx-512 and bottomxxx<topxxx+512 then
      ' if bottomzzz<topzzz then yyy=+.90
       
      ' if bottomzzz>topzzz then    yyy=-.90
                  'end if
  '         jumpjifi2:
          
      '     end if
       rndey=rnd
       if int(rndey*100)+1>49 then 
           yyy=-.90
       else
           yyy=+.90
           end if
       
           
  '            end if
              
 singled2=6
    '   if topzzz<bottomzzz
         '  xxx2=rnd:yyy2=rnd

 'if bottomxxx>topxxx-128 and bottomxxx<topxxx+128 AND RND>.49 then buttons=16'(4  And (1 Shr a))
'if mercy=1 then buttons=16
if Rnd<.50 And bottomfired<255/2+1 then 
	
'EndIf
buttons=16 
'ElseIf Rnd<.50 And bottomfired<255/2+1 Then  
'	buttons=16
	End If
'end if
'for astrocnt=3 to 1 STEP -1
'    IF ASTX(ASTROCNT)<BOTTOMXXX+96 AND ASTX(ASTROCNT)>BOTTOMXXX-96 AND RND<.34 THEN BUTTONS=16
'    NEXT ASTROCNT
 jumpy2:
else 'yyy2=yyy
 joy2=1
    end if

if judgedbottom>timer then buttons=0:end if


' yyy2=yyy2-(yyy2*2)
 ' if singlebottomzzz><0 then 
'if yyy2<-.75 and yyy2>-1.5 then singlebottomzzz=singlebottomzzz-.03:bottomzzz=bottomzzz-.003:if bottomzzz<-1 then bottomzzz=-1:end if
'if yyy2>+.90 then bottomzzz=.90
'if yyy2<-.90 then bottomzzz=-.90
'if yyy2>.75 and yyy2<1.5 then singlebottomzz=singlebottomzzz+.03:bottomzzz=bottomzzz+.003:if bottomzzz<1 then bottomzzz=1:end if
if yyy2<-.75 and yyy2>-1.5 then singlebottomz=singlebottomz+singled2

if yyy2>.75 and yyy2<1.5 then singlebottomz=singlebottomz-singled2
if bottomzzz<(.20/5) then bottomzzz=(.20/5)

if bottomzzz>.20 then bottomzzz=.20':bottomzzz 
'bottomzzz=bottomzzz-.25/4'.125

dim as integer ballyx3
ballyx3=ballx

if game2=10 then
if balldirectiony<0 or (balldirectiony>0 and bally<(720/3/2)) then ballx=1280/2:if bottomxxx>ballx-(128*1.5/3) and bottomxxx<ballx+(128*1.5/3) then singlebottomx=0':singlebottomx=0
'if balldirectiony>0 and bally>(720/3) then ballx=ballyx3

if ballx>bottomxxx and bottomxxx<(1280-129)-(ballyx/17) then xxx=+.90
if ballx>bottomxxx and bottomxxx>(1280-129)-(ballyx/17) then xxx=-.90
if ballx<bottomxxx and bottomxxx>+129+(ballyx/17) then xxx=-.90
if ballx<bottomxxx and bottomxxx<+129+(ballyx/17) then xxx=+.90

'if ballx>topxxx and topxxx<1279-(1279/16+129) then xxx2=+.90
'if ballx>topxxx and topxxx>1279-(1279/16+129) then xxx2=-.90
'if ballx<topxxx and topxxx>127916+129 then xxx2=-.90
'if ballx<topxxx and topxxx<1279/16+129 then xxx2=+.90
end if
ballx=ballyx3
    if xxx>-.75 and xxx<.75 then
    if (singlebottomx<0) and (game2=1 or game2=12) then singlebottomx=singlebottomx+(pieyedi*2/(360))*1
if (singlebottomx>0) and (game2=1 or game2=12) then singlebottomx=singlebottomx-(pieyedi*2/(360))*1
if singlebottomx<4 and singlebottomx>-4 then singlebottomx=0'singlebottomx=0
'elseif xxx>-.75 and xxx<.75 and (game2<>1 and game2<>12) then
   if (singlebottomx<0) and (game2=3 or game2=5 or game2=10) then singlebottomx=singlebottomx+.125'333
if (singlebottomx>0) and (game2=3 or game2=5 or game2=10) then singlebottomx=singlebottomx-.125'333 
'singlebottomx=0
'end if
' if yyy>-.75 and yyy<.75 and (game2=1 or game2=12) then
    if (singlebottomy<0) and (game2=1 OR GAME2=12) then singlebottomy=singlebottomy+(pieyedi*2/(360))*1
if (singlebottomy>0) and (game2=1) then singlebottomy=singlebottomy-(pieyedi*2/(360))*1
'singlebottomy=0
'elseif yyy>-.75 and yyy<.75 and (game2<>1 and game2<>12) then
   if (singlebottomy<0) and (game2=3 or game2=5 or game2=10) then singlebottomy=singlebottomy+.125'333
if (singlebottomy>0) and (game2=3 or game2=5 or game2=10) then singlebottomy=singlebottomy-.125'333 
end if
if bottomxxx<(128+8) and (game2=1 or game2=12) then 
        bottomxxx=(128+8)'topxxx
        singlebottomx=0

 ' singletopx=-64+singletopx
 ' sound pulsewave(notes(2)),3/(2*24)
end if

if xxx<-.75 and xxx>-1.5 then
 '          if ankle=1 then ankle=0:singlebottomx=-1.5

             '   sound pulsewave(notes(1)),3/(2*24)
            ' if smoothtopxxx2=2 and smoothtopxxx>0 then
             '  smoothtopxxx=smoothtopxxx-64
              '                if smoothtopxxx<0 then smoothtopxxx=0
'if tiesaucer=0 and vol255=255 and (game2=1 or game2=12) and singlebottomx<0 then FSOUND_Stream_Play(FSOUND_FREE, stream11)
               '    topxxx=topxxx+64

         '      else
'smoothtopxxx=smoothtopxxx+64
'smoothtopxxx2=1corey READ THIS WORKING ON REMOVING SMOOTHXXX AND FILTHY CODE RIGHT HERE
'smoothtopxxx2-1
'end if

'if singlebottomx-8>-(33+8) then
 '       singlebottomx=0
'if singlebottomx-8>-(33+8) then 

    if   (game2=1 or game2=12) then 
        
         IF singlebottomx>-(pieyedi) THEN   SINGLEBOTTOMX=SINGLEBOTTOMX-(pieyedi*2/(360))*9
    
   ' IF TIESAUCER=1 THEN    singlebottomx=singlebottomx-(pieyedi*2/(360))*9'-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-`-3
    
       END IF 
'  END IF      
        
       ' singlebottomx=singlebottomx-3':FSOUND_Stream_Play(FSOUND_FREE, stream10)
if game2=10  then 
    singlebottomx=singlebottomx-2*2.25'5'int(.5*(60-tt4*100/(120)/100))'1.5
elseif game2=3 OR GAME2=5 then
  IF BOTTOMXXX+(128*1.5)>1280-(3) THEN
      
        SINGLEBOTTOMX=SINGLEBOTTOMX-1.625*.95'*1.25
        ELSE
    singleBOTTOMx=singleBOTTOMx-1.625*.95'*1.25'int(.5*(60-tt4*100/(120)/100))
'END IF
end if
end if
    'topxxx=topxxx+singletopx
'if smoothtopxxx2<1 then smoothtopxxx2=0
 ' if singletopx<=-32 then singletopx=singletopx-4
'ELSE
'    SINGLEBOTTOMX=0
 '   END IF
  
  
end if
 if bottomxxx>1280-(128+8) and (game2=1 or game2=12) then 
        bottomxxx=1280-(128+8)
        singlebottomx=0

  '      singletopx=64+singletopx
  '      sound pulsewave(notes(2)),3/(2*24)
 '''''''''''''''''''''''''   'sound pulsewave(notes(2)/4),3/(2*24)
end if
if xxx>.75 and xxx<1.5  then 
  '         if ankle=1 then ankle=0:singletopx=1.5

   ' topxxx=topxxx+(64*(smoothtopxxx2*100/64/100))
              '  sound pulsewave(notes(1)),3/(2*24)
'if tiesaucer=0 and vol255=255 and (game2=1 or game2=12) and singlebottomx>0 then FSOUND_Stream_Play(FSOUND_FREE, stream11)
'    singlebottomx=0

'if singlebottomx+8<+(33+8) then 
    if   (game2=1 or game2=12) then 
        IF singlebottomx<(pieyedi) THEN SINGLEBOTTOMX=SINGLEBOTTOMX+(pieyedi*2/(360))*9
        
  ' IF TIESAUCER=1 THEN     singlebottomx=singlebottomx+(pieyedi*2/(360))*9
   
END IF
'END IF
    if game2=10 then 
        singlebottomx=singlebottomx+2*2.25'5'int(.5*(60-tt4*100/(120)/100))
    elseif game2=3 OR GAME2=5 then
       IF BOTTOMXXX-(128*1.5)<3 THEN
        SINGLEBOTTOMX=SINGLEBOTTOMX+1.625*.95'*1.25
        ELSE
    singleBOTTOMx=singleBOTTOMx+1.625*.95'*1.25'int(.5*(60-tt4*100/(120)/100))
'END IF
end if
        end if
        
        '11.5
    'topxxx=topxxx+singletopx
   ' topxxx=topxxx+64



 '  ELSE
 '   SINGLEBOTTOMX=0
 '   END IF
    end if
if yyy<-.75 and yyy>-1.5  and (game2=1 or game2=12) then 
 '       if tiesaucer=0 and vol255=255 and (game2=1 or game2=12) and singlebottomy<0 then FSOUND_Stream_Play(FSOUND_FREE, stream10)

    'topyyy=topyyy+singletopy

                 '  topyyy=topyyy-64'(64*(smoothtopyyy*100/64/100))
            'sound pulsewave(notes(1)),3/(2*24)

'if singlebottomy-7>-(33+7) then 
    
if game2=1 or game2=12 then '    if singlebottomy+7<(33+7) then 
     '     IF TIESAUCER=0 THEN singlebottomy=singlebottomy-(pieyedi*2/(360))*9

IF singlebottomy>-(pieyedi) THEN    SINGLEBOTTOMY=SINGLEBOTTOMy-(pieyedi*2/(360))*9
    END IF
    
'    END IF
    
    
    
 '   singlebottomy=singlebottomy-5



end if
if yyy>.75 and yyy<1.5 and (game2=1 or game2=12) then 
     '   FSOUND_Stream_Play(FSOUND_FREE, stream9)

  '    if singlebottomy+(pieyedi*2/360)*(7<(33+7)) then 
  if game2=1 or game2=12 then
         ' IF TIESAUCER=0 THEN singlebottomy=singlebottomy+(pieyedi*2/(360))*9

IF singlebottomy<(pieyedi) THEN    SINGLEBOTTOMY=SINGLEBOTTOMY+(pieyedi*2/(360))*9
    END IF
   ' topyyy=topyyy+singletopy
'END IF

end if
 'IF BOTTOMFIRED>255*3-1 THEN
  '      BOTTOMFIRED=0
       ' FOR ASTROCNT=1 TO 255*5
       '     BOTTOMGUN(BOTTOMGUNX(ASTROCNT),BOTTOMGUNY(ASTROCNT),1)=0
       '     BOTTOMGUN3(
       '     BOTTOMGUNX(ASTROCNT)=0
       '     BOTTOMGUNY(ASTROCNT)=0
       '     NEXT ASTROCNT
   '  END IF   
   'end if
if   (game2=1 or game2=12) then 
if (buttons=16 or buttons=32) And mercy=1 AND BOTTOMFIRED<255/2+1 then
 '  if buttons=1 then buttons=5
' if tiesaucer=1 then
'      bottomgun(bottomxxx,bottomyyy,1)=1
'        bottomgun3(bottomxxx,bottomyyy)=int(bottomzzz*5*12)
       ' bottomgun(bottomxxx+(48*(bottomzzz*5)),bottomyyy,1)=1
       ' bottomgun3(bottomxxx+(48*(bottomzzz*5)),bottomyyy)=int(bottomzzz*5*12)
       ' if bottomgn3=0 then bottomgn3=int(bottomzzz*5*12)
'    else
      '  if buttons=1 then buttons=5
' if tiesaucer=1 then
'      bottomgun(bottomxxx,bottomyyy,1)=1
'        bottomgun3(bottomxxx,bottomyyy)=int(bottomzzz*5*12)
       ' bottomgun(bottomxxx+(48*(bottomzzz*5)),bottomyyy,1)=1
       ' bottomgun3(bottomxxx+(48*(bottomzzz*5)),bottomyyy)=int(bottomzzz*5*12)
       ' if bottomgn3=0 then bottomgn3=int(bottomzzz*5*12)
'    else
If buttons=16 Then
      bottomgun(bottomxxx-INT(34*(bottomzzz*5)),bottomyyy+33,1)=1
        bottomgun3(bottomxxx-INT(34*(bottomzzz*5)),bottomyyy+33)=int(bottomzzz*5*12)
        bottomgun(bottomxxx+INT(34*(bottomzzz*5)),bottomyyy+33,1)=1
        bottomgun3(bottomxxx+INT(34*(bottomzzz*5)),bottomyyy+33)=int(bottomzzz*5*12)
'        if bottomgn3=0 then bottomgn3=int(bottomzzz*5*12)
'        end if
shots2=3
bottomgunx(bottomfired)=bottomxxx-INT(34*(bottomzzz*5))
bottomguny(bottomfired)=bottomyyy+33
     bottomfired=bottomfired+1
     bottomgunx(bottomfired)=bottomxxx+INT(34*(bottomzzz*5))
bottomguny(bottomfired)=bottomyyy+33
     bottomfired=bottomfired+1

    ' for ranny=256 to 1 step -1
    '  FSOUND_Stream_Close(stream5)
         '      FSOUND_Stream_Close(stream3)
'  ElseIf buttons=32 And bottomxxx>topxxx-Int(32*(topzzz*5)) And bottomxxx<topxxx+Int(32*(topzzz*5)) And Int(bottomzzz*5)<=Int(topzzz*5)+3 And Int(bottomzzz*5)>=Int(topzzz*5)-3 then
'   	 FSOUND_Stream_Stop(stream13)
'        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream13)
'   	Line(bottomxxx,topyyy-16)-(bottomxxx,bottomyyy+16),RGB(127,63,255)
'   	 for hondal=1 to int(rnd*25/3)+1*(25/3)
'                      hondax=rnd*64
'                       honday=rnd*64
 '                      screenlock
 '                      circle(topxxx+32-hondax,topyyy+32-honday),rnd*256/4*1.5*(topzzz*5),rgb(255,(255*rnd),0),,,,F
  '                             screenunlock
'judgedbottom=timer+1

  '                 screencopy
  '                 flip
                  ' screenunlock
   '                sleep 0,1
    '               next hondal
  ' circle(bottomxxx,bottomyyy+32),64,rgb(255,192,0),,,,F
  '      screenunlock    
     ' BOTTOMgun(tempgunx2,tempguny2,1)=0':bottomfired=bottomfired-1
     '           BOTTOMgun3(tempgunx2,tempguny2)=0':bottomfired=bottomfired-1
'BOTTOMgunx(calca)=0
'BOTTOMguny(calca)=0
'astrox(astrocnt)=0
'astroy(astrocnt)=0
'int(astroz(astrocnt)*5*12)=0
'astroz(astrocnt)=365
'cup9bottom=cup9bottom+5
'                 energytop=energytop-3
'                if energytop<1 then
'                    judgedtop=timer+3
 '                   energytop=100
 '                   livestop=livestop-1
 '                   END IF
  '                  if livestop<1 then livestop=0
 
   End If
 ' FSOUND_Stream_Close(stream5)
         '      FSOUND_Stream_Close(stream3)

  '       stream=FSOUND_Stream_Open(SOUND_FILE5, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
      '  FSOUND_Stream_Stop(stream5)
      '  if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream5)
'ELSEif (buttons2=16 or buttons2=32) and topfired>255*4.97 AND TIMER>TOPTIME THEN
'TOPTIME=TIMER+16
'END If
        ' stream=FSOUND_Stream_Open(SOUND_FILE5, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep

    ' for ranny=256 to 1 step -1
    '  FSOUND_Stream_Close(stream5)
         '      FSOUND_Stream_Close(stream3)

        ' stream=FSOUND_Stream_Open(SOUND_FILE5, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep

'FSOUND_Stream_Stop(stream5)
'        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream5)
'ElseIf buttons2=32 Then
'		Line(bottomxxx,0)-(bottomxxx,bottomyyy+16),RGB(127,63,255)
'		 FSOUND_Stream_Stop(stream5)
'        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream5)
ELSEif (buttons=16 or buttons=32) AND BOTTOMFIRED>255*4.95 AND TIMER>BOTTOMTIME THEN
BOTTOMTIME=TIMER+16
    END IF
end if 
if game2=3 or game2=5 or game2=10 then timered2=timer+60
    if (livestop<1 or livesbottom<1) OR timer>timered2 or (CUP9TOP>99975 OR CUP9BOTTOM>99975) and (game2=1 or game2=12) then
'    FMUSIC_StopAllSongs

    'paint(1,1),violet,rgb(255,3,3)
    'screencopy
    game3=game2 
   'mutexlock mutex
    game=2
   ' game2=2
   ''mutexunlock mutex
    dim as SINGLE tmpgunx,tmpguny
    dim as integer tempgunz
    for tmpgunx=1 to 1280
    for tmpguny=1 to 720
    for tempgunz=1 to 4
    topgun(tmpgunx,tmpguny,tempgunz)=0
                bottomgun(tmpgunx,tmpguny,tempgunz)=0
next
    next tmpguny
next tmpgunx
buttons=0
buttons2=0
   pressed=0
   dim as Integer answer2=0
   answer2=0
   for scores=1 to 10' step -1
       if cup9top>starscore(scores) and cup9top>cup9bottom and answer2=0 then 
       'or ) then
           
   ' if cup9top>cup9bottom then
        screenset 1,0:cls:color rgb(255,255,0),rgb(0,0,0)
         FOR SCORE=9 to scores STEP -1
            STARNAME(SCORE)=STARNAME(SCORE+1)
            STARSCORE(SCORE)=STARSCORE(SCORE+1)
            NEXT SCORE
        if game2=12 or joy1=0 then 
            starname(scores)="C.P.U."
        else
    '    print
    '    print
    '    print"You have won a ";scores;" place position top screen player person"
        ' do
         
 '          screencopy 
            'screenset 0,0
            dim as integer th,th2=65,th3
           ' dim as integer choice1=65',choice2
            dim initials(3) as string
            
          ' for th=1 to 3:initials(th)="":next th
            th=1
            do
                cls
             drawdraw(1,1,"You won "+str(scores)+" place top player",rgb(0,255,0),3)
'screencopy
       
           ' print
           ' print
           ' print"Enter your name top screen player position"
             drawdraw(1,25,"Enter only your initials top screen player position",rgb(255,0,0),3)
  drawdraw(1,720/2+96,"Use left joystick up/down pick letter then hit A (1) Select D (4) Deselect START to Finish",rgb(0,255,255),2)             
            for th3=1 to th
                if th3<th then
                                 drawdraw(1280/2-(8*8*6/2)+(64*th3),720/2-(8*8/2),initials(th3),rgb(255,255,255),8)
   else
             line(1280/2-(8*8*6/2)+(64*th3),720/2-(8*8/2)+64)-(1280/2-(8*8*5/2-64)+(64*th)+64,720/2-(8*8/2)),rgb(0,0,0),BF   
                drawdraw(1280/2-(8*8*6/2)+(64*th3),720/2-(8*8/2),chr(th2),rgb(255,0,0),8)
       end if
       next 
            line(1280/2-(8*8*6/2)+(64*th),720/2-(8*8/2)+67)-(1280/2-(8*8*5/2-64)+(64*th)+64,720/2-(8*8/2)+67),rgb(255,255,255)
                sleep (25)
    DIM DULL2 AS SINGLE
    GetJoystick(0,buttons2,xxx2,yyy2,DULL2,DULL2)
'GetJoystick(1,buttons2,xxx2,yyy2,DULL2,DULL2)
    k=inkey
   ' if th>3 then buttons2=512
'if choice=choice2 and (buttons=1 or buttons2=1) then buttons=0:buttons2=0:k="":k2="":choice2=0
'if th=4 then th=3
if buttons2=1 and th<4 then
   initials(th)=chr(th2)
       if th=3 then 
           buttons2=512
           elseif th<3 then 
       th=th+1
   end if
   sleep (250)
end if
if buttons2=8 and th>1 and th<4 then
    th=th-1
    initials(th)=""
    sleep (250)
    end if
    if (yyy2<-.75 and yyy2>-1.5) then 
        
        th2=th2+1
if th2>90 then th2=65
        
        sleep (int(250/2))
        end if
if (yyy2>.75 and yyy2<1.5) then 
    'singletopz=singletopz-singled
  ' a(th
 th2=th2-1
        if th2<65 then th2=90 
  sleep (int(250/2))
  

end if
screencopy
sleep (75)
        loop until buttons2=512
        
            starname(scores)=""
            
           for th=1 to 3:starname(scores)=starname(scores)+initials(th)+".":next
           ' line input starname(scores)
            end if
            starscore(scores)=cup9top'+(128*5-cup9BOTTOM)
         starname(scores)=starname(scores)+" TOP SCREEN "
         screenset 1,0:cls
         
         
          'dim score as integer
    kill("scores.dat"):sleep (1000)
             open "scores.dat" for OUTPUT as #1

    for score=1 to 10
        print #1,starname(score)
        print #1,starscore(score)
    next score
    for score=1 to 10
        print #1,pongname(score)
        print #1,pongscore(score)
    next score
    print #1,timeflag
    print #1,tiesaucer:print #1,vol255:print #1,muting
    print #1,fps:Print #1,fullscreentoggle
    close #1
         'exit for
         answer2=1
elseif cup9bottom>starscore(scores) and cup9bottom>cup9top and answer2=0 then
    screenset 1,0:cls:color rgb(255,255,0),rgb(0,0,0)

screencopy
       FOR SCORE=9 to scores STEP -1
            STARNAME(SCORE)=STARNAME(SCORE+1)
            STARSCORE(SCORE)=STARSCORE(SCORE+1)
            NEXT SCORE
        if game2=12 or joy2=0 then 
            starname(scores)="C.P.U."
        else
           
            'screenset 0,0
            dim as integer th,th2=65,th3
           ' dim as integer choice1=65',choice2
            dim initials(3) as string
            
           ' for th=1 to 3:initials(th)=" ":next th
            th=1
            
            do
                cls
                                   drawdraw(1,1,"You won "+str(scores)+" place bottom player",rgb(0,255,0),3)
                 drawdraw(1,25,"Enter only your initials bottom screen player position",rgb(255,0,0),3)
             drawdraw(1,720/2+96,"Use left joystick up/down pick letter then hit A (1) Select D (4) Deselect START to Finish",rgb(0,255,255),2)          
for th3=1 to th
                if th3<th then
                                 drawdraw(1280/2-(8*8*6/2)+(64*th3),720/2-(8*8/2),initials(th3),rgb(255,255,255),8)
   else
             line(1280/2-(8*8*6/2)+(64*th3),720/2-(8*8/2)+64)-(1280/2-(8*8*5/2-64)+(64*th)+64,720/2-(8*8/2)),rgb(0,0,0),BF   
                drawdraw(1280/2-(8*8*6/2)+(64*th3),720/2-(8*8/2),chr(th2),rgb(255,0,0),8)
       end if
       next 
            line(1280/2-(8*8*6/2)+(64*th),720/2-(8*8/2)+67)-(1280/2-(8*8*5/2-64)+(64*th)+64,720/2-(8*8/2)+67),rgb(255,255,255)
                sleep (25)
    DIM DULL2 AS SINGLE
    GetJoystick(0,buttons2,xxx2,yyy2,DULL2,DULL2)
'GetJoystick(1,buttons2,xxx2,yyy2,DULL2,DULL2)
    k=inkey
   ' if th>3 then buttons2=512
'if choice=choice2 and (buttons=1 or buttons2=1) then buttons=0:buttons2=0:k="":k2="":choice2=0
if th=4 then th=3
if buttons2=1 and th<4 then
   initials(th)=chr(th2)
       if th=3 then 
           buttons2=512
           elseif th<3 then 
       th=th+1
   end if
   sleep (250)
end if
if buttons2=8 and th>1 and th<4 then
    th=th-1
    initials(th)=""
    sleep (250)
    end if
    if (yyy2<-.75 and yyy2>-1.5) then 
        
        th2=th2+1
if th2>90 then th2=65
        
        sleep (int(250/2))
        end if
if (yyy2>.75 and yyy2<1.5) then 
    'singletopz=singletopz-singled
  ' a(th
 th2=th2-1
        if th2<65 then th2=90 
  sleep (int(250/2))
  

end if
screencopy
sleep (75)
        loop until buttons2=512
       ' dim scores7 as integer
       '  for scores7=9 to scores
       '      starscore(scores7)=starscore(scores7+1)
       '      starname(scores7)=starname(scores7+1)
       '  next scores7
            starname(scores)=""
            
           for th=1 to 3:starname(scores)=starname(scores)+initials(th)+".":next
           ' line input starname(scores)
            end if
            starscore(scores)=cup9bottom
         starname(scores)=starname(scores)+" BOTTOM SCREEN "
        cls
          'dim score as integer
    kill("scores.dat"):sleep (1000)
             open "scores.dat" for OUTPUT as #1

    for score=1 to 10
        print #1,starname(score)
        print #1,starscore(score)
    next score
    for score=1 to 10
        print #1,pongname(score)
        print #1,pongscore(score)
    next score
    print #1,timeflag
    print #1,tiesaucer:print #1,vol255:print #1,muting
    print #1,fps:Print #1,fullscreentoggle
    close #1
        ' exit for
        answer2=1
         end if
'    end if
    next
         'if fileexists
    
   ' sleep 2000,1
            timered2=timer+60

        sleep (1500)
        'end if

end if
'END IF
'For a = 0 To 26
        'If (buttons And (1 Shl a)) Then
            'Print "Button ";a;" pressed.    "
        'Else
        if   (game2=1 or game2=12) then
'bottomyyy=720-64:topyyy=64+16
'bulletflag=0
'dim as SINGLE tpx=topxxx,tpy=topyyy,btx=bottomxxx,bty=bottomyyy
'topxxx=(tpx)'+((cos(ship1rotatex)*1)-(sin(ship1rotatex)*1))
'topyyy=(tpy)
'if singletopx>0 then
'bottomxxx=btx'-(btx*2*singletopx)
'end if'*(singletopx-(singletopx*2)))
'bottomyyy=bty+64' x = x'
'if singletopx>0 then
'bottomxxx=bottomxxx-512
'elseif singletopx<0 then
' bottomxxx=bottomxxx+64'-cos(-pieyedi)*4
'  end if 
'+((sin(ship1rotatey)*1)+(cos(ship1rotatey)*1))
'dim rhino as SINGLE
'rhino=rnd
'if rhino>5 then bottomxxx=bottomxxx+9 else bottomxxx=bottomxxx-9:end if
'rhino=rnd

'if rhino>5 then topxxx=topxxx+9 else topxxx=topxxx-9:end if
'rhino=rnd
'if rhino>5 then bottomyyy=bottomyyy+9 else bottomyyy=bottomyyy-9:end if
'rhino=rnd

'if rhino>5 then topyyy=topyyy+9 else topyyy=topyyy-9:end if

'bottomxxx=btx
'bottomyyy=+32' x = x'
'y = (sin(angle) * z') + (cos(angle) * y')
'z = (cos(angle) * z') - (sin(angle) * y')

'The second set of algorithms is for rotation about the y axis:

'x = (cos(angle) * x') - (sin(angle) * z')
'y = y'
'z = (sin(angle) * x') + (cos(angle) * z')
'screenlock

'line(topxxx,topyyy)-(topxxx-32,topyyy-32),rgb(255,251,0)
'line(topxxx-32,topyyy-32)-(topxxx+32,topyyy-32),rgb(255,251,0)
'line(topxxx+32,topyyy-32)-(topxxx,topyyy),rgb(255,251,0)
'paint(topxxx,topyyy),rgb(255,251,0),rgb(255,251,0)
'line(topxxx-1,topyyy-24)-(topxxx+1,topyyy),rgb(0,0,0),bf
'for crightx=1 to 7'3 to 1 step -1
'    line(topxxx-cleftx,topyyy-24-cleftx)-(topxxx+cleftx,topyyy-24-cleftx),rgb(0,0,255)
'next cleftx
'pset(topxxx,topyyy-24),rgb(0,0,0)
'for cleftx=1 to int(rnd * 9)+1 'to 1 step -1
'      line(topxxx-(cleftx),topyyy-33-(9-cleftx))-(topxxx+(cleftx),topyyy-33-(9-cleftx)),rgb(255,(255)-255*(cleftx*100/9/100),0)
'  next cleftx
'line(bottomxxx,bottomyyy)-(bottomxxx-32,bottomyyy+32),rgb(255,251,0)
'line(bottomxxx-32,bottomyyy+32)-(bottomxxx+32,bottomyyy+32),rgb(255,251,0)
'line(bottomxxx+32,bottomyyy+32)-(bottomxxx,bottomyyy),rgb(255,251,0)
'paint(topxxx,topyyy),rgb(0,0,255),rgb(255,251,0)
'paint(bottomxxx,bottomyyy+16),rgb(255,251,0),rgb(255,251,0)
'line(bottomxxx-1,bottomyyy+24)-(bottomxxx+1,bottomyyy),rgb(0,0,0),bf
'for cleftx=1 to 7'3 to 1 step -1
'    line(bottomxxx-cleftx,bottomyyy+24+cleftx)-(bottomxxx+cleftx,bottomyyy+24+cleftx),rgb(0,0,255)
'next cleftx
'pset(bottomxxx,bottomyyy-24),rgb(0,0,0)
'for cleftx=1 to int(rnd * 9)+1 'to 1 step -1
'      line(bottomxxx-(cleftx),bottomyyy+33+(9-cleftx))-(bottomxxx+(cleftx),bottomyyy+33+(9-cleftx)),rgb(255,(255)-255*(cleftx*100/9/100),0)
'  next cleftx
'  screenunlock
'  topxxx=tpx
'  topyyy=tpy
'  bottomxxx=btx
'  bottomyyy=bty
 ' end if
'if spin>0 then
'    spin=0
'goto jumpxy

'teyan=teyan-1
'if teyan<1 then teyan=6
'if tiesaucer=1 then teyan=6
'for tempgunx=1 to 1280 step 7-teyan'int(rnd*2)+1
'    for tempguny=1 to 720 step 7-teyan'int(rnd*2)+1'int(rnd*4)+1'int(rnd(1)*2)+1
' for calca=(720/2) to 1 step -1

'   topgunx(calca)=topgunx(calca+1)
'    topguny(calca)=topguny(calca+1)
'    bottomgunx(calca)=bottomgunx(calca+1)
'    bottomguny(calca)=bottomguny(calca+1)
'    next
If TOPFIRED>BOTTOMFIRED THEN 
    TRAINERS=TOPFIRED
Else
    TRAINERS=BOTTOMFIRED
END IF
for calca=1 to TRAINERS'(255*5)
    tempgunx=topgunx(calca)
    tempguny=topguny(calca)
    tempgunx2=bottomgunx(calca)
    tempguny2=bottomguny(calca)
   
'for tempgunz=1 to 4
'        for tempgunz=-4 to 4' step .001
bottomgunz=0
        bottomgunz2=0
        bottomgunz3=0
        bottomgunz4=0
  '      goto jumpxy
        for topgunz=1 to 3 step +1'teyan'20'9-1/2
        if topgun(tempgunx-3+(topgunz),tempguny-3+(topgunz),1)<>0 then 
            bottomgunz=bottomgunz+1
              end if
              if bottomgun(tempgunx2-3+(topgunz),tempguny2-3+(topgunz),1)<>0 then 
                  bottomgunz2=bottomgunz2+1
                  
end if
if bottomgunz=1 and bottomgunz2=1 then
    if topgun3(tempgunx-3+(topgunz),tempguny-3+(topgunz))=bottomgun3(tempgunx2-3+(topgunz),tempguny2-3+(topgunz)) then
        blowup
    elseif topzzz>bottomzzz and topgun3(tempgunx-3+(topgunz),tempguny-3+(topgunz))-1=bottomgun3(tempgunx2-3+(topgunz),tempguny2-3+(topgunz)) then    
blowup
elseif bottomzzz>topzzz and topgun3(tempgunx-3+(topgunz),tempguny-3+(topgunz))+1=bottomgun3(tempgunx-3+(topgunz),tempguny-3+(topgunz)) then
blowup
end if
end if

'if topgun3(tempgunx-3+(topgunz),tempguny+3-(topgunz))<>0 then 
'            bottomgunz3=bottomgunz3+1
'              end if
          '    if bottomgun3(tempgunx-3+(topgunz),tempguny-3+(topgunz))=topgun3(tempgunx-3+topgunz,tempguny-3+topgunz) then 
           '       bottomgunz4=bottomgunz4+1
            '      end if
      '          if bottomgunz>0 and bottomgunz2>0 then 
      '             blowup'exit for
                   
      '              end if
next topgunz
'next tempgunz
'goto enderbender
'enderbender:
'        next tempgunz

'next tempguny
'next tempgunx
jumpxy:
tempgunz=1

'dim zzzz as SINGLE'integer
'texan=texan+1
'if texan=3 then texan=1

 

'for tempgunx=1 to 1280 step +1'texan 'step int(rnd*2)+1
'    if tempgunx>1280 then tempgunx=1
'    if tempguny>720 then tempguny=1
'IF TEMPGUNY=0 THEN TEMPGUNY=1:GOTO JUMPY
'TEMPGUNY=TEMPGUNY+int(rnd*2)+1
'IF TEMPGUNY>720 THEN TEMPGUNY=1
'['IF TEMPXGUN=1 THEN TEMPGUNX=1'INT(RND*2)+1

'JUMPY:
'DECIDE=INT(2*RND)+1
'IF DECIDE=0 THEN DECIDE=1

'TEXAN=INT(2*RND)+1'
'IF TEXAN<2 THEN 
'TEMPGUNX=TEMPGUNX+1
'ELSEIF DECIDE=1 THEN
'    TEMPGUNX=TEMPGUNX+INT(2*RND)+1
'END IF


'INT(RND*2)+1
'IF TEMPXGUN=1 THEN TEMPGUNX=1'INT(RND*2)+1
 '   for tempguny=1 to 720 step teyan
        
      '  IF TEMPYGUN=1 THEN TEMPGUNY=0'INT(RND*2)+1

'JUMPY:
'TEYAN=INT(2*RND)+1
'IF TEYAN<2 THEN 
'TEMPGUNY=TEMPGUNY+1
'ELSEIF DECIDE=2 THEN
'    TEMPGUNY=TEMPGUNY+INT(2*RND)+1
'END IF

        
        
        
 '       TEMPGUNY=TEMPGUNY+1'INT(RND*2)+1
 '       IF TEMPGUNY>720 THEN TEMPGUNY=1

       ' for tempgunz=-4 to 4'(100/25)' step 25
      'for tempgunz=1 to 4
     ' zzzz=1 
      tempgunz=1
     ' screenlock
   ' if tempgunz<0 then zzzz=tempgunz-(tempgunz*2) else zzzz=tempgunz:end if
'    zzzz=zzzz*100/4/100
        'and bottomgun(tempgunx,tempguny)<>0
       ' if topgun3(tempgunx,tempguny,tempgunz)<0 then
'zzzz=topgun3(tempgunx,tempguny,tempgunz)+8
'end if
'elseif topgun3(tempgunx,tempguny,tempgunz)>0 then
'end if
'if zzzz<0 then zzzz=zzzz-(zzzz*2)
'zzzz=zzzz*100/4/100
'if zzzz=0 or zzzz>1 then zzzz=1
dim as integer bottomzzz2,topzzz2
tempgunz=1
bottomzzz2=int(bottomzzz*5*12)
topzzz2=int(topzzz*5*12)
if topgun(tempgunx,tempguny,tempgunz)>0 and topgun3(tempgunx,tempguny)>=bottomzzz2 then 
    zzzz=(topgun3(tempgunx,tempguny))/12*3.343'*25/100'/4
screenlock
    line(tempgunx+((5)*zzzz*1),tempguny)-(tempgunx-((5)*zzzz*1),tempguny),rgb(255,7,7)
        circle(tempgunx,tempguny),2*zzzz,rgb(255,128,0),,,,F

    circle(tempgunx,tempguny),1*zzzz,rgb(255,255,251),,,,F
screenunlock
'if topgun(tempgunx,tempguny,tempgunz)=1 then circle(tempgunx,tempguny),int(4*(zzzz)),rgb(255,7,7),,,,f
end if
if bottomgun(tempgunx2,tempguny2,tempgunz)>0 and bottomgun3(tempgunx2,tempguny2)>=topzzz2 then 
                   zzzz=(bottomgun3(tempgunx2,tempguny2))/12*3.343'*25/100'*25/100'/4
screenlock
    line(tempgunx2+((5)*zzzz*1),tempguny2)-(tempgunx2-((5)*zzzz*1),tempguny2),rgb(255,7,7)
        circle(tempgunx2,tempguny2),2*zzzz,rgb(255,128,0),,,,F

    circle(tempgunx2,tempguny2),1*zzzz,rgb(255,255,251),,,,F

                 '  circle(tempgunx2,tempguny2),1+((4)*zzzz*1),rgb(255,7,7),,,,F
screenunlock
'if topgun(tempgunx,tempguny)>3 then line(tempgunx-3,tempguny-3)-(tempgunx+3,tempguny+3),rgb(255,127,0),bf
'                if bottomgun(tempgunx,tempguny)>2 then line(tempgunx-3,tempguny-3)-(tempgunx+3,tempguny+3),rgb(255,127,0),bf
end if









'elseif topgun3(tempgunx,tempguny,tempgunz)=0 then
       'circle(tempgunx,tempguny),3.5,rgb(255,7,7),,,,f
' end if
'dim anda as integer
'for anda=1 to 3
 

'if bottomzzz2<0 then bottomzzz2=bottomzzz2-(bottomzzz2*2)
'if topzzz2<0 then topzzz2=topzzz2-(topzzz2*2)
     IF topgun(tempgunx,tempguny,1)>0 and tempgunx>bottomxxx-32 and tempgunx<bottomxxx+32 and tempguny>bottomyyy and (topgun3(tempgunx,tempguny)=bottomzzz2) then

    ' IF JUdgedbottom<timer and topgun(tempgunx,tempguny,1)>0 and tempgunx>bottomxxx-32 and tempgunx<bottomxxx+32 and tempguny>bottomyyy and (topgun3(tempgunx,tempguny)=bottomzzz2) then
       'color ,rgb(255,0,0)
 '      topgn3=0
       'RANDOMIZE RND+TIMER+Rnd
       
       ' FSOUND_Stream_Close(stream6)
         '      FSOUND_Stream_Close(stream3)

        ' stream=FSOUND_Stream_Open(SOUND_FILE6, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
       ' FSOUND_Stream_Stop(stream)
        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream6)
       
       
        'screenlock
        'cls
               ' paint(1,1),rgb(255,0,0)',rgb(255,5,5)

 for hondal=1 to int(rnd*25)+1*(25/3)
                      hondax=rnd*64
                       honday=rnd*64
                       screenlock
                       circle(bottomxxx+32-hondax,bottomyyy+32-honday),rnd*256/3*(bottomzzz*5),rgb(255,(255*rnd),0),,,,F
                               screenunlock
'judgedbottom=timer+1

                   screencopy
                   flip
                   sleep 0,1
                   next hondal
  ' circle(bottomxxx,bottomyyy+32),64,rgb(255,192,0),,,,F
       ' screenunlock
     '   screencopy
        'color ,rgb(0,0,0)
       'LINE(BOTTOMXXX-32,BOTTOMYYY+32)-(BOTTOMXXX+32,BOTTOMYYY),rgb(255,7,7),BF',rgb(255,3,3)
                cup9top=cup9top+5
                 energybottom=energybottom-3
                if energybottom<1 then
                    judgedbottom=timer+3
                    energybottom=100
                    livesbottom=livesbottom-1
                    END IF
                    if livesbottom<1 then livesbottom=0
               ' for rannycount=1 to 12
               '         sound pulsewave(notes(rannycount)),1/16
               '         next rannycount
topgun(tempgunx,tempguny,1)=0
'topgn3=topgun3(tempgunx,tempguny)
topgun3(tempgunx,tempguny)=0
topgunx(calca)=0
topguny(calca)=0
end if
'draw string (1,8),str(bottomzzz2),rgb(255,255,255)
'draw string (1,16),str(bottomyyy),rgb(255,255,255)

'dim as integer bottomzzz2,topzzz2
'if bottomgun(tempgunx,tempguny,1)>0 and tempguny<topyyy then sleep




'if singletopz<0 and topzzz>.25/2 then topzzz=topzzz-.0125:singletopz=singletopz+1:end if 'else singletopz=singletopz+.5:end if
'if singletopz>0 and topzzz<.25 then topzzz=topzzz+.0125:singletopz=singletopz-1:end if  'else singletopz=singletopz-.5:end if
'if singlebottomz<0 and bottomzzz>.25/2 then bottomzzz=bottomzzz-.0125:singlebottomz=singlebottomz+1:end if 'else singlebottomz=singlebottomz+.5:end if
'if singlebottomz>0 and bottomzzz<.25 then bottomzzz=bottomzzz+.0125:singlebottomz=singlebottomz-1:end if 'else singlebottomz=singlebottomz-.5:end if


 if bottomgun(tempgunx2,tempguny2,1)>0 and tempgunx2>topxxx-32 and tempgunx2<topxxx+32 and tempguny2<topyyy and (bottomgun3(tempgunx2,tempguny2)=topzzz2) then

' if judgedtop<timer and bottomgun(tempgunx2,tempguny2,1)>0 and tempgunx2>topxxx-32 and tempgunx2<topxxx+32 and tempguny2<topyyy and (bottomgun3(tempgunx2,tempguny2)=topzzz2) then
      '  bottomgn3=0
        
        'RANDOMIZE RND+TIMER+Rnd
        
        
       '  FSOUND_Stream_Close(stream6)
         '      FSOUND_Stream_Close(stream3)

       '  stream=FSOUND_Stream_Open(SOUND_FILE6, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, FSOUND_MPEGACCURATE, 0, 0)

    '    FSOUND_Stream_Play(FSOUND_FREE, stream2)       'sleep
    '    FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
       ' FSOUND_Stream_Stop(stream)
        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream6)
       ' screenlock
       ' cls
for hondal=1 to int(rnd*25)+1*(25/3)
                      hondax=rnd*64
                       honday=rnd*64
                       screenlock
                       circle(topxxx+32-hondax,topyyy-32+32-honday),rnd*256/3*(topzzz*5),rgb(255,(255*rnd),0),,,,F
                               screenunlock
'judgedtop=timer+1

                   screencopy
                   flip
                   sleep 0,1
                   next hondal ' paint(1,1),rgb(255,0,0)
  'circle(topxxx,topyyy-32),64,rgb(255,192,0),,,,F
       ' screenunlock
    '    screencopy
        'LINE(TOPXXX-32,TOPYYY-32)-(TOPXXX+32,TOPYYY),rgb(255,7,7),BF',rgb(255,3,3)
                cup9bottom=cup9bottom+5
                energytop=energytop-3
                if energytop<1 then
                    judgedtop=timer+3
                    energytop=100
                    livestop=livestop-1
                    END IF
                    if livestop<1 then livestop=0
                    
                
             ' cup9top=cup9top+1
              '  for rannycount=12 to 1 step -1' to 12
              '          sound pulsewave(notes(rannycount)),1/16'1000/360
'next rannycount
bottomgun(tempgunx2,tempguny2,1)=0
'bottomgn3=bottomgun3(tempgunx,tempguny)
bottomgun3(tempgunx2,tempguny2)=0
bottomgunx(calca)=0
bottomguny(calca)=0
end if
  
    
    
    'rgb(255,127,0),bf
'if bottomgun3(tempgunx,tempguny+7,tempgunz)<0 then
'zzzz=bottomgun3(tempgunx,tempguny+7,tempgunz)+8
'end if
'elseif bottomgun3(tempgunx,tempguny+7,tempgunz)>0 then
'end if
'if zzzz<0 then zzzz=zzzz-(zzzz*2)
'zzzz=zzzz*100/4/100
'if zzzz=0 or zzzz>1 then zzzz=1
               
  '  screenunlock
    dim as integer tmpgnx,tmpgny
    tmpgnx=tempgunx
    tmpgny=tempguny
     if bottomgun(tempgunx2,tempguny2,tempgunz)>0 and tempguny2<=(24)*1.5 then
        bottomgun(tempgunx2,tempguny2,tempgunz)=0':bottomfired=bottomfired-1
                bottomgun3(tempgunx2,tempguny2)=0':bottomfired=bottomfired-1
topgunx(calca)=0
topguny(calca)=0
'bottomfired=bottomfired-1
'bottomfired=0
        end if
       
    if bottomgun(tempgunx2,tempguny2,tempgunz)>0 and bottomgun(tempgunx2,tempguny2,tempgunz)<2 then
        bottomgun(tempgunx2,tempguny2,tempgunz)=bottomgun(tempgunx2,tempguny2,tempgunz)+1':topgun(tempgunx,tempguny)=0
    end if
 if bottomgun(tempgunx2,tempguny2,1)>0 and tempguny2>(24)*1.5 then
    ' if bottomgun(tempgunx,tempguny,tempgunz)>0
    bottomguny(calca)=bottomguny(calca)-32
    
    
    
          'If bottomxxx<tempgunx2 Then
    	
    'tempgunx2=tempgunx2-12
                bottomgun(tempgunx2,tempguny2-32,1)=1:bottomgun(tempgunx2,tempguny2,1)=0
                bottomgun3(tempgunx2,tempguny2-32)=bottomgun3(tempgunx2,tempguny2)
               ' bottomgn3=tempgunx
               bottomgun3(tempgunx,tempguny)=0
             '  bottomgunx(calca)=tempgunx2
               
          '.ElseIf bottomxxx>tempgunx2 Then
   ' 	tempgunx2=tempgunx2+12
    '	 bottomgun(tempgunx2,tempguny2-32,1)=1:bottomgun(tempgunx2,tempguny2,1)=0
    '            bottomgun3(tempgunx2,tempguny2-32)=bottomgun3(tempgunx2,tempguny2)
    '            bottomgun3(tempgunx2,tempguny2)=0
      '    elseIf bottomxxx>tempgunx2-48 Then
      '    	tempgunx2=tempgunx2-49
      '       bottomgun(tempgunx2,tempguny2-32,1)=1:bottomgun(tempgunx2,tempguny2,1)=0
      '          bottomgun3(tempgunx2,tempguny2-32)=bottomgun3(tempgunx2,tempguny2)
               ' bottomgn3=tempgunx
       '        bottomgun3(tempgunx,tempguny)=0
        '  ElseIf bottomxxx<tempgunx2+48 Then
    	   	'tempgunx2=tempgunx2+49
    	' bottomgun(tempgunx2,tempguny2-32,1)=1:bottomgun(tempgunx2-49,tempguny2,1)=0
      '          bottomgun3(tempgunx2,tempguny2-32)=bottomgun3(tempgunx2-49,tempguny2)
      '          bottomgun3(tempgunx2-49,tempguny2)=0
   ' EndIf
   ' End If
             
         '       bottomgn3=tempgunx
'bottomgun3(tempgunx2+4,tempguny2)=0
'end if
 bottomgunx(calca)=tempgunx2
 'topgn3=tempgunx'

    '  if topzzz2>bottomgun3(tempgunx2,tempguny2-(24)) and bottomgun3(tempgunx2,tempguny2-(24))<.20 then
          
    '                                bottomgun3(tempgunx2,tempguny2-(24))=bottomgun3(tempgunx2,tempguny2-(24))+.0245
    '                            elseif topzzz2<bottomgun3(tempgunx2,tempguny2-(24)) and bottomgun3(tempgunx2,tempguny2-(24))>.20/5 then
    '                                bottomgun3(tempgunx2,tempguny2-(24))=bottomgun3(tempgunx2,tempguny2-(24))-.0245
    '                                end if
    '            bottomgun(tempgunx2+24,tempguny2-(24),1)=1:bottomgun(tempgunx2,tempguny2-(24),1)=0
    '                                            bottomgun3(tempgunx2+24,tempguny2-(24))=bottomgun3(tempgunx2,tempguny2-(24))
'bottomgunx(calca)=bottomgunx(calca)+24

                
                '                bottomgn3=tempgunx',tempguny-(24))
               
'bottomgun3(tempgunx2,tempguny2-(24))=0
'elseif bottomgun(tempgunx2,tempguny2,1)>0 and tempgunx2>topxxx and tempguny2>(24)*1.5 then
'    bottomguny(calca)=bottomguny(calca)-24
    
    ' if bottomgun(tempgunx,tempguny,1)>0 
'                bottomgun(tempgunx2,tempguny2-(24),1)=1:bottomgun(tempgunx2,tempguny2,1)=0
'                bottomgun3(tempgunx2,tempguny2-(24))=bottomgun3(tempgunx2,tempguny2)
               ' bottomgn3=tempgunx
'bottomgun3(tempgunx2,tempguny2)=0
'end if
'    if topzzz2>bottomgun3(tempgunx2,tempguny2-(24)) and bottomgun3(tempgunx2,tempguny2-(24))<.20 then
'                                    bottomgun3(tempgunx2,tempguny2-(24))=bottomgun3(tempgunx2,tempguny2-(24))+.0245
'                                elseif topzzz2<bottomgun3(tempgunx2,tempguny2-(24)) and bottomgun3(tempgunx2,tempguny2-(24))>.20/5 then
'                                    bottomgun3(tempgunx2,tempguny2-(24))=bottomgun3(tempgunx2,tempguny2-(24))-.0245
'                                    end if
'   bottomgun(tempgunx2-24,tempguny2-(24),1)=1:bottomgun(tempgunx2,tempguny2-(24),1)=0
'                                   bottomgun3(tempgunx2-24,tempguny2-(24))=bottomgun3(tempgunx2,tempguny2-(24))
'bottomgunx(calca)=bottomgunx(calca)-24
    
                '                topgn3=tempgunx',tempguny)
               
'bottomgun3(tempgunx,tempguny-(24))=0
  
  
  
  end if
  
   
     if topgun(tempgunx,tempguny,1)>0 and tempguny>=720-((24)*1.5) then
        
        topgun(tempgunx,tempguny,1)=0':topfired=topfired-1
                topgun3(tempgunx,tempguny)=0':topfired=topfired-1
topgunx(calca)=0
topguny(calca)=0

        end if
    if topgun(tempgunx,tempguny,tempgunz)>0 and topgun(tempgunx,tempguny,tempgunz)<2 then
        topgun(tempgunx,tempguny,tempgunz)=topgun(tempgunx,tempguny,tempgunz)+1':topgun(tempgunx,tempguny)=0
    end if

if topgun(tempgunx,tempguny,1)>0  and tempguny<720-((24)*1.5) then
  '  if topgun(tempgunx,tempguny,1)>0
 
  topguny(calca)=topguny(calca)+32  
  
'  If topxxx<tempgunx-48 Then
  '	tempgunx=tempgunx-12
  '	tempguny=tempguny-32
 ' EndIf
                topgun(tempgunx,tempguny+32,1)=1:topgun(tempgunx,tempguny,1)=0
                                topgun3(tempgunx,tempguny+32)=topgun3(tempgunx,tempguny)
  topgun3(tempgunx,tempguny)=0
 
'  ElseIf topxxx>tempgunx+48 Then
  '	tempgunx=tempgunx+12
'                 topgun(tempgunx,tempguny+32,1)=1:topgun(tempgunx,tempguny,1)=0
'                                topgun3(tempgunx,tempguny+32)=topgun3(tempgunx,tempguny)
'  topgun3(tempgunx,tempguny)=0
'  End If
  
 ' topgunx(calca)=topgunx
 ' elseIf topxxx>tempgunx2-48 Then
 '         	tempgunx2=tempgunx2-49
 '            topgun(tempgunx2,tempguny2+32,1)=1:topgun(tempgunx2+49,tempguny2,1)=0
 '               topgun3(tempgunx2,tempguny2+32)=topgun3(tempgunx2+49,tempguny2)
               ' topgn3=tempgunx
 '              topgun3(tempgunx+49,tempguny)=0
 ' ElseIf topxxx<tempgunx+48 Then
 '   	   	tempgunx=tempgunx+49
 '   	 topgun(tempgunx,tempguny+32,1)=1:topgun(tempgunx-49,tempguny,1)=0
  '              topgun3(tempgunx,tempguny+32)=topgun3(tempgunx-49,tempguny)
  '              topgun3(tempgunx-49,tempguny)=0
   ' EndIf
  'End If
                
       topgunx(calca)=tempgunx          
                
                
                
        '                topgn3=tempgunx',tempguny)

  '  end if
'     if bottomzzz2>topgun3(tempgunx,tempguny+(24)) and topgun3(tempgunx,tempguny+(24))<.20 then
'                                    topgun3(tempgunx,tempguny+(24))=topgun3(tempgunx,tempguny+(24))+.0125
'                                elseif bottomzzz2<topgun3(tempgunx,tempguny+(24)) and topgun3(tempgunx,tempguny+(24))>.20/5 then
'                                    topgun3(tempgunx,tempguny+(24))=topgun3(tempgunx,tempguny+(24))-.0125
'                                    end if
'                               
'                topgun(tempgunx+24,tempguny+(24),1)=1:topgun(tempgunx,tempguny+(24),1)=0
'                                                topgun3(tempgunx+24,tempguny+(24))=topgun3(tempgunx,tempguny+(24))
'
'                topgunx(calca)=topgunx(calca)+24
                '                topgn3=tempgunx',tempguny+(24))
'topgun3(tempgunx,tempguny+(24))=0
'elseif topgun(tempgunx,tempguny,1)>0 and tempgunx>bottomxxx and tempguny<720-((24)*1.5) then
'    topguny(calca)=topguny(calca)+24
  '  if topgun(tempgunx,tempguny,1)>0 
'                topgun(tempgunx,tempguny+(24),1)=1:topgun(tempgunx,tempguny,1)=0
'                                topgun3(tempgunx,tempguny+(24))=topgun3(tempgunx,tempguny)
                '                topgn3=tempgunx',tempguny)
                
'topgun3(tempgunx,tempguny)=0
  '  end if
'     if bottomzzz2>topgun3(tempgunx,tempguny+(24)) and topgun3(tempgunx,tempguny+(24))<.20 then
'                                    topgun3(tempgunx,tempguny+(24))=topgun3(tempgunx,tempguny+(24))+.0125
'                                elseif bottomzzz2<topgun3(tempgunx,tempguny+(24)) and topgun3(tempgunx,tempguny+(24))>.20/5 then
'                                    topgun3(tempgunx,tempguny+(24))=topgun3(tempgunx,tempguny+(24))-.0125
'                                    end if
'   topgun(tempgunx-24,tempguny+(24),1)=1:topgun(tempgunx,tempguny+(24),1)=0
'                                   topgun3(tempgunx-24,tempguny+(24))=topgun3(tempgunx,tempguny+(24))
'
'  
'                '                topgn3=tempgunx',tempguny+(24))
'    topgunx(calca)=topgunx(calca)-24            
'topgun3(tempgunx,tempguny+(24))=0
'  
  
  
  end if
   '     topgun(tempgunx,tempguny,1)=topgun(tempgunx,tempguny,1)+1':topgun(tempgunx,tempguny,1)=0
 


   
    
   
   
  

 
dim tempgunz2 as integer
dim temprgb as ulongint
'for tempgunz2=-4 to 4

'END IF
'if bottomgun3(tempgunx,tempguny)=topzzz2 and bottomgun(tempgunx,tempguny,1)<>0 and tempguny>topyyy and  tempgunx>topxxx-32 and tempgunx<topxxx+32 and bottomgun3(tempgunx,tempguny)<.25/2 and topzzz<.25 then singletopz=singletopz+1
'if bottomgun3(tempgunx,tempguny)=topzzz2 and bottomgun(tempgunx,tempguny,1)<>0 and tempguny>topyyy and  tempgunx>topxxx-32 and tempgunx<topxxx+32 and bottomgun3(tempgunx,tempguny)>.25/2 and topzzz>.25/2 then singletopz=singletopz-1
'end if
'if joy2<2 or game2=12 then

'if topgun3(tempgunx,tempguny)=bottomzzz2 and topgun(tempgunx,tempguny,1)<>0 and tempguny<bottomyyy and tempgunx>bottomxxx-32 and tempgunx<bottomxxx+32 and topgun3(tempgunx,tempguny)>.25/2 and bottomzzz>.25/2 then singlebottomz=singlebottomz-1
'if topgun3(tempgunx,tempguny)=bottomzzz2 and topgun(tempgunx,tempguny,1)<>0 and tempguny<bottomyyy and  tempgunx>bottomxxx-32 and tempgunx<bottomxxx+32 and topgun3(tempgunx,tempguny)<.25/2 and bottomzzz<.25 then singlebottomz=singlebottomz+1
'if topgun3(tempgunx,tempguny)=bottomzzz2 and topgun(tempgunx,tempguny,1)<>0 and tempguny<bottomyyy and  tempgunx>bottomxxx-32 and tempgunx<bottomxxx+32 and topgun3(tempgunx,tempguny)<.25/2 and bottomzzz<.25 then singlebottomz=singlebottomz+1
'if topgun3(tempgunx,tempguny)=bottomzzz2 and topgun(tempgunx,tempguny,1)<>0 and tempguny<bottomyyy and  tempgunx>bottomxxx-32 and tempgunx<bottomxxx+32 and topgun3(tempgunx,tempguny)>.25/2 and bottomzzz>.25/2 then singlebottomz=singlebottomz-1
'end if
'END IF
'next calca
'next tempguny
'next tempgunx
'screenunlock
   ' draw string (1280/2-(16*8/2),720),"Seconds left:"+str(int(timered2-timer)),rgb(255,255,0)
'end if
'elseif bottomgun3(tempgunx,tempguny,tempgunz)=0 then
'       circle(tempgunx,tempguny),3.5,rgb(255,7,7),,,,f
' end if
    'dim as integer rannycount',rannychoice
'if bottomgun3(tempgunx,tempguny,tempgunz)>0 and bottomgun3(tempgunx,tempguny,tempgunz)<3 then
'        bottomgun3(tempgunx,tempguny,tempgunz)=bottomgun3(tempgunx,tempguny,tempgunz)+1':bottomgun(tempgunx,tempguny,tempgunz)=0
'    end if
'    if bottomgun3(tempgunx,tempguny,tempgunz)>2 and tempguny<=720-(24*1.5) then
'                bottomgun3(tempgunx,tempguny+24,tempgunz)=1:bottomgun(tempgunx,tempguny,tempgunz)=0
'    end if
'draw string (1,32),str(topxxx),rgb(255,255,255)
'draw string (1,32+8),str(bottomgn3),rgb(255,255,255)
'next tempgunz
'dim toy AS integer
'if toy=0 then
'if joy1<2 or game2=12 then
'for andy=1 to 6
   
   
   
   
   
   
   
   
   
   
   
 
'if topgun3(tempgunx,tempguny)=bottomzzz2 and topgun(tempgunx,tempguny,1)<>0 and tempguny<bottomyyy and tempgunx>bottomxxx-32 and tempgunx<bottomxxx+32 and topgun3(tempgunx,tempguny)>.25/2 and bottomzzz>.25/2 then singlebottomz=singlebottomz-1
'if topgun3(tempgunx,tempguny)=bottomzzz2 and topgun(tempgunx,tempguny,1)<>0 and tempguny<bottomyyy and  tempgunx>bottomxxx-32 and tempgunx<bottomxxx+32 and topgun3(tempgunx,tempguny)<.25/2 and bottomzzz<.25 then singlebottomz=singlebottomz+1
'if topgun3(tempgunx,tempguny)=bottomzzz2 and topgun(tempgunx,tempguny,1)<>0 and tempguny<bottomyyy and  tempgunx>bottomxxx-32 and tempgunx<bottomxxx+32 and topgun3(tempgunx,tempguny)<.25/2 and bottomzzz<.25 then singlebottomz=singlebottomz+1
'if topgun3(tempgunx,tempguny)=bottomzzz2 and topgun(tempgunx,tempguny,1)<>0 and tempguny<bottomyyy and  tempgunx>bottomxxx-32 and tempgunx<bottomxxx+32 and topgun3(tempgunx,tempguny)>.25/2 and bottomzzz>.25/2 then singlebottomz=singlebottomz-1
'end if
'END IF
'next tempguny
'next tempgunx
'screenunlock


   ' draw string (1280/2-(16*8/2),720),"Seconds left:"+str(int(timered2-timer)),rgb(255,255,0)
'end if
'dim as integer bottomzzz2,topzzz2
tempgunz=1
bottomzzz2=int(bottomzzz*5*12)
topzzz2=int(topzzz*5*12)

for astrocnt=3 to 1 step -1
'for astrocnt=3 to 1 STEP -1
IF astrox(ASTROCNT)>TEMPGUNX-((64)) AND astrox(ASTROCNT)<TEMPGUNX+((64)) THEN
 IF astroy(astrocnt)>TEMPGUNY-((64)) AND astroy(astrocnt)<TEMPGUNY+((64)) THEN
 IF int(astroz(astrocnt)*5*12)<tOPGUN3(TEMPGUNX,TEMPGUNY)+9 AND int(astroz(astrocnt)*5*12)>TOPGUN3(TEMPGUNX,TEMPGUNY)-9 THEN  
   
   'RANDOMIZE RND+TIMER+Rnd
 '  screenlock
        'cls
               ' paint(1,1),rgb(255,0,0)',rgb(255,5,5)

 for hondal=1 to int(rnd*25)+1*(25)
                      hondax=rnd*64
                       honday=rnd*64
                       screenlock
                       circle(astrox(astrocnt)+32-hondax,astroy(astrocnt)+32-honday),rnd*256*1.5*(astroz(astrocnt)*5),rgb(255,(255*rnd),0),,,,F
                               screenunlock
'judgedbottom=timer+1

                   screencopy
                   flip
                   sleep 0,1
                   'screenunlock
                   next hondal
  ' circle(bottomxxx,bottomyyy+32),64,rgb(255,192,0),,,,F
  '      screenunlock    
      TOPgun(tempgunx,tempguny,1)=0':bottomfired=bottomfired-1
                TOPgun3(tempgunx,tempguny)=0':bottomfired=bottomfired-1
topgunx(calca)=0
topguny(calca)=0
'astrox(astrocnt)=0
'astroy(astrocnt)=0
'int(astroz(astrocnt)*5*12)=0
astroz(astrocnt)=365
'astrox(astrocnt)=0
'astroy(astrocnt)=0
astrot(astrocnt)=0

END IF
END IF
END IF
IF astrox(ASTROCNT)>TEMPGUNX2-((64)) AND astrox(ASTROCNT)<TEMPGUNX2+((64)) THEN
 IF astroy(astrocnt)>TEMPGUNY2-((64)) AND astroy(astrocnt)<TEMPGUNY2+((64)) THEN
 IF int(astroz(astrocnt)*5*12)<BOTTOMGUN3(TEMPGUNX2,TEMPGUNY2)+9 AND int(astroz(astrocnt)*5*12)>BOTTOMGUN3(TEMPGUNX2,TEMPGUNY2)-9 THEN  

 ' screenlock
        'cls
               ' paint(1,1),rgb(255,0,0)',rgb(255,5,5)
'RANDOMIZE RND+TIMER+Rnd
 for hondal=1 to int(rnd*25)+1*(25)
                      hondax=rnd*64
                       honday=rnd*64
                       screenlock
                       circle(astrox(astrocnt)+32-hondax,astroy(astrocnt)+32-honday),rnd*256*1.5*(astroz(astrocnt)*5),rgb(255,(255*rnd),0),,,,F
                               screenunlock
'judgedbottom=timer+1

                   screencopy
                   flip
                  ' screenunlock
                   sleep 0,1
                   next hondal
  ' circle(bottomxxx,bottomyyy+32),64,rgb(255,192,0),,,,F
  '      screenunlock    
      BOTTOMgun(tempgunx2,tempguny2,1)=0':bottomfired=bottomfired-1
                BOTTOMgun3(tempgunx2,tempguny2)=0':bottomfired=bottomfired-1
BOTTOMgunx(calca)=0
BOTTOMguny(calca)=0
'astrox(astrocnt)=0
'astroy(astrocnt)=0
'int(astroz(astrocnt)*5*12)=0
astroz(astrocnt)=365
'astrox(astrocnt)=5
'astroy(astrocnt)=5
'astrox(astrocnt)=0
'astroy(astrocnt)=0
astrot(astrocnt)=0
END IF
END IF
END IF





'NEXT ASTROCNT
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    If astroh(astrocnt)<>1 Then
    if joy1=0 then
if (astz(astrocnt)>=topzzz2-24 or astz(astrocnt)<=topzzz2+24) and asty(astrocnt)<topyyy and asty(astrocnt)>topyyy-(128) and  astx(astrocnt)>topxxx-128 and astx(astrocnt)<topxxx+128 and topzzz>.20-(.20/2) then 
 if tiesaucer=0 and vol255=255 then    FSOUND_Stream_Play(FSOUND_FREE, stream9)
    singletopz=singletopz-int(singled2*3.33)
    if      astx(astrocnt)<topxxx then 
        singletopx=singletopx+(pieyedi*2/360)*(27) 
elseIF astx(astrocnt)>topxxx then 
    singletopx=singletopx-(pieyedi*2/360)*(27)
end if
 if      asty(astrocnt)<topyyy Then 
        singletopy=singletopy+(pieyedi*2/360)*(27) 
elseIF asty(astrocnt)>topyyy then 
    singletopy=singletopy-(pieyedi*2/360)*(27)
end if
end if
if (astz(astrocnt)>=topzzz2-24 or astz(astrocnt)<=topzzz2+24) and asty(astrocnt)<topyyy and asty(astrocnt)>topyyy-(128) and astx(astrocnt)>topxxx-128 and astx(astrocnt)<topxxx+128 and topzzz<.20-(.20/2) then 
 if tiesaucer=0 and vol255=255 then   FSOUND_Stream_Play(FSOUND_FREE, stream9)
   singletopz=singletopz+(SINGLED2*3.33)
    if astx(astrocnt)<topxxx then 
        singletopx=singletopx+(pieyedi*2/360)*(27) 
    elseIF astx(astrocnt)>topxxx then 
        singletopx=singletopx-(pieyedi*2/360)*(27)
    end if
     if      asty(astrocnt)<topyyy then 
        singletopy=singletopy+(pieyedi*2/360)*(27) 
elseIF asty(astrocnt)>topyyy then 
    singletopy=singletopy-(pieyedi*2/360)*(27)
end if
end if

end if

if joy2=0 then
  if (astz(astrocnt)>=bottomzzz2-24 or astz(astrocnt)<=bottomzzz2+24) and asty(astrocnt)<bottomyyy and asty(astrocnt)>bottomyyy-(128) and  astx(astrocnt)>bottomxxx-128 and astx(astrocnt)<bottomxxx+128 and bottomzzz>.20-(.20/2) then 
 if tiesaucer=0 and vol255=255 then    FSOUND_Stream_Play(FSOUND_FREE, stream9)
    singlebottomz=singlebottomz-int(singled2*3.33)
    if      astx(astrocnt)<bottomxxx then 
        singlebottomx=singlebottomx+(pieyedi*2/360)*(27) 
elseIF astx(astrocnt)>bottomxxx then 
    singlebottomx=singlebottomx-(pieyedi*2/360)*(27)
end if
if      asty(astrocnt)<bottomyyy then 
        singlebottomy=singlebottomy+(pieyedi*2/360)*(27) 
elseIF asty(astrocnt)>bottomyyy then 
    singlebottomy=singlebottomy-(pieyedi*2/360)*(27)
end if
end if
if (astz(astrocnt)>=bottomzzz2-24 or astz(astrocnt)<=bottomzzz2+24) and asty(astrocnt)<bottomyyy and asty(astrocnt)>bottomyyy-(128) and astx(astrocnt)>bottomxxx-128 and astx(astrocnt)<bottomxxx+128 and bottomzzz<.20-(.20/2) then 
 if tiesaucer=0 and vol255=255 then   FSOUND_Stream_Play(FSOUND_FREE, stream9)
   singlebottomz=singlebottomz+(SINGLED2*3.33)
    if astx(astrocnt)<bottomxxx then 
        singlebottomx=singlebottomx+(pieyedi*2/360)*(27) 
    elseIF astx(astrocnt)>bottomxxx then 
        singlebottomx=singlebottomx-(pieyedi*2/360)*(27)
    end if
    if      asty(astrocnt)<bottomyyy then 
        singlebottomy=singlebottomy+(pieyedi*2/360)*(27) 
elseIF asty(astrocnt)>bottomyyy then 
    singlebottomy=singlebottomy-(pieyedi*2/360)*(27)
end if
    end if 
   
end If
    ElseIf astroh(astrocnt)=1 Then
 if joy1=0 then
if (astz(astrocnt)>=topzzz2-24 or astz(astrocnt)<=topzzz2+24) and asty(astrocnt)<topyyy and asty(astrocnt)>topyyy-(128) and  astx(astrocnt)>topxxx-128 and astx(astrocnt)<topxxx+128 and topzzz>.20-(.20/2) then 
 if tiesaucer=0 and vol255=255 then    FSOUND_Stream_Play(FSOUND_FREE, stream9)
    singletopz=singletopz+int(singled2*3.33)
    if      astx(astrocnt)<topxxx then 
        singletopx=singletopx-(pieyedi*2/360)*(27*4) 
elseIF astx(astrocnt)>topxxx then 
    singletopx=singletopx+(pieyedi*2/360)*(27*4)
end if
 if      asty(astrocnt)<topyyy Then 
        singletopy=singletopy-(pieyedi*2/360)*(27*4) 
elseIF asty(astrocnt)>topyyy then 
    singletopy=singletopy+(pieyedi*2/360)*(27*4)
end if
end if
if (astz(astrocnt)>=topzzz2-24 or astz(astrocnt)<=topzzz2+24) and asty(astrocnt)<topyyy and asty(astrocnt)>topyyy-(128) and astx(astrocnt)>topxxx-128 and astx(astrocnt)<topxxx+128 and topzzz<.20-(.20/2) then 
 if tiesaucer=0 and vol255=255 then   FSOUND_Stream_Play(FSOUND_FREE, stream9)
   singletopz=singletopz-(SINGLED2*3.33)
    if astx(astrocnt)<topxxx then 
        singletopx=singletopx-(pieyedi*2/360)*(27*4) 
    elseIF astx(astrocnt)>topxxx then 
        singletopx=singletopx+(pieyedi*2/360)*(27*4)
    end if
     if      asty(astrocnt)<topyyy then 
        singletopy=singletopy-(pieyedi*2/360)*(27*4) 
elseIF asty(astrocnt)>topyyy then 
    singletopy=singletopy+(pieyedi*2/360)*(27*4)
end if
end if

end if

if joy2=0 then
  if (astz(astrocnt)>=bottomzzz2-24 or astz(astrocnt)<=bottomzzz2+24) and asty(astrocnt)<bottomyyy and asty(astrocnt)>bottomyyy-(128) and  astx(astrocnt)>bottomxxx-128 and astx(astrocnt)<bottomxxx+128 and bottomzzz>.20-(.20/2) then 
 if tiesaucer=0 and vol255=255 then    FSOUND_Stream_Play(FSOUND_FREE, stream9)
    singlebottomz=singlebottomz-int(singled2*3.33)
    if      astx(astrocnt)<bottomxxx then 
        singlebottomx=singlebottomx-(pieyedi*2/360)*(27*4) 
elseIF astx(astrocnt)>bottomxxx then 
    singlebottomx=singlebottomx+(pieyedi*2/360)*(27*4)
end if
if      asty(astrocnt)<bottomyyy then 
        singlebottomy=singlebottomy-(pieyedi*2/360)*(27*4) 
elseIF asty(astrocnt)>bottomyyy then 
    singlebottomy=singlebottomy+(pieyedi*2/360)*(27*4)
end if
end if
if (astz(astrocnt)>=bottomzzz2-24 or astz(astrocnt)<=bottomzzz2+24) and asty(astrocnt)<bottomyyy and asty(astrocnt)>bottomyyy-(128) and astx(astrocnt)>bottomxxx-128 and astx(astrocnt)<bottomxxx+128 and bottomzzz<.20-(.20/2) then 
 if tiesaucer=0 and vol255=255 then   FSOUND_Stream_Play(FSOUND_FREE, stream9)
   singlebottomz=singlebottomz+(SINGLED2*3.33)
    if astx(astrocnt)<bottomxxx then 
        singlebottomx=singlebottomx-(pieyedi*2/360)*(27*4) 
    elseIF astx(astrocnt)>bottomxxx then 
        singlebottomx=singlebottomx+(pieyedi*2/360)*(27*4)
    end if
    if      asty(astrocnt)<bottomyyy then 
        singlebottomy=singlebottomy-(pieyedi*2/360)*(27*4) 
elseIF asty(astrocnt)>bottomyyy then 
    singlebottomy=singlebottomy+(pieyedi*2/360)*(27*4)
end if
    end if 
   
end If
    End If
 next  
  next calca
 end if
dim as integer bottomzzz2=int(bottomzzz*5*12),topzzz2=int(topzzz*5*12)  
   
   
   
   
   
if (bottomgun3(tempgunx2,tempguny2)>=topzzz2-1 or bottomgun3(tempgunx2,tempguny2)<=topzzz2+1) and bottomgun(tempgunx2,tempguny2,1)<>0 and tempguny>topyyy AND tempguny2<TOPYYY+(128) and  tempgunx2>topxxx-128 and tempgunx2<topxxx+128 and topzzz>.20/2 then 
if tiesaucer=0 and vol255=255 then     FSOUND_Stream_Play(FSOUND_FREE, stream9)
    singletopz=singletopz-Int(singled*3.33)
    if tempgunx2<topxxx then 
        singletopx=singletopx+(pieyedi*2/360)*27 
elseif tempgunx2>topxxx then 
    singletopx=singletopx-(pieyedi*2/360)*27
end if
end if
'END IF
if (bottomgun3(tempgunx2,tempguny2)<=topzzz2-1 or bottomgun3(tempgunx2,tempguny2)>=topzzz2-1) and bottomgun(tempgunx2,tempguny2,1)<>0 and tempguny2>topyyy and tempguny2<topyyy+(128) and tempgunx2>topxxx-128 and tempgunx2<topxxx+128 and topzzz<.20/2 then 
 if tiesaucer=0 and vol255=255 then   FSOUND_Stream_Play(FSOUND_FREE, stream9)
   singletopz=singletopz+Int(singled*3.33)
    if tempgunx2<topxxx then 
        singletopx=singletopx+(pieyedi*2/360)*27 
    elseif tempgunx2>topxxx then 
        singletopx=singletopx-(pieyedi*2/360)*27
        end if
end if
'END IF
'if bottomgun3(tempgunx,tempguny)=topzzz2 and bottomgun(tempgunx,tempguny,1)<>0 and tempguny>topyyy and  tempgunx>topxxx-32 and tempgunx<topxxx+32 and bottomgun3(tempgunx,tempguny)<.25/2 and topzzz<.25 then singletopz=singletopz+1
'if bottomgun3(tempgunx,tempguny)=topzzz2 and bottomgun(tempgunx,tempguny,1)<>0 and tempguny>topyyy and  tempgunx>topxxx-32 and tempgunx<topxxx+32 and bottomgun3(tempgunx,tempguny)>.25/2 and topzzz>.25/2 then singletopz=singletopz-1
'end if
'if joy2<2 or game2=12 then
if (topgun3(tempgunx,tempguny)>=bottomzzz2-1 or topgun3(tempgunx,tempguny)<=bottomzzz2+1) and topgun(tempgunx,tempguny,1)<>0 and tempguny<bottomyyy and tempguny>bottomyyy-(128) and  tempgunx>bottomxxx-128 and tempgunx<bottomxxx+128 and bottomzzz>.20/2 then 
 if tiesaucer=0 and vol255=255 then    FSOUND_Stream_Play(FSOUND_FREE, stream9)
    singlebottomz=singlebottomz-int(singled2*3.33)
    if      tempgunx<bottomxxx then 
        singlebottomx=singlebottomx+(pieyedi*2/360)*27 
elseIF tempgunx>bottomxxx then 
    singlebottomx=singlebottomx-(pieyedi*2/360)*27
end if
end if
if (topgun3(tempgunx,tempguny)>=bottomzzz2-1 or topgun3(tempgunx,tempguny)<=bottomzzz2+1) and topgun(tempgunx,tempguny,1)<>0 and tempguny<bottomyyy and tempguny>bottomyyy-(128) and tempgunx>bottomxxx-128 and tempgunx<bottomxxx+128 and bottomzzz<.20/2 then 
 if tiesaucer=0 and vol255=255 then   FSOUND_Stream_Play(FSOUND_FREE, stream9)
   singlebottomz=singlebottomz+(SINGLED2*3.33)
    if tempgunx<bottomxxx then 
        singlebottomx=singlebottomx+(pieyedi*2/360)*27 
    elseIF tempgunx>bottomxxx then 
        singlebottomx=singlebottomx-(pieyedi*2/360)*27
    end if
    end if
        '    Print "Button ";a;" not pressed."
        'End If
'         Next a
 if game2=3 or game2=5 or game2=10 then
     
    ' sleep
    ' color rgb(255,255,255),rgb(0,0,0)
 '    locate 1,1:print"balldirectionx:";balldirectionx
 '         locate 2,1:print"balldirectiony:";balldirectiony
 '         locate 3,1:print"ballspeed:";ballspeed
'if gameonflag<>255 then
dim as single radnum=int((25*1.33)),radcount
dim as single radcountlineout=radnum+1,radcountlinein=radnum
dim as single a,x,y,x1,y1
'screenlock
For a = 0 To 360 step +1
        x = Cos(a) * radnum
        y = Sin(a) * radnum
       ' x1=cos(a) * radcountlinein
       ' y1=sin(a) * radcountlinein
        screenlock

        circle (ballx + x, bally + y),.75,rgb(0,255,0),,,,f'point(hammerx2+x,hammery2+y)
    screenunlock

Next
'screenlock
'paint(ballx,bally),rgb(0,255,0),rgb(0,255,0)
'circle(hammerx2,hammery2),(xfive)*1,RGB(0,0,0)',,,,F'VIOLET'rgb(255,255,255)
'screenunlock
for radcount=radcountlineout to (int(25*1.33)*2)
radcountlinein=radcountlinein-1
 For a = 0 To 360 step +1'.5'1'.5'3333333'25
        x = Cos(a) * radcount
        y = Sin(a) * radcount
        x1=cos(a) * radcountlinein
        y1=sin(a) * radcountlinein
        screenlock

        PSet (ballx + x1, bally + y1),point(ballx+x,bally+y)
    screenunlock

Next
next






' screenlock
' if bally<(720)/2+1 then
'circle(ballx,bally),7+((10)*bally*100/((720)/2)/100),rgb(255,255,255),,,,f
'else
'  circle(ballx,bally),7+((10)*(720-bally)*100/((720)/2)/100),rgb(255,255,255),,,,f
'end if  
'line(0,(720)/2)-(1280-1,(720)/2),rgb(255,255,255)
'line(129,1)-(129,720),rgb(255,255,255)
'line(1280-129,1)-(1280-129,720-1),rgb(255,255,255)
'screenunlock
'if buttons<>1 then        
      screenlock
      line(bottomxxx-(128*1.5),720-3)-(bottomxxx+(128*1.5),720-3-8),rgb(0,0,127),BF'rgb((bottomr),(bottomr),255),bf
'screenunlock
'end if
' if buttons2<>1 then 
'    screenlock
    line(topxxx-(128*1.5),3)-(topxxx+(128*1.5),3+8),rgb(0,0,127),BF'rgb((topr),(topr),255),bf
'end if
 screenunlock
'screencopy
 'if readyflg=0 then
 if headstales=0 and gameonflag=0 and zcount=0 then
                          if zcount=0 then zcount=timer+7
      'RANDOMIZE RND+TIMER+Rnd
      flat33=0+(pieyedi/6-(pieyedi/(96*2)))

        ' flat33=0-(pieyedi/10)
     'headstales=rnd
     'if headstales>.5 then 
         balldirectiony=rnd
         balldirectionx=(-.5+rnd)
        
         ballx=1280/2-257+(rnd*(257*2))'+131
         bally=720/2-257+(rnd*257)
         headstales=1
        ' if game2<6 then
         gameonflag=255
    ' elseif game2=10 then
    '     gameonflag=1
    '     end if
         ballspeed=7
     elseif headstales=1 and gameonflag=0 and zcount=0 then
        ' screencopy
                                  if zcount=0 then zcount=timer+7
'flat33=0-(pieyedi/10)
'RANDOMIZE RND+TIMER+Rnd
      flat33=0+(pieyedi/6-(pieyedi/(96*2)))

         balldirectiony=-rnd
         balldirectionx=-(-.5+rnd)
         ballx=1280/2+257-(rnd*(257*2))'-131
         bally=720/2+257-(rnd*257)'-131
         headstales=0
'if game2<6 then
         gameonflag=255
'     elseif game2=10 then
'         gameonflag=1
'     end if
     ballspeed=7
     '    else
     end if
 if zcount>timer then
 if readyflg=0 then
     readyflg=1
 screenset 2,0
     cls
     'screencopy 0,2
     SCREENLOCK
     draw string (1,1),"READY",RGB(0,255/2,0)'127,63)
     SCREENUNLOCK
               dim as SINGLE rgbar,rgbag,rgbab
               rgbar=int(rnd*257)+1:rgbag=int(rnd*257)+1:rgbab=int(rnd*257)+1

     for ccx=-(5*8/2) to (5*8/2)' step +2'64
    for ccy=-5 to 5
        if point(ccx+(5*8/2)+1,ccy+5)=rgb(0,255/2,0) then
          ' for ccx2=1 to 6
          '     for ccy2=1 to 6
         ' dim as SINGLE rgbar=int(rnd*257),rgbag=int(rnd*257),rgbab=int(rnd*257)
                   display3(ccx,ccy,rgb(255-(rgbar*((ccy+5)*100/10/100)),255-(rgbag*((ccy+5)*100/10/100)),255-(rgbab*((ccy+5)*100/10/100))),0)
                   ' txtyy()=rgb(255,255,255)
            '   next ccy2
            'next ccx2
        elseif point(ccx+(5*8/2)+1,ccy+5)=rgb(0,0,0) then
          'for ccx2=1 to 6
          '     for ccy2=1 to 6
                   display3(ccx,ccy,rgb(0,0,0),0)
                   ' txtyy()=rgb(255,255,255)
      '   end if   
           ' next ccy2
           ' next ccx2  
        end if
    next ccy
next ccx
'if zcount=0 then zcount=timer+5
 '    do
' readyflg=1
' screenset 1,0
screenset 1,0

 end if
       
'flip

'screencopy '2,0
'sleep 1000/60,1
'if timer<zcount then

'loop until timer>zcount'zcount>timer
        ' SCREENSET 1,0
      ' screencopy 0,1
      dim ccz as integer
      dim as integer ppp1=1,ppp2=1
 dim as SINGLE xccx,yccy,flatzz,newz',flatx,flaty,newx,newy,newz

       for ccx=-(5*8*12/2) to (5*8*12/2) step +ppp1
           ppp1=ppp1+1
           if ppp1=3 then ppp1=1
           ppp1=2
           
           for ccy=-(5*12) to (5*12) step +ppp2
                ppp2=ppp2+1
           if ppp2=3 then ppp2=1
           ppp2=2
                     for ccz=int(24) to 1 step -3'*.66

 '   if ccy>8*6-1 then txtxxyy(ccx,ccy)=rgb(255,255,0)
 '   if ccx>136*6-1 then txtxxyy(ccx,ccy)=rgb(255,255,0)
rem if ccy=1 or ccy=8*6 or ccx=1 or ccx=136*6 then txtxxyy(ccx,ccy)=rgb(0,0,0)
 'if game2<1 then
 IF renderready(CCX,CCY,ccz)<>RGB(0,0,0) THEN
     flatzz=ccz
 'dim as SINGLE pieyedi=3.14159'2653589793238462643383279502884197169399375105820974944592307816406286
     flatx=ccx'(1280)/2-((19*8*6)/2)+ccx
     flaty=ccy'(720)/2-(8*6/2)+ccy
     newX = flatx'newx'
newy = (sin(flat33) * flatzz) + (cos(flat33) * flaty)
newz = (cos(flat33) * flatzz) - (sin(flat33) * flaty)
 flatx=newx
 flaty=newy
 flatzz=newz
 newx = (cos(-(pieyedi*2)+flat33) * flatx) - (sin(-(pieyedi*2)+flat33) * flatzz)
newy=flaty''y = y'
newz = (sin(-(pieyedi*2)+flat33) * flatx) + (cos(-(pieyedi*2)+flat33) * flatzz)
 flatx=newx
 flaty=newy
 flatzz=newz
 
 newx = (cos(-(pieyedi*2)-flat33) * flatx) + (sin(-(pieyedi*2)-flat33) * flaty)
newy = (cos(-(pieyedi*2)-flat33) * flaty) - (sin(-(pieyedi*2)-flat33) * flatx)
newz = flatzz'
 flatx=newx
 flaty=newy
 flatzz=newz
 screenlock
circle(newx+(1280/2),newy+(720/2)),1,renderready(ccx,ccy,ccz),,,,F
 '    pset pic,(ccx,ccy),txtxxyy(ccx,ccy)
'if renderready(ccx,ccy,ccz)<>rgb(0,0,0) then pset(newx+(1280/2),newy+(720/2)),renderready(ccx,ccy,ccz)'rgb(255,0,251)'pset ((1280)/2-((19*8*6)/2)+ccx,(720)/2-(8*6/2)+ccy),RGB(255,0,0)'txtxxyy2(ccx,ccy)
 'elseif game2>0 then
 '       pset((1280)/2-((21*8*6)/2)+ccx,ccy),txtxxyy(ccx,ccy)
 ' end if
  screenunlock
  end if


 next ccz

next ccy:next ccx


         end if
     
 '    end if
 'end if
'end if


'               screenlock

'if game2=10 then buttons=1
if timer>zcount then gameonflag=1:zcount=0
if buttons2=1 and zcount<>0 and headstales=1 and timer>zcount then gameonflag=1:zcount=0
if buttons=1 and zcount<>0 and headstales=0 and timer>zcount then gameonflag=1:zcount=0
if buttons=1 and zcount<>0 and (game2=5) and timer>zcount then gameonflag=1:zcount=0
if game2=10 and zcount<>0 and timer>zcount then zcount=0:gameonflag=1
   ' if headstales=0 then
   '     headstales=1
   ' else
   '     headstales=0
   ' end if
   ' end if
'if game2=10 then gameonflag=1
'if gameonflag=255 then sleep 50,1:goto cainfled
if gameonflag=1 then
     if (game2=3 or game2=5 or game2=10) and ballx<topxxx+(128*1.5) and ballx>topxxx-(128*1.5) and bally-int((25*1.33/2))<(15+(ballspeed)) then
   '      if ballx<129+(ballspeed/1.5) then
   '  if ballspeed<(129-ballx) then ballspeed=(129-ballx*1.5)
   balldirectiony=balldirectiony-(balldirectiony*2)
   pingpongtop=pingpongtop+1
           ' balldirectionx=balldirectionx-(balldirectionx*2)
topr=255
    if ballx<TOPxxx+1 then

IF BALLDIRECTIONX<0 THEN balldirectionx=balldirectionx-(1/34567*((TOPxxx-ballx)*100/(128*1.5/2)/100))
IF BALLDIRECTIONX>0 THEN balldirectionx=balldirectionx+(1/34567*((TOPxxx-ballx)*100/(128*1.5/2)/100))

elseif ballx>TOPxxx-1 then
  IF BALLDIRECTIONX<0 THEN balldirectionx=balldirectionx-(1/34567*((ballx-TOPxxx)*100/(128*1.5/2)/100))
    IF BALLDIRECTIONX>0 THEN balldirectionx=balldirectionx+(1/34567*((ballx-TOPxxx)*100/(128*1.5/2)/100))

end if
        ' balldirectionx=balldirectionx-(balldirectionx*2)
ballspeed=ballspeed+7'6.33'7
'if ballx<topxxx then
'ballspeedflag=0
'balldirectionx=balldirectionx-1
'else
'sound pulsewave(notes(1)*2*2/2/2/2),3/(2*24)/2
shots=shots+1
fsound_stream_stop(stream8)
    if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream8)
' balldirectionx=balldirectionx+1
' end if
'   balldirectiony=balldirectiony+1
   'if ballx<topxxx and balldirectiox<0 then balldirectionx=balldirectionx-1
   'if ballx<topxxx and balldirection>x 
'balldirectionx=balldirectionx+.01'1
       '  ballspeed=ballspeed+3
       '  bally=bally+32
   '      if balldirectionx<0 then 
   '          balldirectionx=1
    '     else
   '          balldirectionx=-1
    '     end if
    '      if xxx2<-.75 and xxx2>-1.5 then 
'balldirectionx=-1'balldirectionx-1
'elseif xxx2>.75 and xxx2<1.5 then
'    balldirectionx=1'balldirectionx+1
'    end if
        
         'end if

      '   if balldirectionx<0 then 
      '       balldirectionx=(+ballspeed/2)
      '   elseif balldirectionx>0 then
      '       balldirectionx=(-ballspeed/2)
      '   end if
        ' if balldirectiony<0 then
        '     balldirectiony=(+balldirectiony*2)
        ' elseif balldirectiony>0 then
        '     balldirectiony=(-balldirectiony*2)
        ' end if
'gameonflag=1
screenlock
         line(topxxx-(128*1.5),4)-(topxxx+(128*1.5),4+8),rgb(255,255,255),bf
         screenunlock
end if
'elseif buttons2=1 then
'    screenlock
'             line(topxxx-32-24,6)-(topxxx+32+24,6+8),rgb(255,255,251),bf
'screenunlock
'    elseif ballx<topxxx+32+24 and ballx>topxxx-32-24 and bally<20 then
'    balldirectiony=balldirectiony-(balldirectiony*2)
'    sound pulsewave(notes(1)/2/2/2/2),3/(2*24)/2

        ' balldirectionx=balldirectionx-(balldirectionx*2)
'if ballx<topxxx then 
'balldirectionx=balldirectionx-1
'else
' balldirectionx=balldirectionx+1
' end if
  ' balldirectiony=balldirectiony+1
   'if ballx<topxxx and balldirectiox<0 then balldirectionx=balldirectionx-1
   'if ballx<topxxx and balldirection>x 
'balldirectionx=balldirectionx-.01
  ' else
'end if
'screenlock
 livestop=5
 livesbottom=5
 energytop=100
 energybottom=100
 if (game2=3 or game2=5 or game2=10) and ballx<bottomxxx+(128*1.5) and ballx>bottomxxx-(128*1.5) and bally+int((25*1.33/2))>720-(15+(ballspeed)) then
    balldirectiony=balldirectiony-(balldirectiony*2)
    pingpongbottom=pingpongbottom+1
          '   balldirectionx=balldirectionx-(balldirectionx*2)
bottomr=255
   if ballx<bottomxxx+1 then

IF BALLDIRECTIONX<0 THEN balldirectionx=balldirectionx-(1/34567*((bottomxxx-ballx)*100/(128*1.5/2)/100))
IF BALLDIRECTIONX>0 THEN balldirectionx=balldirectionx+(1/34567*((bottomxxx-ballx)*100/(128*1.5/2)/100))

elseif ballx>bottomxxx-1 then
  IF BALLDIRECTIONX<0 THEN balldirectionx=balldirectionx-(1/34567*((ballx-bottomxxx)*100/(128*1.5/2)/100))
    IF BALLDIRECTIONX>0 THEN balldirectionx=balldirectionx+(1/34567*((ballx-bottomxxx)*100/(128*1.5/2)/100))

end if
ballspeed=ballspeed+7'6.33'7
'ballspeedflag=0
 '  balldirectiony=balldirectiony-1
   'if ballx<topxxx and balldirectiox<0 then balldirectionx=balldirectionx-1
   'if ballx<topxxx and balldirection>x 
'if ballx<bottomxxx then 
'balldirectionx=balldirectionx-1
'else
' balldirectionx=balldirectionx+1
' end if
   'sound pulsewave(notes(1)*2*2/2/2/2),3/(2*24)/2
shots=shots+1
fsound_stream_stop(stream8)
    if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream8)
    
    
    '.01
    'else
        
       '  end if
       ' else
       screenlock
     line(bottomxxx-(128*1.5),720-4)-(bottomxxx+(128*1.5),720-4-8),rgb(255,255,255),bf
 'end if
 screenunlock
 end if
' elseif buttons=1 then
'     screenlock
'          line(bottomxxx-32-24,720-6)-(bottomxxx+32+24,720-8-6),rgb(255,255,251),bf
'screenunlock
'elseif ballx<bottomxxx+32+24 and ballx>bottomxxx-32-24 and bally>720-20 then
'   balldirectiony=balldirectiony-(balldirectiony*2)
 '        balldirectionx=balldirectionx-(balldirectionx*2)
'sound pulsewave(notes(1)/2/2/2/2),3/(2*24)/2

 '  balldirectiony=balldirectiony-1
   'if ballx<topxxx and balldirectiox<0 then balldirectionx=balldirectionx-1
   'if ballx<topxxx and balldirection>x 
'balldirectionx=balldirectionx+1
'if ballx<bottomxxx then 
'balldirectionx=balldirectionx-1
'else
' balldirectionx=balldirectionx+1
' end if

'end if
    'else
    
 

'if gameonflag=1 then
if balldirectionx>0 then ballx=(ballx+ballspeed)+(balldirectionx)

if balldirectionx<0 then ballx=(ballx-ballspeed)+(balldirectionx)
    
   if balldirectionx=0 and balldirectiony<0 then 
    bally=bally-ballspeed
    if rnd>.5 then
        balldirectionx=+1
    else
        balldirectionx=-1
        end if
elseif balldirectionx=0 and balldirectiony>0 then
    bally=bally+ballspeed
    if rnd>.5 then
        balldirectionx=+1
    else
        balldirectionx=-1
        end if
   ' else
end if
 'if balldirectionx=0 and balldirectiony=0 then 
    'bally=bally-ballspeed
  '  if rnd>.5 then
  '      balldirectiony=-1
  '  else
  '      balldirectiony=+1
  '  end if
  '  if rnd>.5 then
  '      balldirectionx=+1
  '  else
  '      balldirectionx=-1
  '  end if
  '  end if
    
if balldirectiony<0 then bally=bally-ballspeed
if balldirectiony>0 then bally=bally+ballspeed
'if ballspeed>5 and ballspeedflag=1 then ballspeedflag=0
if ballspeed>0 and ballspeedflag=0 then ballspeed=ballspeed-(1/24)
if ballspeed>6 and ballspeedflag=1 then ballspeedflag=0
if ballspeed<3 or ballspeedflag=1 then
    ballspeedflag=1
    if bally>(720)/2 and balldirectiony>0 then 
        ballspeed=ballspeed+1/24'(1/24)*4'10'.75
       balldirectiony=+1
    else
        ballspeed=ballspeed+1/24'(1/24)*4'10'.75
       balldirectiony=-1
    end if
end if

   ' ballspeed=ballspeed-(ballspeed*2)+1
if ballspeed>int(36/2) then ballspeed=int(36/2)
'if ballspeed<0 then 
'    bally=bally+ballspeed
'elseif ballspeed>0 then
'    bally=bally-ballspeed
' end if 
' gameonflag=0
' end if
 if ballx-int((25*1.33*.85))<(1+(ballspeed*1.5)) then
     if ballspeed<((1)-ballx) then ballspeed=(131*1.5-ballx)
     balldirectionx=balldirectionx-(balldirectionx*2):ballspeed=ballspeed-1'(1/32)
'if ballspeed<131-ballx then ballspeed=ballspeed+(131-ballx)
end if
 if ballx+int((25*1.33*.85))>(1280-(ballspeed*1.5)) then 
          if ballspeed<(ballx-(1280-(1))) then ballspeed=(ballx-(1280-(133*1.5)))

     balldirectionx=balldirectionx-(balldirectionx*2):ballspeed=ballspeed-1'(1/16)
'if ballspeed<ballx-(128031) then ballspeed=ballspeed+(ballx-(128031))
end if

if cup9top<15 and cup9bottom<15 then
if (ballx-int((25*1.33))<topxxx-(128*1.5) or ballx+int((25*1.33))>topxxx+(128*1.5)) and bally<2 then 
     ' sound pulsewave(notes(4)/2/2/2/2/2),3/2
                     FSOUND_Stream_stop(stream2)
               FSOUND_Stream_stop(stream3)
               FSOUND_Stream_stop(stream4)
               FSOUND_Stream_stop(stream7)
     '    stream2=FSOUND_Stream_Open(SOUND_FILE7, 0, 0, 0)
if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream7)
        ' stream2=FSOUND_Stream_Open(SOUND_FILE2, 0, 0, 0)
   'stream3=FSOUND_Stream_Open(SOUND_FILE3, 0, 0, 0)
   '   stream4=FSOUND_Stream_Open(SOUND_FILE4, 0, 0, 0)

       if shots>30*.75 and vol255=255 then FSOUND_Stream_Play(FSOUND_FREE, stream2):shots=0       'sleep
    if cup9bottom>cup9top and vol255=255 then   FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
        if cup9bottom>4 and vol255=255 then   FSOUND_Stream_Play(FSOUND_FREE, stream4)       'sleep
readyflg=0
'do
'sleep 50,1
'loop       until (FSOUND_Stream_GetPosition(stream3) >= FSOUND_Stream_GetLength(stream3))

'If (FSOUND_Stream_GetPosition(stream3) >= FSOUND_Stream_GetLength(stream3)) Then
   '             Exit While
   '         End If
   '         Sleep 50, 1
   '     Wend
       
'        FSOUND_Stream_Stop(stream3)
'        FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
'       end if
    cup9bottom=cup9bottom+1
    gameonflag=0
   ' game2=game2+1
    pressed=0
  ' 'mutexlock mutex
    'gameonflag=0
   ' game=game2
   ' ''mutexunlock mutex
        ' sleep (1000,1)

   ' game2=game2+1
  '  ballx=(1280)/2
  '  bally=(720)/2

   ' gameonflag=0
    ' balldirectiony=balldirectiony-(balldirectiony*2)
        ' balldirectionx=balldirectionx-(balldirectionx*2)
'sleep 1000,1
end if
   'balldirectiony=balldirectiony-1
   'if ballx<topxxx and balldirectiox<0 then balldirectionx=balldirectionx-1
   'if ballx<topxxx and balldirection>x 
'balldirectionx=balldirectionx-1
    'else
'end if
rem hamington
if (ballx-int((25*1.33))<bottomxxx-(128*1.5) or ballx+int((25*1.33))>bottomxxx+(128*1.5)) and bally>720-2 then
 ' sound pulsewave(notes(4)/2/2/2/2/2),3/2
           FSOUND_Stream_stop(stream2)
               FSOUND_Stream_stop(stream3)
               FSOUND_Stream_stop(stream4)
         FSOUND_Stream_stop(stream7)
        ' stream2=FSOUND_Stream_Open(SOUND_FILE7, 0, 0, 0)
if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream7)
         'stream2=FSOUND_Stream_Open(SOUND_FILE2, 0, 0, 0)
  ' stream3=FSOUND_Stream_Open(SOUND_FILE3, 0, 0, 0)
  '    stream4=FSOUND_Stream_Open(SOUND_FILE4, 0, 0, 0)

if vol255=255 then
       if shots>30*.75 then FSOUND_Stream_Play(FSOUND_FREE, stream2):shots=0       'sleep
     if cup9top>cup9bottom then    FSOUND_Stream_Play(FSOUND_FREE, stream3)
     if cup9top>4 then FSOUND_Stream_Play(FSOUND_FREE, stream4)
'sleep 50,1
end if
'   loop    until (FSOUND_Stream_GetPosition(stream3) >= FSOUND_Stream_GetLength(stream3))
readyflg=0
'If (FSOUND_Stream_GetPosition(stream3) >= FSOUND_Stream_GetLength(stream3)) Then
   '             Exit While
   '         End If
   '         Sleep 50, 1
   '     Wend
       
'        FSOUND_Stream_Stop(stream3)
'        FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
'       end if

  pressed=0

    cup9top=cup9top+1
    gameonflag=0
'game2=game2+1
    
 '   'mutexlock mutex
    'gameonflag=0
 '   game=game2
 '   ''mutexunlock mutex
        ' sleep (1000,1)

   ' game2=game2+1
   ' ballx=(1280)/2
   ' bally=(720)/2
'sleep 1000,1
end if
'IF TOPXXX<132 THEN SINGLETOPX=0
'IF TOPXXX>1280-132 THEN SINGLETOPX=0
'IF BOTTOMXXX<132 THEN SINGLEBOTTOMX=0
'IF BOTTOMXXX>1280-132 THEN SINGLEBOTTOMX=0

end if
'ballspeed=ballspeed-.25

 '    screenunlock
     
     if (cup9top>14 or cup9bottom>14) then
                 'FSOUND_Stream_Play(FSOUND_FREE, stream3)       'sleep
       ' FSOUND_Stream_Play(FSOUND_FREE, stream4)       'sleep
' FSOUND_Stream_Close(stream2)
               FSOUND_Stream_stop(stream4)

        ' FSOUND_Stream_Open(SOUND_FILE2, FSOUND_MPEGACCURATE, 0, 0)
  ' stream4=FSOUND_Stream_Open(SOUND_FILE4, 0, 0, 0)

        if vol255=255 then fsound_Stream_Play(FSOUND_FREE, stream4)       'sleep
       ' FSOUND_Stream_Play(FSOUND_FREE, stream3) 
    gameonflag=0
    cup9toppuff=cup9top+(14-cup9bottom)*pingpongtop
        cup9bottompuff=cup9bottom+(14-cup9top)*pingpongbottom

 '    sleep (1000,1)
 dim answer1 as integer
 answer1=0
 for scores=1 to 10'1 step -1
     '  if (cup9top+(10-cup9bottom)>PONGscore(scores) then' elseif cup9bottom+(10-cup9top)>PONGscore(scores)) then
           
    if cup9toppuff>PONGscore(scores) and cup9top>cup9bottom and answer1=0 then
        screenset 1,0:cls:color rgb(255,255,0),rgb(0,0,0)
       ' print
       ' print
      ' draw string theking
        screencopy
       FOR SCORE=9 to scores STEP -1
           PONGNAME(SCORE)=PONGNAME(SCORE+1)
            PONGSCORE(SCORE)=PONGSCORE(SCORE+1)
            NEXT SCORE
        if game2=5 or GAME2=10 then 
            PONGname(scores)="C.P.U."
        else
            
                             
           'screencopy 
            'screenset 0,0
            dim as integer th,th2=65,th3
           ' dim as integer choice1=65',choice2
            dim initials(3) as string
            
           ' for th=1 to 3:initials(th)=" ":next th
            th=1
            do
                cls
                 drawdraw(1,1,"You won "+str(scores)+" place top screen player",rgb(0,255,0),3)

           drawdraw(1,25,"Enter only your initials top screen player position",rgb(255,0,0),3)
  drawdraw(1,720/2+96,"Use left joystick up/down pick letter then hit A (1) Select D (4) Deselect START to Finish",rgb(0,255,255),2)
  '              screencopy 1,0
            for th3=1 to th
                if th3<th then
                                 drawdraw(1280/2-(8*8*6/2)+(64*th3),720/2-(8*8/2),initials(th3),rgb(255,255,255),8)
   else
             line(1280/2-(8*8*6/2)+(64*th3),720/2-(8*8/2)+64)-(1280/2-(8*8*5/2-64)+(64*th)+64,720/2-(8*8/2)),rgb(0,0,0),BF   
                drawdraw(1280/2-(8*8*6/2)+(64*th3),720/2-(8*8/2),chr(th2),rgb(255,0,0),8)
       end if
       next 
            line(1280/2-(8*8*6/2)+(64*th),720/2-(8*8/2)+67)-(1280/2-(8*8*5/2-64)+(64*th)+64,720/2-(8*8/2)+67),rgb(255,255,255)
                sleep (25)

    DIM DULL2 AS SINGLE
    GetJoystick(0,buttons2,xxx2,yyy2,DULL2,DULL2)
'GetJoystick(1,buttons2,xxx2,yyy2,DULL2,DULL2)
    k=inkey
   ' if th>3 then buttons2=512
'if choice=choice2 and (buttons=1 or buttons2=1) then buttons=0:buttons2=0:k="":k2="":choice2=0
if th=4 then th=3
if buttons2=1 and th<4 then
   initials(th)=chr(th2)
   if th=3 then 
           buttons2=512
           elseif th<3 then 
       th=th+1
   end if
   sleep (250)
end if
if buttons2=8 and th>1 and th<4 then
    th=th-1
    initials(th)=""
    sleep (250)
    end if
    if (yyy2<-.75 and yyy2>-1.5) then 
        
        th2=th2+1
if th2>90 then th2=65
        
        sleep (int(250/2))
        end if
if (yyy2>.75 and yyy2<1.5) then 
    'singletopz=singletopz-singled
  ' a(th
 th2=th2-1
        if th2<65 then th2=90 
  sleep (int(250/2))
  

end if
screencopy
sleep (75)
        loop until buttons2=512
       
            
           for th=1 to 3:pongname(scores)=pongname(scores)+initials(th)+".":next
           ' line input starname(scores)
            end if
            PONGscore(scores)=cup9toppuff
         PONGname(scores)=PONGname(scores)+" TOP SCREEN "+str(cup9top)+"-"+str(cup9bottom)+" "
         screenset 1,0:cls
          'dim score as integer
    kill("scores.dat"):sleep (1000)
             open "scores.dat" for OUTPUT as #1

    for score=1 to 10
        print #1,starname(score)
        print #1,starscore(score)
    next score
    for score=1 to 10
        print #1,pongname(score)
        print #1,pongscore(score)
    next score
        print #1,timeflag
print #1,tiesaucer:print #1,vol255:print #1,muting
print #1,fps:Print #1,fullscreentoggle':print #1,pingpongtop:print #1,pingpongbottom
    close #1
       '  exit for
         answer1=1
elseif cup9bottompuff>PONGscore(scores) and cup9bottom>cup9top and answer1=0 then
    screenset 1,0:cls:color rgb(255,255,0),rgb(0,0,0)
        'print
        'print
        'print"You have won a ";scores;" place position bottom screen player person"
screencopy
         FOR SCORE=9 to scores STEP -1
            PONGNAME(SCORE)=PONGNAME(SCORE+1)
            PONGSCORE(SCORE)=PONGSCORE(SCORE+1)
            NEXT SCORE
        if game2=10 then 
            PONGname(scores)="C.P.U."
        else
          '  print
          '  print
          '  print"Enter your name bottom screen player position"
                 
           screencopy 
           ' screenset 0,0
            dim as integer th,th2=65,th3
           ' dim as integer choice1=65',choice2
            dim initials(3) as string
            
           ' for th=1 to 3:initials(th)=" ":next th
            th=1
            do
                cls
 drawdraw(1,1,"You won "+str(scores)+" place bottom screen player",rgb(0,255,0),3)

            drawdraw(1,25,"Enter only your initials bottom screen player position",rgb(255,0,0),3)
  drawdraw(1,720/2+96,"Use left joystick up/down pick letter then hit A (1) Select D (4) Deselect START to Finish",rgb(0,255,255),2)                          
            '    screencopy 1,0
          for th3=1 to th
                if th3<th then
                                 drawdraw(1280/2-(8*8*6/2)+(64*th3),720/2-(8*8/2),initials(th3),rgb(255,255,255),8)
   else
             line(1280/2-(8*8*6/2)+(64*th3),720/2-(8*8/2)+64)-(1280/2-(8*8*5/2-64)+(64*th)+64,720/2-(8*8/2)),rgb(0,0,0),BF   
                drawdraw(1280/2-(8*8*6/2)+(64*th3),720/2-(8*8/2),chr(th2),rgb(255,0,0),8)
       end if
       next 
            line(1280/2-(8*8*6/2)+(64*th),720/2-(8*8/2)+67)-(1280/2-(8*8*5/2-64)+(64*th)+64,720/2-(8*8/2)+67),rgb(255,255,255)
                sleep (25)

    DIM DULL2 AS SINGLE
    GetJoystick(0,buttons2,xxx2,yyy2,DULL2,DULL2)
'GetJoystick(1,buttons2,xxx2,yyy2,DULL2,DULL2)
    k=inkey
   ' if th>3 then buttons2=512
'if choice=choice2 and (buttons=1 or buttons2=1) then buttons=0:buttons2=0:k="":k2="":choice2=0
if th=4 then th=3
if buttons2=1 and th<4 then
   initials(th)=chr(th2)
   if th=3 then 
           buttons2=512
           elseif th<3 then 
       th=th+1
   end if
   sleep (250)
end if
if buttons2=8 and th>1 and th<4 then
    th=th-1
    initials(th)=""
    sleep (250)
    end if
    if (yyy2<-.75 and yyy2>-1.5) then 
        
        th2=th2+1
if th2>90 then th2=65
        
        sleep (int(250/2))
        end if
if (yyy2>.75 and yyy2<1.5) then 
    'singletopz=singletopz-singled
  ' a(th
 th2=th2-1
        if th2<65 then th2=90 
  sleep (int(250/2))
  

end if
screencopy
sleep (75)
        loop until buttons2=512
        ' dim scores7 as integer
        ' for scores7=9 to scores
        '     pongscore(scores7)=pongscore(scores7+1)
        '     pongname(scores7)=pongname(scores7+1)
        ' next scores7
            pongname(scores)=""
           ' pongname(scores)=""
            
           for th=1 to 3:pongname(scores)=pongname(scores)+initials(th)+".":next
           ' line input starname(scores)
            end if
            PONGscore(scores)=cup9bottompuff
         PONGname(scores)=PONGname(scores)+" BOTTOM SCREEN "+str(cup9bottom)+"-"+str(cup9top)+" "
         screenset 1,0:cls
        ' delete
    'dim score as integer
    kill("scores.dat"):sleep (1000)
             open "scores.dat" for OUTPUT as #1

    for score=1 to 10
        print #1,starname(score)
        print #1,starscore(score)
    next score
    for score=1 to 10
        print #1,pongname(score)
        print #1,pongscore(score)
    next score
        print #1,timeflag
print #1,tiesaucer:print #1,vol255:print #1,muting
print #1,fps:Print #1,fullscreentoggle
    close #1
    answer1=1
      '   exit for
         end if
'    end if
    next
    if game2=3 then 
        game=4
        game2=4
        end if
    if game2=5 then 
        game=6
        game2=6
        end if
    if game2=10 then 
        game=11
        game2=11
        end if
    
'game2=game2+1
timered2=timer+60
readyflg=0
pressed=0
    'paint(1,1),violet,rgb(255,3,3)
    'screencopy
 '   'mutexlock mutex
   ' game3=game-1
    
    
   ' game3=0
  '  ''mutexunlock mutex
   ' game2=4
   ' dim as SINGLE tmpgunx,tmpguny
   ' for tmpgunx=1 to 1280
   ' for tmpguny=1 to 720
   '     topgun(tmpgunx,tmpguny)=0
   '             bottomgun(tmpgunx,tmpguny)=0
'ballx=(1280)/2
'    bally=(720)/2
'
'    next tmpguny
'    next tmpgunx
  '  sleep 3000,1
'end if
     'if bally<7 then cuptop64
    ' sleep (50)
 end if
 end if
end if
'end if

'end if
'if singletopx<-33 then singletopx=-33
'if singletopx>33 then singletopx=33
'if singlebottomx<-33 then singlebottomx=-33
'if singlebottomx>33 then singlebottomx=33

end if
'tempgunx=tempgunx+1
'if tempgunx>1280 then tempgunx=0
 '   tempguny=tempguny+1
' next tempguny
'next tempgunx
'end if
'if tempguny>720 then tempguny=0
if game2=11 or game2=6 or game2=4 then
    draw string (1280/2-(16*8/2),720),"Seconds left:"+str(int(timered2-timer)),rgb(255,255,255)
end if
if game2=2 then
    draw string (1280/2-(16*8/2),720),"Seconds left:"+str(int(timered2-timer)),rgb(255,255,255)
end if
'spin=spin+1    
    
  '  Locate 1,1
  '  Print ;"result:";result;" x:" ;x;" y:";y;" Buttons:";buttons,"","",""
   
    'This tests to see which buttons from 1 to 27 are pressed.
   ' For a = 0 To 26
   '     If (buttons And (1 Shl a)) Then
   '         Print "Button ";a;" pressed.    "
   '     Else
   '         Print "Button ";a;" not pressed."
   '     End If
   ' Next a
'Loop

 'sleep(1000,1)
' alphahouse:
 'screencopy 'cccc,0
 
'cls
'cls
IF GAME2=1 OR GAME2=12 THEN DRAWDRAW(1280/2-(LEN(" SECONDS LEFT"+STR(int(TIMERED2-TIMER)))),16,STR(int(TIMERED2-TIMER))+" SECONDS LEFT",RGB(255,128,0),1)
if fps=1 then drawdraw(1,32,"FPS:"+str(tt4)+"/"+str(timber3),rgb(255,255,255),2)',,xor
 flip
screencopy

if shots1>0 then shots1=shots1-1
if shots2>0 then shots2=shots2-1
'screenunlock
'flip
'sleep (1.5,1)
'paint(0,0),rgb(0,0,0)
'screencopy
'dim as SINGLE calc1
'counter_end()
if game2=0 then timber3=50
if   (game2=1 or game2=12)  then timber3=30
if game2=2 or game2=4 or game2=6 or game2=11 then timber3=60
if game2=3 then timber3=50'45
if game2=5 then timber3=50'45
if game2=10 then timber3=50'45
if game2=13 then timber3=60
if topr<0 then topr=0
if bottomr<0 then bottomr=0
if topr>1 then topr=topr-8
if bottomr>1 then bottomr=bottomr-8
'if game2=3 or game2=5 then sleep (0)
mercy=mercy+1
If mercy>4 Then
mercy=1	
EndIf
dogaine:
'RANDOMIZE Timer/Rnd*Rnd/Rnd*Rnd

 
' If mercy<1 Then GoTo dogaine
'If mercy>4 Then GoTo dogaine
If Rnd<.5 Then 
	
'EndIf
Randomize (Rnd*Timer)*Rnd
Else
	RANDOMIZE (Timer/rnd)/Rnd
EndIf
 mercy=Int(Rnd*3)+1
 If mercy<1 Or mercy>3 Then GoTo dogaine

'End if
FSOUND_Update
'sleep (25)
counter_end()

calc1=1000*(counter_cycles*100/total_cycles/100)
if ((1000/timber3)-calc1)<0 or game2<>game then
sleep (int(1000/timber3))
elseif game2=game then
sleep (int(1000/timber3-calc1))'-calc1,1
end if
'if game16<>0 and game2=0 then
    'mutexlock mutex
'    game=game16
'    game2=game16
    ''mutexunlock mutex
'    game16=0
'    end if
'calc1=1000*(counter_cycles*100/totalcycles/100)
'calc1 =timer-timered/1000

'if calc1<0 or calc1>1000/60 then
''flag=1
'sleep(0,1)
'else
    
'sleep (1000/timber3,1)




'if game2=0 or game2=2 then
'if calc1>1000/timber3 or calc1<0 then
'    sleep (0,1)
'    else
'sleep (1000/timber3-calc1,1)
'end if




'else
'    sleep 0,1
'    end if
'end if

'cls
'paint(1,1),rgb(255,7,7),rgb(1,2,3)
'screencopy 
'screencopy
'cls
'else
'sleep (1000/60-calc1,1)'-calc1,1)
'end if
'sleep (1/60,1)
'end if
'sleep (1000/30,1)
'sleep(1000/60-temphemp,1)
'screencopy
'sleep(3,1)
'sleep (3/1.5,1)
'screencopy
'color ,rgb(0,0,0)
'screencopy
'sleep(15,1)
'screencopy 2,0
'sleep (6,1)

'screencopy 2,0
'paint(0,0),rgb(0,0,0)
'screencopy cccc,2
'sleep (1000*.0003333333333333333,1)
'screencopy
'sleep (3,1)
'Put( 40, 40 ), picture, PSet

'get(0,0)-(1280,720),picture
'rem is it genious what matters its three dee mind expansion
'cls
'counter_begin(1,HIGH_PRIORITY_CLASS,2)
'sleep
'screencopy
'if cccc=1 then cls:screencopy else cls

'sleep (33-(3/(2*24)/2),1)

 ' counter_end()
   
loop
REM cOPYRIGHTED © 2022 cOREY dENNIS wILSON
