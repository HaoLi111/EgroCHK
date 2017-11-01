program EgroCHK
character :: f
real :: wallheight,sofawidth,latitute
print *,"=====         ///// |_| |/"
print *,"=====/\ |/ /\ \\\\\ | | |\"
print *,"=====\| |  \/"
print *,"     _|  "
print *,"EgroCHK_Egronomic check tool and calculator for design, version 1.0 by Hao Li built with FTN95 with Plato IDE"
!print *,"This is an opensourse program following AGPL license with absolutely no warranty, for more information or to contribute go to"
print *,"https://github.com/HaoLi111/EgroCHK/tree/Built-executable"
print *,"k (kitchen check);w (window sunlight design);t (tv and living room dimensions);a (sun angle calculation)"
!load default value
wallheight=2.45
sofawidth=0.7
!main panel modifying environmental variables an dcalling subsequences and functions
do
 print *,"type ? for help; p for a list of program you can run,s for settings,e to exit"
 read *,f
if (f=="e") stop
if (f=="?") then
   print *,"help"
 else if(f=="p") then
   print *,"k (kitchen check);w (window sunlight design);t (tv and living room dimensions);a (sun angle calculation)"
 else if(f=="s") then
   print *,"setup: wall height: type in, Unit: meter (type 0 for 2.45m (default))" 
   read *,wallheight
   if (abs(wallheight)<=0.000001) then
     wallheight=2.45
     print *,"set to default"
   else if (wallheight<(-0.000001)) then
       print *,"invalid wall height, reset to default"
       wallheight=2.45
   end if
   print *,"setup: chain&sofa width: type in 0 for default"
   read *,sofawidth
   if (abs(sofawidth)<=0.000001) then
     sofawidth=0.7
     print *,"set to default"
   else if (sofawidtht<(-0.000001)) then
       print *,"invalid sofa width, reset to default"
       wallheight=0.7
   end if
   print *,"wall height=",wallheight,";sofa&chair width=",sofawidth
 else if(f=="k") then
   call kitchenCheck()
 else if (f=="w") then
   call windowCalc(wallheight)
 else if (f=="t") then
   call tv(wallheight,sofawidth)
 else if (f=="a") then
   call sunDiag(wallheight)
 end if
end do
end program EgroCHK
!============================================================================================================
subroutine kitchenCheck
implicit none
real :: x1,x2,x3,y1,y2,y3,a,b,c,sa,sb,sc,e,p,area
real ,dimension(3) :: s
character :: aa*1
integer :: tp,i
!do
print *,"Select input type"
print *,"1. Sizes of 3 sides"
print *,"2. Coordinates of 3 points (x1,y1) (x2,y2) (x3,y3)"
print *,"Unit: meter"
read *,tp
if (tp==1) then
  print *,"a="
  read *,a
  print *,"b="
  read *,b
  print *,"c="
  read *,c
  end if
if (tp==2) then
  print *,"x1="
  read *,x1
  print *,"y1="
  read *,y1
  print *,"x2="
  read *,x2
  print *,"y2="
  read *,y2
  print *,"x3="
  read *,x3
  print *,"y3="
  read *,y3
  a=sqrt((y2-y3)**2+(x2-x3)**2)
  b=sqrt((y1-y3)**2+(x1-x3)**2)
  c=sqrt((y1-y2)**2+(x1-x2)**2)
  end if
print *,"(a,b,c)=(",a,",",b,",",c,")"
if ((a+b)<=c .or. abs(a-b)>=c) then
  print *,"not a triangle"
  else
sa=1.95-a
sb=1.95-b
sc=1.95-c
e=sqrt(sa**2+sb**2+sc**2)
p=(a+b+c)/2
area=sqrt(p*(p-a)*(p-b)*(p-c))
print *,"Indicator of difference from the average equilateral: (",sa,",",sb,",",sc,")"
print *,"Difference ",e
print *,"Area of working triangle=",area," m^2"
print *,"Checking dimensions of sizes"
s(1)=a
s(2)=b
s(3)=c
p=2*p
if (p>7.9) then
  print *,"perimemter too big"
else if (p<4.0) then
  print *,"perimeter too small"
end if
do i =1,3
  if (s(i)<1.2) then
    print *,"Side",i,"too short"
  else if (s(i)>2.7) then
    print *,"Side",i,"too long"
  end if
end do
end if
print *,"Check finished,type e to exit,type other keys to go to main menu" 
read *,aa
if (aa=="e") stop
!end do   
end subroutine kitchenCheck
!===============================================================================
subroutine windowCalc(wallheight)
character :: aa
real :: t1,t2,wallheight,yup,ylow,yw,yea,yeafromwall,x
integer :: sel1
print *,"Select input mode 1"
print *,"1 .Calculate ease from window"
print *,"2 .Calculate window from ease"
read *,sel1,sel3
!print *,"Select imput mode 2"
!print *,"1.Calculate from known sun angle"
!print *,"2.Calculate from latitute"
!read *,sel2
!if (sel2==2) then
!  call subroutine sunDiag(wallheight)
  t2=angle(2)
  t1=angle(4)
!  else
    print *,"winter sun deg:"!deg
    read *,t2
    print *,"Summer sun deg"
    read *,t1
 !   end if
t2=t2*3.14159/180!rad
t1=t1*3.14159/180!rad
if (sel1==1) then !e from w
  print *,"ylow="
  print *,"All height wirh respect to"
  print *,"1.	ground"
  print *,"2.	walltip"
  read *,sel3
  if (sel3==1) then!ground->wall tip
    end if
end if
print *,"Check finished,type e to exit,type other keys to go to main menu" 
read *,aa
if (aa=="e") stop
end subroutine windowCalc
!===============================================================================
subroutine tv(wallheight,sofawidth)
character :: aa
real :: wallheight,sofawidth,h1,h2,d1,d2,d,ss,score1,score2,p1,p2,h,ho,dv1,dv2,x,y,tvrat,tvdiag
integer :: i,h1i,h2i,sel1
print *,"frequency of watching while sitting(0~1)"
read *,p1
p2=1-p1
print *,"Use default human figure?	0 for yes;	 1 for no"
read *,sel1
if (sel1==0) then
  h1i=1192!((819+444)+(738+383))/2
  h2i=1799
  else
    print *,"Enter h1(Standing Viewpoint),h2(Sitting Viewpoint) in order in MM"
    read *,h1i
    read *,h2i
    end if
h2=h2i/1000
h1=h1i/1000
print *,"Enter room depth"
read *,d2
d1=d2-sofawidth
score1=d2**2
do i = h1i,h2i
  ss=p1*p1*atan((h11-i)/d1/1000)**2+p2*p2*atan((h2i-i)/d2/1000)**2
  if (ss<score1) then
    score1=ss
    ho=i
    ho=ho/1000
   !else
     !print *,"checking",i,"mm"
  end if
end do
print *,"The center of the tv is", (wallheight-ho),"from top,",ho,"from floor"
dv1=sqrt(d1**2+(h-h1)**2)
dv2=sqrt(d2**2+(h-h2)**2)
d=(dv1*p1**2+dv2*p2**2)/(p1**2+p2**2)
score2=p2**2*(dv2-d)**2+p1**2*(dv1-d)**2
print *,"type in the ratio you would like for tv diagonal: watching distance, type 0 for default(1/3)"
read *,ss
if (ss<0.00001) then
  ss=1/3
  end if
tvdiag=ss*d
print *,"type in the ratio of tv; type 0 for default (16:9)"
read *,tvrat
if (tvrat<0.00001) then
  tvrat=(16/9)
  end if
x=tvrat*y
y=sqrt(tvdiag**2/(tvrat**2+1))
print *,"Input review: TV ratio",tvrat,"watching ratio",ss,"The following are your tv results"
print *,"TV size diagonal:",tvdiag,"m","",(tvdiag/2.45),"in;",x,"m in length;",y,"in height placed centre",h,":upper rim is"
print *,(wallheight-(h+y/2)),"from top","lower rim is",h-y/2
print *,"from ground, head tilting consistency is",score1,"distance consistency is",score2,"weighted average distance",d
print *,"Check finished,type e to exit,type other keys to go to main menu" 
read *,aa
if (aa=="e") stop
end subroutine tv
!===============================================================================
subroutine sunDiag(wallheight)
character :: aa
real :: latitude
integer :: i,j
real , dimension(4) :: shadow,angle,delangle
print *,"type the latitude"
read *,latitude
delangle(1)=0
delangle(2)=23.5
delangle(3)=0
delangle(4)=-23.5
angle=90-latitude+delangle
!angle=angle*3.14159/180
shadow=sin(angle*3.14159/180)*wallheight
if (latitude<23.5)then
  print *,"Tropic Zone, this model is less useful as there is always enough sunlight"
  else if (latitude<66.5) then
    print *,"Temperate Zone"
    else
      print *,"Arctic or antarctic, but you ain't kidding are you."
      end if
print *,"	Sping;Summer;Fall;Winter"
print *,"angle",angle
print *,"shadow",shadow
!print *,t4, "type r to write"
!print *,shadow, "type r to write"
read *,aa
!if (aa=="r") then
!  write("Sunangle.csv"),
!  write("Shadow.csv"),shadow
!  end if
print *,"Check finished,type e to exit,type other keys to go to main menu" 
read *,aa
if (aa=="e") stop
end subroutine sunDiag
