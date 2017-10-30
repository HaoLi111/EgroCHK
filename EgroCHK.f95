program EgroCHK
character :: f
real :: wallheight
print *,"=====         ///// |_| |/"
print *,"=====/\ |/ /\ \\\\\ | | |\"
print *,"=====\| |  \/"
print *,"     _|  "
print *,"EgroCHK-Egronomic check tool and calculator for design, version 1.0 by Hao Li"
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
 else if(f=="k") then
   call kitchenCheck()
 else if (f=="w") then
   call windowCalc(wallheight)
 else if (f=="t") then
   call tv(wallheight)
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
print *,"Check finished,type e to exit,type other keys to go to main menu" 
read *,aa
if (aa=="e") stop
!end do   
end subroutine kitchenCheck
!===============================================================================
subroutine windowCalc(wallheight)
character :: aa
print *,"Check finished,type e to exit,type other keys to go to main menu" 
read *,aa
if (aa=="e") stop
end subroutine windowCalc
!===============================================================================
subroutine tv(wallheight)
character :: aa
print *,"Check finished,type e to exit,type other keys to go to main menu" 
read *,aa
if (aa=="e") stop
end subroutine tv
!===============================================================================
subroutine sunDiag(wallheight)
character :: aa
real :: latitude
integer :: i,j
real , dimension(4,24) :: t1,t2,t3,t4,shadow
print *,"type the latitude"
read *,latitude
do i=2,4,1
  do j=1,12,1
  t1(i,j)=abs((i-3)*23.5+abs(latitude))!deg
  t2(i,j)=abs((j/24-1/2)*2*3.14159)!rad
  t3(i,j)=atan(abs(tan(t1(i,j)*3.1415/180))/abs(tan(t2(i,j))))!rad
  t4(i,j)=atan(cos(t1(i,j)*3.14159/180)/sqrt((cos(t3(i,j))**2)+cos(t1(i,j)*3.1415/180)**2))!rad
  shadow=sin(t4(i,j))*wallheight
  t4(i,(24-j))=t4(i,j)!rad
  t3(i,j)=t3(i,j)*180/3.14159
  t3(i,(24-j))=t3(i,j)
  shadow(i,(24-j))=shadow(i,j)
  end do
end do
do j=1,24,1
  t3(1,j)=t3(3,j)!spring-fall
  t4(1,j)=t4(3,j)
  end do
if (latitude<23.5)then
  print *,"Tropic Zone"
  else if (latitude<66.5) then
    print *,"Temperate Zone"
    else
      print *,"You ain't kidding are you."
      end if
print *,t4, "type r to write"
print *,shadow, "type r to write"
read *,aa
!if (aa=="r") then
!  write("Sunangle.csv"),
!  write("Shadow.csv"),shadow
!  end if
print *,"Check finished,type e to exit,type other keys to go to main menu" 
read *,aa
if (aa=="e") stop
end subroutine sunDiag