program kitchenCheck
real :: x1,x2,x3,y1,y2,y3,a,b,c,sa,sb,sc,e,p,area
real ,dimension(3) :: s
integer :: tp
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
  print *,"y2"
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
print *,"Area of working triangle=",area," m^2"
print *,"Difference ",e
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
print *,"Check finished"    
    
end program kitchenCheck
