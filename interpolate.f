C CHUONG TRINH NOI SUY DO SAU
C F1 : file so lieu do sau convert tu file .grd cua surfer (voi khoang cach dx,dy)
C F2: file toa do can noi suy do sau voi format nhu sau:
C Ndiem ( noi suy)
C X, Y
C ....
C F3: ten file ket qua
C File Err.dat dung de xem gia tri cac diem khong noi suy duoc

      Parameter(NNX=13136) ! '166260'                  
      Dimension z(NNX),x(NNX),y(NNX),xnew(NNX), ynew(NNX),ier(150)
      Character*12 F1,F2,F3
      Character*1 txt

      write(*,*)
      write(*,*) '                   CHUONG TRINH NOI SUY DO SAU   '
      Write(*,*) '                   =========================== '
      write(*,*)

      write(*,110)
      read(*,'(a)')F1
      write(*,112)
      read(*,'(a) ')F2
      write(*,114)
      read(*,'(a)')F3

      open(45,file='err.dat')
      open(50,file=F1,status='old')
      open(40,file=F2,status='old')
      open(52,file=F3)




c Doc so lieu
      Read(50,'(a)') txt
      Read(50,*) Nx,Ny
      Read(50,*) xd,xc
      Read(50,*) yd,yc
      Read(50,'(a)') txt
c Kiem tra bo nho
      if((Nx*Ny).gt.NNX) then
      write(*,28) Nx*Ny,NNX
28    format(5x,///,6x, ' So diem day data co: ',I5,
     &',toi da chi duoc :',i5)
      stop
      endif

      dx= (xc-xd)/(Nx-1)
      dy= (yc-yd)/(Ny-1)
      Do 10 Iy=1,Ny
      Id = (Iy-1)*Nx + 1
      Ic = Iy*Nx
      Read(50,*) (z(i),I=Id,Ic)
      Do 11 I=Id,Ic
      x(i) = xd + (I-Id)*dx
      y(i) = yd + (Iy-1)*dy
c18    format(2x,'i= ',i3,2(1x,f7.1),1x,f5.2)
11    Continue
10    Continue
5     NN = Nx*Ny
      Read(40,*)NNew
      if(NNew.gt.NNX) then
      write(*,29) NNew,NNX
29    format(5x, ' So diem noi suy: ',I5, ', toi da chi duoc :',i5)
      stop
      endif
      Do 20 i = 1,NNew
      read(40,*) xnew(i),ynew(i)
20    Continue


      Do 22 jn = 1,5
22    write(*,*)
      write(*,23) NN,nnew
23    format('      So diem co data : ',i5,', so diem can noi suy: ',i5)
      write(*,*)
      write(*,*)




c Noi suy do sau
      k = 0
      Do 30 i = 1,nnew
      i1 = 0
      i2 = 0
      i3 = 0
      i4 = 0
      insuy = 0
      xx = xnew(i)
      yy = ynew(i)
74    format(2x,' xx= ',f7.2,' yy = ',f7.2g
            
C Xac dinh vi tri hinh chu nhat chua diem noi suy
      Do 40 ii=1,NN
      ddx = xx - x(ii)
      ddy = yy - y(ii)
      if((ddx.eq.0).and.(ddy.eq.0)) then
      zz = z(ii)
      insuy = 1
      goto 200
      else if((abs(ddx).le.dx).and.(abs(ddy).le.dy)) then
c      write(*,*) 'ii = ',ii,' x,y= ' ,x(ii),y(ii)
      if((ddx.ge.0).and.(ddy.ge.0)) i1 = ii
      if((ddx.le.0).and.(ddy.ge.0)) i2 = ii
      if((ddx.le.0).and.(ddy.le.0)) i3 = ii
      if((ddx.ge.0).and.(ddy.le.0)) i4 = ii
      endif
40    continue
76    format(2x,'i1,i2,i3,i4 = ', 4(1x,i3))
      if(i1.eq.0.or.i2.eq.0.or.i3.eq.0.or.i4.eq.0) goto 62
c Xac dinh tam giac dinh i1,i2,i3 chua diem noi suy
      dd = x(i2)-xx
      dm = (y(i3)- yy)*dx/dy
      if(dd.gt.dm) i2 = i4
      x1 = x(i1)
      y1 = y(i1)
      z1 = z(i1)
      x2 = x(i2)
      y2 = y(i2)
      z2 = z(i2)
      x3 = x(i3)
      y3 = y(i3)
      z3 = z(i3)
c Noi suy do sau qua 3 diem i1,i2,i3
      zz = depth(x1,y1,z1,x2,y2,z2,x3,y3,z3,xx,yy)
200   continue
70    format(2x,'xx = ',f8.2,' yy = ',f8.2)
72    format(2x,'i1,i2,i3 = ',3(1x,i3),/,
     * 2x,'x1 =',f7.2,' y1 =',f7.2,' z1 =',f5.2,/,
     * 2x,'x2 =',f7.2,' y2 =',f7.2,' z2 =',f5.2,/,
     * 2x,'x3 =',f7.2,' y3 =',f7.2,' z3 =',f5.2/,2x,' zz = ',f5.2)
      insuy = 1
      write(52,60)xx,yy,zz
60    format(2x,2(f7.2,1x),f6.2)
62    continue
      if(insuy.eq.0) then
      k = k+1
      ier(k) = i
      endif
      rdo =(real(i)/nnew)*100
      write(*,68) rdo
68    FORMAT(1H+,'     Thuc hien  = ',f4.0,'%')
30    continue
c      write(52,61) k
      write(45,61) k
      if(k.ne.0) then
      write(*,116) k
116   format(2x,'    Co ',i3,' diem khong noi suy duoc,xem ',
     &' file err.dat')
      endif
      write(*,*)
      write(*,*)
61    format(2x,'    Co ',i3,' diem khong noi suy duoc ')
      Do 32 j = 1,k
32    write(45,65) xnew(ier(j)),ynew(ier(j))


110   format(//,'    Nhap ten file data day dung de noi suy : ',/)
112   format(/, '    Nhap ten file data x,y can noi suy day    : ',/)
114   format(/, '    Nhap ten file data ket qua    : ',/)
65    format(2(2x,f7.2))
      close(40)
      close(45)
      close(50)
      close(52)
      stop
      end




c Function noi suy
      function depth(x1,y1,z1,x2,y2,z2,x3,y3,z3,xx,yy)

      if (z2.eq.min(z1,z2,z3)) then
       a = x1
       b = y1
       c = z1
      x1 = x2
      y1 = y2
      z1 = z2
      x2 = a
      y2 = b
      z2 = c
      else if(z3.eq.min(z1,z2,z3)) then
       a = x3
       b = y3
       c = z3
      x1 = x3
      y1 = y3
      z1 = z3
      x3 = a
      y3 = b
      z3 = c
      endif
c
      if((xx.eq.x1.and.xx.eq.x2).or.(xx.eq.x1.and.xx.eq.x3).
     & or.(xx.eq.x2.and.xx.eq.x3)) goto 300
      if((yy.eq.y1.and.yy.eq.y2).or.(yy.eq.y1.and.yy.eq.y3).
     & or.(yy.eq.y2.and.yy.eq.y3)) goto 400


      dz2 = z2 - z1
      dz3 = z3 - z1
      if(x2.ne.x3)then
      a23 = (y2 - y3)/(x2-x3)
      endif
      b23 = y2 - a23*x2
      a1n = (y1 - yy)/(x1-xx)
      b1n = y1 - a1n*x1
      if(x2.ne.x3) then
      xm = (b23 - b1n)/(a1n - a23)
      else
      xm=x2
      endif
      ym = a1n*xm + b1n
c      write(45,*) ' xm = ',xm,' ym = ',ym
      sm2 = side(xm,ym,x2,y2)
      sm3 = side(xm,ym,x3,y3)
      s23 = sm2 + sm3
c      write(45,*) 's23= ',s23

      if (dz3.GT.dz2)then
      dzm = dz2 + sm2*(dz3-dz2)/s23
      else
      dzm = dz3 + sm3*(dz2-dz3)/s23
      endif
      snm = side(xx,yy,xm,ym)
      sn1 = side(xx,yy,x1,y1)
      sm1 = snm + sn1
      depth = z1 + sn1*dzm/sm1
      goto 500

300   continue
      if(x1.eq.x2.and.y1.ne.y2) then
         if(z1.le.z2) depth = z1+(yy-y1)*(z2-z1)/(y2-y1)
         if(z2.le.z1) depth = z2+(yy-y2)*(z1-z2)/(y1-y2)
      else if(x2.eq.x3.and.y2.ne.y3) then
         if(z2.le.z3) depth = z2+(yy-y2)*(z3-z2)/(y3-y2)
         if(z3.le.z2) depth = z3+(yy-y3)*(z2-z3)/(y2-y3)
      else if(x1.eq.x3.and.y1.ne.y3) then
         if(z1.le.z3) depth = z1+(yy-y1)*(z3-z1)/(y3-y1)
         if(z3.le.z1) depth = z3+(yy-y3)*(z1-z3)/(y1-y3)
      endif
      goto 500

400   continue
      if(y1.eq.y2.and.x1.ne.x2) then
         if(z1.le.z2) depth = z1+(xx-x1)*(z2-z1)/(x2-x1)
         if(z2.le.z1) depth = z2+(xx-x2)*(z1-z2)/(x1-x2)
      else if(y2.eq.y3.and.x2.ne.x3) then
         if(z2.le.z3) depth = z2+(xx-x2)*(z3-z2)/(x3-x2)
         if(z3.le.z2) depth = z3+(xx-x3)*(z2-z3)/(x2-x3)
      else if(y1.eq.y3.and.x1.ne.x3) then
         if(z1.le.z3) depth = z1+(xx-x1)*(z3-z1)/(x3-x1)
         if(z3.le.z1) depth = z3+(xx-x3)*(z1-z3)/(x1-x3)
      endif
      goto 500

500   continue

c      write(45,*) 'zz= ',depth
      return
      end


c Function tinh canh hinh vuong
      function side(x1,y1,x2,y2)
      side = sqrt((x1-x2)*(x1-x2)+(y1-y2)*(y1-y2))
      return
      end


