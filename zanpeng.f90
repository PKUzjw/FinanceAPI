   ! include 'f_ashpanel.f90'
   ! include 'f_rouwenhorst.f90'
   ! include 'f_discretize.f90'
   ! program zanpeng
   !    implicit none
   !    integer, parameter :: singlep = kind(1.0), doublep = kind(1.0d0)
   !    integer, parameter :: fp = singlep !doublep  
   !    integer, parameter ::      agein=30
   !    integer, parameter :: nT=60, Tspan=nT-agein+21
   !    real(fp),dimension(Tspan):: Apath,Bpath,Dpath,Hpath,Mpath,Cpath
   !    real(fp),dimension(Tspan):: mApath,mBpath,mDpath,mHpath,mMpath,mCpath
   !    real(fp),dimension(Tspan):: nApath,nBpath,nDpath,nHpath,nMpath,nCpath
   !    real(fp):: theta,beta,gamma,income,muH,sigma_occu,sigma_sector,Medexp,muY
   !    real(fp):: stock, bond,deposit,househome,mortgagehome,houseINV,mortgageINV
   !    real(fp):: Htransfer,Hsupport,Cmean,cbar,edufundQ
   !    integer:: numearner,numchild,ReverseM,edufund,edufundyr
   !    integer::t

   !    theta=0.3   !relates to cjump
   !    beta=0.96   !relates to saverate
   !    gamma=4.0   !affects stockshare through stkmulti
   !    numearner=2
   !    numchild =2
   !    income=0.0 !rescale asset levels
    
   !    muH=0.06
   !    sigma_occu=0.02
   !    sigma_sector=0.02
   !    Medexp = 0.2
   !    muY = 1.01   !affects saverate and Yprofile

   !    stock=10.0
   !    bond=10.0
   !    deposit=10.0
   !    househome=400.0
   !    mortgagehome=200.0
   !    houseINV=0.0
   !    mortgageINV=0.0

   !    Htransfer = 0.0
   !    Hsupport = 0.0
   !    ReverseM = 1
   !    cmean = 4000 !not used in fortran code 
   !    cbar=2000.0  !yuan, used in simulation
   !    edufund = 0
   !    edufundQ = 0.0
   !    edufundyr = 0

    
   !   ! call allocation(gamma,theta,beta,agein,income,stock,bond,deposit,house,mortgage, &
   !    call allocation(theta,beta,gamma,agein,numearner,numchild,income, &
   !         muH,sigma_occu,sigma_sector,Medexp,muY,&
   !         stock,bond,deposit,househome,mortgagehome,houseINV,mortgageINV, &
   !         Htransfer,Hsupport,ReverseM,Cmean,Cbar,edufund,edufundQ,edufundyr, &
   !       Apath,Bpath,Dpath,Hpath,Mpath,Cpath,mapath,mbpath,mdpath,mhpath,mmpath,mcpath,&
   !       napath,nbpath,ndpath,nhpath,nmpath,ncpath)

   !    do t=1,Tspan
   !       write(*,'(9f12.4)') (/apath(t),bpath(t),cpath(t),dpath(t),hpath(t),mpath(t)/)
   !    enddo

   !    print*,'*******Average profiles'
   !    do t=1,Tspan
   !       write(*,'(9f12.4)') (/mapath(t),mbpath(t),mcpath(t),mdpath(t),mhpath(t),mmpath(t)/)
   !    enddo
   !    print*,'*******Average profiles:naive'
   !    do t=1,Tspan
   !       write(*,'(9f12.4)') (/napath(t),nbpath(t),ncpath(t),ndpath(t),nhpath(t),nmpath(t)/)
   !    enddo
   !  end program zanpeng
 
! !  Subroutine allocation(gamma,theta,beta,ageini,income,stock0,bond0,deposit0,house0,mortgage0, &
! !       Apath,Bpath,Dpath,Hpath,Mpath,Cpath,mapath,mbpath,mdpath,mhpath,mmpath,mcpath, &
! !       napath,nbpath,ndpath,nhpath,nmpath,ncpath)
  subroutine allocation(theta,beta,gamma,ageini,numearner,numchild,income0, &
       muH,sigma_occu,sigma_sector,Medexp,muY,&
       stock0,bond0,deposit0,house0,mortgage0,houseINV0,mortgageINV0, &
       Htransfer,Hsupport,ReverseM,Cmean,Cbar0,edufund,edufundQ,edufundyr, &
       Apath,Bpath,Dpath,Hpath,Mpath,Cpath,mapath,mbpath,mdpath,mhpath,mmpath,mcpath,&
       napath,nbpath,ndpath,nhpath,nmpath,ncpath)   
  implicit none
  integer, parameter :: singlep = kind(1.0), doublep = kind(1.0d0)
  integer, parameter :: fp = singlep !doublep
  real(fp),intent(in):: theta,beta,gamma,income0,muH,sigma_occu,sigma_sector,Medexp,muY
  real(fp),intent(in):: stock0, bond0,deposit0,house0,mortgage0,houseINV0,mortgageINV0
  real(fp),intent(in):: Htransfer,Hsupport,Cmean,cbar0,edufundQ
  integer,intent(in) :: ageini,numearner,numchild,ReverseM,edufund,edufundyr
  real(fp)::saverate,cjump,stkmulti,income
  !real(fp),intent(in)::gamma,theta,beta
  !real(fp),intent(in)::cbar0
  !real(fp),intent(in)::income,stock0,bond0,deposit0,house0,mortgage0
  !integer,parameter:: agein=40
  integer, parameter :: nT=60
  real(fp),dimension(nT-ageini+21),intent(out):: Apath,Bpath,Dpath,Hpath,Mpath,Cpath
  real(fp),dimension(nT-ageini+21),intent(out):: mApath,mBpath,mDpath,mHpath,mMpath,mCpath
  real(fp),dimension(nT-ageini+21),intent(out):: nApath,nBpath,nDpath,nHpath,nMpath,nCpath
  real(fp),parameter :: Rb = 1.02    !gross return of risk-free bond
  real(fp),parameter :: Rd = 1.05    !gross return of 2-year CD
  real(fp),parameter :: Rd0= 0.95    !gross return of 2-year return if withdrawn earlier

  real(fp),parameter :: muA = 0.08
  real(fp),parameter:: rM=1.05
  real(fp),parameter:: downpay=0.4
  real(fp),parameter:: sellcost=0.06, buycost=0.02, rentrate=0.02
  
  real(fp), parameter:: Bmin=0.0, Bmax=12.0
  real(fp), parameter:: Amin=0.0, Amax=10.0
  real(fp), parameter:: Wmin=0.0, Wmax=20.0
  real(fp), parameter:: Hmin=1.0, Hmax=30.0 !18.0
  real(fp), parameter:: Mmin=0.0, Mmax=20.0
  real(fp), parameter:: Dmin=0.0, Dmax=10.0
  integer,parameter:: nx=21,nx0=15
  real(fp),dimension(nx) ::adecnacoef,bdecnacoef,cdecnacoef,ddecnacoef
  real(fp),dimension(nx0)::adecacoef,bdecacoef,cdecacoef,ddecacoef,hdecacoef
  real(fp):: matcoefadj(nx0,5),matcoefna(nx,4)
  real(fp):: predictor(nx),predictoradj(nx0)
  real(fp)::age,age2,hreturn,incomenow,mortshare,wprime,cnadj,adj
  real(fp)::bond,stock,deposit,house,mortgage
  integer ::t,i,j,model,iy,ira,irh,ttoT,ii,Tspan,tpast,agein
  real(fp),dimension(nT-ageini+21,6)::path6
  real(fp):: a0,b0,d0,h0,m0,a01,b01,c01,d01,h01,hinv,m01,a1,b1,c1,d1,h1,m1
  ! mars fitting parameters
  integer,parameter:: np0=6      !number of regressors in mars fitting: w,h,m,d,hreturn,income
  integer,parameter:: nk0=21     ! maximum number of interaction to allow
  integer,parameter:: mi0=np0
  integer,parameter:: nmcv = 1,  ntcv = 1
  integer,parameter:: nfm=3+nk0*(5*mi0+nmcv+6)+2*np0+ntcv
  integer,parameter:: nim=21+nk0*(3*mi0+8)
  real(fp):: tsnew(1,np0),sp2new(1,2),vadj,vna
  real(fp),dimension(nfm,nT)::valueafmT,valuenafmT
  integer, dimension(nim,nT)::valueaimT,valuenaimT
  real(fp),dimension(nfm)::valueafm,valuenafm
  integer,dimension(nim)::valueaim,valuenaim
  ! obtain average paths
  integer,parameter::nY=3,nEps=3,nRh=3,nRa=5
  real(fp)::Yprofiletot0(70), Yprofile(nT),cbar
  real(fp):: Rhvec(nRh), Ravec(nRa), Yvec(nY)
  real(fp)::Ytran(nY,nY),Rhtran(nRh,nRh),pRavec(nRa),Ratran(nRa,nRa)
  real(fp), parameter :: sigEta=0.015_fp**0.5_fp, rho=0.9_fp  !permanent shocks
  real(fp), parameter :: sigEps=0.2000**0.5000                !transitory income shocks
  real(fp), parameter :: sigStk=0.3_fp!0.469804427065292000         !stock return shocks
  real(fp), parameter :: sigH=0.1_fp, rhoH=0.20_fp
  real(fp)::Yeps(nEps), pYeps(nEps)
  integer,parameter:: nprob=3
  integer,parameter:: HH=1000
  real :: prob(nprob,nprob), rsd
  integer,dimension(HH,nT-ageini+21):: Ystate,Rhstate,Rastate
  integer,dimension(nT-ageini+21):: Ypath,Rhpath,Rapath
  real(fp),dimension(HH,nT-ageini+21)::Apanel,Bpanel,Cpanel,Dpanel,Hpanel,Mpanel!,Adjpanel

  if (income0<=0) then
     income=0.1
  else
     income = income0
  endif
  agein= ageini-20
  cbar = (cbar0*12.0/10000.0)/income
  stock = stock0/income
  bond = bond0/income
  deposit=deposit0/income
  house=house0/income
  mortgage=mortgage0/income

  saverate = (beta/muY)**20.0_fp
  cjump    = theta/5.0_fp   !max(theta)=5.0, cjump is between 0 and 1
  stkmulti = 1.0_fp+(0.8_fp-1.0_fp)/(5.0_fp-3.0_fp)*(gamma-3.0_fp) !stkmulti=1 if gamma=3; =0.8 if gamma=5



  call f_rouwenhorst(rho,sigEta, nY, Yvec, Ytran)
  Yvec = exp(Yvec)
  Yvec = Yvec*(nY*1.0/sum(Yvec))    !ensure mean(Yvec)=1
  call  f_discretize(sigEps,nEps,Yeps,pYeps)
  Yeps = exp(Yeps)
  Yeps = Yeps*(nEps*1.0/sum(Yeps))  !ensure mean(Yeps)=1
  call f_rouwenhorst(rhoH,sigH, nRh, Rhvec, Rhtran)
  Rhvec = 1+Rhvec
  call  f_discretize(sigStk,nRa,Ravec,pRavec)
  do j=1,nRa
     Ratran(j,:)=pRavec
  enddo
  Ravec = 1+Ravec   !stock return w/o muA, with prob pRavec


  print*,'Ravec and pRavec:'
  write(*,'(10f9.5)'),Ravec
  write(*,'(10f9.5)'),pRavec
  print*,'Rhvec and Rhtran:'
  write(*,'(10f9.5)'),Rhvec
  do i=1,nRh
     write(*,'(10f9.5)'),Rhtran(i,:)
  enddo
  print*,'Yvec and Ytran:'
  write(*,'(10f9.5)'),Yvec
  do i=1,nY
     write(*,'(10f9.5)'),Ytran(i,:)
  enddo

!  stop

  Yprofiletot0= &
       (/0.7076, 0.7258, 0.7450, 0.7644, 0.7847, 0.8059, 0.8281, 0.8514, 0.8755, 0.9003, 0.9255, 0.9507, 0.9755, 0.9996, &
       1.0224, 1.0435, 1.0627, 1.0798, 1.0945, 1.1069, 1.1173, 1.1261, 1.1341, 1.1409, 1.1470, 1.1525, 1.1571, 1.1599, &
       1.1627, 1.1647, 1.1656, 1.1658, 1.1660, 1.1646, 1.1623, 1.1599, 1.1573, 1.1544, 1.1517, 1.1486, 1.1448, 1.1402, &
       1.1346, 1.1280, 1.1204, 1.1119, 1.1023, 1.0917, 1.0802, 1.0677, 1.0541, 1.0396, 1.0241, 1.0075, 0.9900, 0.9715, &
       0.9520, 0.9315, 0.9101, 0.8876, 0.8642, 0.8399, 0.8147, 0.7886, 0.7617, 0.7341, 0.7060, 0.6773, 0.6484, 0.6190  &
       /)
  do t=1,nT
     Yprofile(t)=Yprofiletot0(t)*muY**(t*1.0_fp-1.0_fp)
  enddo

   ! Rhvec= (/0.85175,  1.00000,  1.14825/)
   ! Yvec = (/0.51957,  0.90480,  1.57563/)
   ! Ravec= (/0.30939,  1.00000,  1.69061/)



    open(unit=10, file='matcoefadj.out', action='read')
    read(10,*)  matcoefadj
    close(unit=10)
    open(unit=20, file='matcoefna.out', action='read')
    read(20,*)  matcoefna
    close(unit=20)
    adecacoef=matcoefadj(:,1)
    bdecacoef=matcoefadj(:,2)
    cdecacoef=matcoefadj(:,3)
    ddecacoef=matcoefadj(:,4)
    hdecacoef=matcoefadj(:,5)
    adecnacoef=matcoefna(:,1)
    bdecnacoef=matcoefna(:,2)
    cdecnacoef=matcoefna(:,3)
    ddecnacoef=matcoefna(:,4)
    
    open(unit=10, file='valueafmT.out', action='read')
    read(10,*)  valueafmT
    close(unit=10)
    open(unit=10, file='valueaimT.out', action='read')
    read(10,*)  valueaimT
    close(unit=10)
    open(unit=10, file='valuenafmT.out', action='read')
    read(10,*)  valuenafmT
    close(unit=10)
    open(unit=10, file='valuenaimT.out', action='read')
    read(10,*)  valuenaimT
    close(unit=10)


    !modify consumption policy according to beta
    a0=0.0; b0=0.0; h0=0.0; m0=0.0; d0=0.0; hreturn=1.0; incomenow=1.0
    age=1.0; age2=1.0
    predictor  = (/1.0, age, age2, a0+b0, d0, h0, m0, hreturn*h0, incomenow, &
         age*(a0+b0), age*d0, age*h0, age*m0, age*hreturn*h0, age*incomenow, &
         age2*(a0+b0), age2*d0, age2*h0, age2*m0, age2*hreturn*h0, age2*incomenow/)
    cnadj = max(0.01,sum(cdecnacoef*predictor))
    !cdecnacoef(1) = cdecnacoef(1)*(1.0_fp-saverate)*(Yprofile(1)*incomenow/cnadj)   !recale contant terms
    cdecnacoef(1) = cdecnacoef(1)-(cnadj-(1.0_fp-saverate)*incomenow*Yprofile(1))
    if (cdecnacoef(1)<0) then
       print*,'warning: for non-adj consumption, constant term is negative, setting it to zero'
       cdecnacoef(1) = 0.0_fp
    endif

    ! a0=0.0; b0=0.0; h0=0.0; m0=0.0; d0=0.0; hreturn=1.0; incomenow=1.0
    ! predictoradj = (/1.0, age, age2, a0+b0+h0-m0, d0, hreturn*h0, incomenow, &
    !      age*(a0+b0+h0-m0), age*d0, age*hreturn*h0, age*incomenow, &
    !      age2*(a0+b0+h0-m0), age2*d0, age2*hreturn*h0, age2*incomenow/)
    ! c1 = max(0.01,sum(cdecacoef*predictoradj))
    ! !cdecacoef(1) = cdecacoef(1)*(1.0_fp-saverate)*(Yprofile(1)*incomenow/c1)   !recale contant terms
    ! cdecacoef(1) = cdecacoef(1)-(c1-(1.0_fp-saverate)*incomenow*Yprofile(1))
    ! if (cdecacoef(1)<0) then
    !    print*,'warning: for adj consumption, constant term is negative, setting it to zero'
    !    cdecacoef(1) = 0.0_fp
    ! endif
    
    
    ! find a single realization path
    rhpath = 2
    rapath = 2
    ypath  = 2

    call singlepath(stock,bond,deposit,house,mortgage,income,rhpath,ypath,rapath,nT,agein,path6, &
       Yprofile,Rhvec,nRh,Ravec,nRa,Yvec,nY,muA,Rb,Rd,muH,rM,downpay,sellcost,buycost,rentrate,cbar,&
       saverate,cjump,stkmulti,np0,nfm,nim,nx,nx0,matcoefadj,matcoefna, &
       valueafmT,valuenafmT,valueaimT,valuenaimT)

    apath = path6(:,1)
    bpath = path6(:,2)
    cpath = path6(:,3)
    dpath = path6(:,4)
    hpath = path6(:,5)
    mpath = path6(:,6)


    ! find the average path
    
    Tspan=nT-agein+1
    call f_ashpanel(Ytran,nY,  HH,Tspan,rsd, Ystate) 
    call f_ashpanel(Rhtran,nRh,HH,Tspan,rsd, Rhstate) 
    call f_ashpanel(Ratran,nRa,HH,Tspan,rsd, Rastate) 
    do ii=1,HH       
       a0 = stock
       b0 = bond
       d0 = deposit
       h0 = house
       m0 = mortgage

       rhpath = Rhstate(ii,1:nT-agein+1)
       rapath = Rastate(ii,1:nT-agein+1)
       ypath  = Ystate(ii,1:nT-agein+1)
       
       call singlepath(stock,bond,deposit,house,mortgage,income,rhpath,ypath,rapath,nT,agein,path6, &
            Yprofile,Rhvec,nRh,Ravec,nRa,Yvec,nY,muA,Rb,Rd,muH,rM,downpay,sellcost,buycost,rentrate,cbar,&
            saverate,cjump,stkmulti,np0,nfm,nim,nx,nx0,matcoefadj,matcoefna, &
            valueafmT,valuenafmT,valueaimT,valuenaimT)
       Apanel(ii,:)  = path6(:,1)
       Bpanel(ii,:)  = path6(:,2)
       Cpanel(ii,:)  = path6(:,3)
       Dpanel(ii,:)  = path6(:,4)
       Hpanel(ii,:)  = path6(:,5)
       Mpanel(ii,:)  = path6(:,6)
       !Adjpanel(ii,:)=adj
    
       ! do t=agein,nT
       !    iy  = Ystate(ii,t-agein+1)
       !    ira = Rastate(ii,t-agein+1)
       !    irh = Rhstate(ii,t-agein+1)
       !    hreturn= Rhvec(irh)

       !    incomenow = Yvec(iy)!*Yprofile(t)  ! did use Yprofile in fitting, so don't use it here
       !    age = t*1.0
       !    age2=age**2
       !    predictor  = (/1.0, age, age2, a0+b0, d0, h0, m0, hreturn*h0, incomenow, &
       !         age*(a0+b0), age*d0, age*h0, age*m0, age*hreturn*h0, age*incomenow, &
       !         age2*(a0+b0), age2*d0, age2*h0, age2*m0, age2*hreturn*h0, age2*incomenow/)
       !    predictoradj = (/1.0, age, age2, a0+b0+h0-m0, d0, hreturn*h0, incomenow, &
       !         age*(a0+b0+h0-m0), age*d0, age*hreturn*h0, age*incomenow, &
       !         age2*(a0+b0+h0-m0), age2*d0, age2*hreturn*h0, age2*incomenow/)
       !    !predictoradj=predictor
       !    ttoT = nT-t+1
       !    mortshare=(rM**(ttoT-1.0)-rM**ttoT)/(1.0-rM**ttoT)  !assume pay off before nT
          
       !    ! First of all, compare vadj with vna
       !    tsnew(1,:) = (/a0+b0,d0,h0,m0,hreturn,incomenow/)  !predictor
       !    valueafm=valueafmT(:,t)
       !    valueaim=valueaimT(:,t)
       !    call fmod(model,1,tsnew, valueafm, valueaim, vadj, sp2new)
       !    valuenafm=valuenafmT(:,t)
       !    valuenaim=valuenaimT(:,t)
       !    call fmod(model,1,tsnew, valuenafm, valuenaim, vna, sp2new)
       !    cnadj = max(cbar,sum(cdecnacoef*predictor))
       !    If (vna.ge.vadj) then
       !       c1 = cnadj
       !       !if (t==agein) then
       !       !   c1 = cnadj
       !       !else
       !       !   c1 = max(path6(t-agein,3)*0.9, min(path6(t-agein,3)*1.1,cnadj))
       !       !endif             
       !       wprime=a0+b0+Yprofile(t)*incomenow+h0*rentrate-c1-m0*mortshare
       !       a01 = sum(adecnacoef*predictor); a01=max(Amin,a01)
       !       b01 = sum(bdecnacoef*predictor); b01=max(Bmin,b01)
       !       d01 = sum(ddecnacoef*predictor); d01=max(Dmin,d01)
       !       if (a01+b01+d01>0.and.wprime>0) then
       !          a1 = wprime*a01/(a01+b01+d01)
       !          b1 = wprime*b01/(a01+b01+d01)
       !          d1 = wprime*d01/(a01+b01+d01)
       !       else
       !          vna=0.0
       !          a1=0.0; b1=0.0; c1=0.0
       !       endif
       !       h1 = h0
       !       m1 = m0*(1.0-mortshare)
       !       adj= 0.0
       !    endif
       !    if (vna<vadj) then  !so if wprime<0 above, vna will be reassigned a very small value, i.e.vna<vadj
       !       c01=max(cbar, cnadj+sum(cdecacoef*predictoradj))
       !       !if (t==agein) then
       !          c1 = c01
       !       !else
       !       !   c1 = max(path6(t-agein,3)*0.9, min(path6(t-agein,3)*1.1,c01))
       !       !endif             
       !       a01 = sum(adecacoef*predictoradj); a01=max(Amin,a01)
       !       b01 = sum(bdecacoef*predictoradj); b01=max(Bmin,b01)
       !       d01 = sum(ddecacoef*predictoradj); d01=max(Dmin,d01)
       !       h01 = sum(hdecacoef*predictoradj)
       !       if (h01.le.0) then  !exiting housing market
       !          h1=0.0; m1=0.0
       !          wprime=a0+b0+Yprofile(t)*incomenow+h0*rentrate+(1-sellcost)*h0-m0-c1
       !          if (a01+b01+d01>0) then
       !             a1 = wprime*a01/(a01+b01+d01)
       !             b1 = wprime*b01/(a01+b01+d01)
       !             d1 = wprime*d01/(a01+b01+d01)
       !          else
       !             a1=0.0; b1=0.0; c1=0.0
       !          endif
       !       elseif (h01>0) then
       !          wprime=a0+b0+Yprofile(t)*incomenow+h0*rentrate+(1-sellcost)*h0-m0-c1
       !          hinv= (downpay+buycost+(1.0-downpay)*mortshare)*h01   !current period inv in housing in order to have h01
       !          a1 = wprime*a01/(a01+b01+d01+hinv)
       !          b1 = wprime*b01/(a01+b01+d01+hinv)
       !          d1 = wprime*d01/(a01+b01+d01+hinv)
       !          h1 = ( wprime*hinv/(a01+b01+d01+hinv) )/(downpay+buycost+(1.0-downpay)*mortshare)
       !          m1 = h1*(1.0-downpay)*(1.0-mortshare)
       !       endif
       !       adj= 1.0
       !    endif
       !    Apanel(ii,t-agein+1) = a1
       !    Bpanel(ii,t-agein+1) = b1
       !    Cpanel(ii,t-agein+1) = c1
       !    Dpanel(ii,t-agein+1) = d1
       !    Hpanel(ii,t-agein+1) = h1
       !    Mpanel(ii,t-agein+1) = m1
       !    Adjpanel(ii,t-agein+1)=adj
       !    !path6(t,:)=(/a1,b1,c1,d1,h1,m1,adj/)
       !    a0 = a1*(muA+Ravec(ira))
       !    b0 = b1*Rb+d0*Rd
       !    d0 = d1*Rd
       !    h0 = h1*(muH+hreturn)
       !    m0 = m1*rM
       ! enddo
    enddo

    do t=1,Tspan
       path6(t,:)=(1.0/HH)*(/sum(Apanel(:,t)),sum(Bpanel(:,t)),sum(Cpanel(:,t)), &
            sum(Dpanel(:,t)),sum(Hpanel(:,t)),sum(Mpanel(:,t))/) !,sum(Adjpanel(:,t))/)
    enddo
    call mvaverage(path6(:,1),Tspan,6,mapath) !moving average in 4 year window
    call mvaverage(path6(:,2),Tspan,6,mbpath)
    call mvaverage(path6(:,3),Tspan,6,mcpath)
    call mvaverage(path6(:,4),Tspan,6,mdpath)
    call mvaverage(path6(:,5),Tspan,6,mhpath)
    call mvaverage(path6(:,6),Tspan,6,mmpath)
    ! mapath=mapath*income
    ! mbpath=mbpath*income
    ! mcpath=mcpath*income
    ! mdpath=mdpath*income
    ! mhpath=mhpath*income
    ! mmpath=mmpath*income
    !stop
    !naive allocation: never liquidate houses, 40% stock, 15% bond, 45% deposit
    do ii=1,HH       
       a0 = stock
       b0 = bond
       d0 = deposit
       h0 = house
       m0 = mortgage
       do t=agein,nT
          iy  = Ystate(ii,t-agein+1)
          ira = Rastate(ii,t-agein+1)
          irh = Rhstate(ii,t-agein+1)
          hreturn= Rhvec(irh)
          incomenow = Yvec(iy)!*Yprofile(t)  ! did use Yprofile in fitting, so don't use it here
          age = t*1.0
          age2=age**2
          predictor  = (/1.0, age, age2, a0+b0, d0, h0, m0, hreturn*h0, incomenow, &
               age*(a0+b0), age*d0, age*h0, age*m0, age*hreturn*h0, age*incomenow, &
               age2*(a0+b0), age2*d0, age2*h0, age2*m0, age2*hreturn*h0, age2*incomenow/)
          predictoradj = (/1.0, age, age2, a0+b0+h0-m0, d0, hreturn*h0, incomenow, &
               age*(a0+b0+h0-m0), age*d0, age*hreturn*h0, age*incomenow, &
               age2*(a0+b0+h0-m0), age2*d0, age2*hreturn*h0, age2*incomenow/)
          !predictoradj=predictor
          ttoT = nT-t+1
          mortshare=(rM**(ttoT-1.0)-rM**ttoT)/(1.0-rM**ttoT)  !assume pay off before nT
          
          ! First of all, compare vadj with vna
          tsnew(1,:) = (/a0+b0,d0,h0,m0,hreturn,incomenow/)  !predictor
          valueafm=valueafmT(:,t)
          valueaim=valueaimT(:,t)
          call fmod(model,1,tsnew, valueafm, valueaim, vadj, sp2new)
          valuenafm=valuenafmT(:,t)
          valuenaim=valuenaimT(:,t)
          call fmod(model,1,tsnew, valuenafm, valuenaim, vna, sp2new)
          cnadj = sum(cdecnacoef*predictor)
          If (vna.ge.vadj) then
             !c1 = cnadj
             !if (t==agein) then
                c1 = cnadj
             !else
             !   c1 = max(path6(t-agein,3)*0.9, min(path6(t-agein,3)*1.1,cnadj))
             !endif             
             wprime=a0+b0+Yprofile(t)*incomenow+h0*rentrate-c1-m0*mortshare
             a01 = sum(adecnacoef*predictor); a01=max(Amin,a01)
             b01 = sum(bdecnacoef*predictor); b01=max(Bmin,b01)
             d01 = sum(ddecnacoef*predictor); d01=max(Dmin,d01)
             if (a01+b01+d01>0.and.wprime>0) then
                a1 = wprime*0.4
                b1 = wprime*0.15
                d1 = wprime*0.45
             else
                vna=0.0
                a1=0.0; b1=0.0; c1=0.0
             endif
             h1 = h0
             m1 = m0*(1.0-mortshare)
             adj= 0.0
          endif
          if (vna<vadj) then  !so if wprime<0 above, vna will be reassigned a very small value, i.e.vna<vadj
             !c1 = cnadj+sum(cdecacoef*predictoradj)
             !c1 = min(c01,1.5*cnadj)
             c01=cnadj+sum(cdecacoef*predictoradj)
             !if (t==agein) then
                c1 = c01
             !else
             !   c1 = max(path6(t-agein,3)*0.9, min(path6(t-agein,3)*1.1,c01))
             !endif             
             a01 = sum(adecacoef*predictoradj); a01=max(Amin,a01)
             b01 = sum(bdecacoef*predictoradj); b01=max(Bmin,b01)
             d01 = sum(ddecacoef*predictoradj); d01=max(Dmin,d01)
             h01 = sum(hdecacoef*predictoradj)
             if (h01.le.0) then  !exiting housing market
                h1=0.0; m1=0.0
                wprime=a0+b0+Yprofile(t)*incomenow+h0*rentrate+(1-sellcost)*h0-m0-c1
                a1 = wprime*0.4
                b1 = wprime*0.15
                d1 = wprime*0.45
             elseif (h01>0) then
                wprime=a0+b0+Yprofile(t)*incomenow+h0*rentrate+(1-sellcost)*h0-m0-c1
                hinv= (downpay+buycost+(1.0-downpay)*mortshare)*h01   !current period inv in housing in order to have h01
                a1 = wprime*a01/(a01+b01+d01+hinv)
                b1 = wprime*b01/(a01+b01+d01+hinv)
                d1 = wprime*d01/(a01+b01+d01+hinv)
                h1 = ( wprime*hinv/(a01+b01+d01+hinv) )/(downpay+buycost+(1.0-downpay)*mortshare)
                m1 = h1*(1.0-downpay)*(1.0-mortshare)
                !naive allocation
                wprime = a1+b1+d1
                a1 = wprime*0.4
                b1 = wprime*0.15
                d1 = wprime*0.45
             endif
             adj= 1.0
          endif
          Apanel(ii,t-agein+1) = a1
          Bpanel(ii,t-agein+1) = b1
          Cpanel(ii,t-agein+1) = c1
          Dpanel(ii,t-agein+1) = d1
          Hpanel(ii,t-agein+1) = h1
          Mpanel(ii,t-agein+1) = m1
          !Adjpanel(ii,t-agein+1)=adj
          !path6(t,:)=(/a1,b1,c1,d1,h1,m1,adj/)
          a0 = a1*(muA+Ravec(ira))
          b0 = b1*Rb+d0*Rd
          d0 = d1*Rd
          h0 = h1*(muH+hreturn)
          m0 = m1*rM
       enddo
    enddo

    do t=1,Tspan
       path6(t,:)=(1.0/HH)*(/sum(Apanel(:,t)),sum(Bpanel(:,t)),sum(Cpanel(:,t)), &
            sum(Dpanel(:,t)),sum(Hpanel(:,t)),sum(Mpanel(:,t))/)!,sum(Adjpanel(:,t))/)
    enddo
    call mvaverage(path6(:,1),Tspan,6,napath) !moving average in 4 year window
    call mvaverage(path6(:,2),Tspan,6,nbpath)
    call mvaverage(path6(:,3),Tspan,6,ncpath)
    call mvaverage(path6(:,4),Tspan,6,ndpath)
    call mvaverage(path6(:,5),Tspan,6,nhpath)
    call mvaverage(path6(:,6),Tspan,6,nmpath)
    napath=napath*income
    nbpath=nbpath*income
    ncpath=ncpath*income
    ndpath=ndpath*income
    nhpath=nhpath*income
    nmpath=nmpath*income
    
!   do t=1,Tspan
!      write(*,'(9f12.4)') (/apath(t),bpath(t),cpath(t),dpath(t),hpath(t),mpath(t)/)
!   enddo
!   print*,'*******Average profiles'
!   do t=1,Tspan
!      write(*,'(9f12.4)') (/mapath(t),mbpath(t),mcpath(t),mdpath(t),mhpath(t),mmpath(t)/)
!   enddo
       
  end subroutine allocation
!  end program zanpeng


  subroutine mvaverage(path,np,nw,newpath)
    implicit none
    integer,intent(in):: np,nw !nw is the moving window
    real(4),intent(in):: path(np)
    real(4),intent(out)::newpath(np)
    integer:: t,nw2

    nw2 = max(1,floor(nw*0.5))    
    do t=1,np
       if (t.le.nw2) then
          newpath(t) = sum(path(1:t+nw2))*(1.0/(t+nw2))
       elseif (t.gt.(np-nw2)) then
          newpath(t) = sum(path(t-nw2:np))*(1.0/(1.0+np-(t-nw2)))
       else
          newpath(t) = sum(path(t-nw2:t+nw2))*(1.0/(1.0+2.0*nw2))
       endif
    enddo
  end subroutine mvaverage


  subroutine singlepath(stock,bond,deposit,house,mortgage,income,rhpath,ypath,rapath,nT,agein,path6, &
       Yprofile,Rhvec,nRh,Ravec,nRa,Yvec,nY,muA,Rb,Rd,muH,rM,downpay,sellcost,buycost,rentrate,cbar,&
       saverate,cjump,stkmulti,np0,nfm,nim,nx,nx0,matcoefadj,matcoefna, &
       valueafmT,valuenafmT,valueaimT,valuenaimT)
    implicit none
    integer, parameter :: singlep = kind(1.0), doublep = kind(1.0d0)
    integer, parameter :: fp = singlep !doublep    
    integer, intent(in) :: nT,agein,nx,nx0,np0,nfm,nim,nRh,nRa,nY
    integer,dimension(nT-agein+1),intent(in):: Ypath,Rhpath,Rapath
    real(fp),intent(in) :: stock,bond,deposit,house,mortgage,income
    real(fp),intent(in) :: Rhvec(3),Ravec(3),Yvec(3),Yprofile(nT),muA,Rb,Rd,muH,rM
    real(fp),intent(in) :: downpay,sellcost,buycost,rentrate,cbar
    real(fp),intent(in) :: saverate,cjump,stkmulti
    real(fp),intent(in) :: matcoefadj(nx0,5),matcoefna(nx,4)
    real(fp),dimension(nfm,nT),intent(in)::valueafmT,valuenafmT
    integer, dimension(nim,nT),intent(in)::valueaimT,valuenaimT
    real(fp),intent(out):: path6(nT-agein+1,6)
    real(fp),dimension(nx) ::adecnacoef,bdecnacoef,cdecnacoef,ddecnacoef
    real(fp),dimension(nx0)::adecacoef,bdecacoef,cdecacoef,ddecacoef,hdecacoef
    real(fp):: predictor(nx),predictoradj(nx0)
    real(fp)::age,age2,hreturn,incomenow,mortshare,wprime,cnadj,adj
    integer ::t,i,j,model,ttoT,ii,Tspan,tpast,irh,ira,iy
    real(fp):: a0,b0,d0,h0,m0,a01,b01,c01,d01,h01,hinv,m01,a1,b1,c1,d1,h1,m1
    real(fp), parameter:: Bmin=0.0
    real(fp), parameter:: Amin=0.0
    real(fp), parameter:: Wmin=0.0
    real(fp), parameter:: Hmin=1.0
    real(fp), parameter:: Mmin=0.0
    real(fp), parameter:: Dmin=0.0    
    ! mars fitting parameters
    real(fp):: tsnew(1,np0),sp2new(1,2),vadj,vna
    real(fp),dimension(nfm)::valueafm,valuenafm
    integer,dimension(nim)::valueaim,valuenaim
    ! obtain average paths
    
    
    adecacoef=matcoefadj(:,1)
    bdecacoef=matcoefadj(:,2)
    cdecacoef=matcoefadj(:,3)
    ddecacoef=matcoefadj(:,4)
    hdecacoef=matcoefadj(:,5)
    adecnacoef=matcoefna(:,1)
    bdecnacoef=matcoefna(:,2)
    cdecnacoef=matcoefna(:,3)
    ddecnacoef=matcoefna(:,4)
    
    !modify consumption policy according to beta
    a0=0.0; b0=0.0; h0=0.0; m0=0.0; d0=0.0; hreturn=1.0; incomenow=1.0
    age=1.0; age2=1.0
    predictor  = (/1.0, age, age2, a0+b0, d0, h0, m0, hreturn*h0, incomenow, &
         age*(a0+b0), age*d0, age*h0, age*m0, age*hreturn*h0, age*incomenow, &
         age2*(a0+b0), age2*d0, age2*h0, age2*m0, age2*hreturn*h0, age2*incomenow/)
    cnadj = max(0.01,sum(cdecnacoef*predictor))
    !cdecnacoef(1) = cdecnacoef(1)*(1.0_fp-saverate)*(Yprofile(1)*incomenow/cnadj)   !recale contant terms
    cdecnacoef(1) = cdecnacoef(1)-(cnadj-(1.0_fp-saverate)*incomenow*Yprofile(1))
    if (cdecnacoef(1)<0) then
       print*,'warning: for non-adj consumption, constant term is negative, setting it to zero'
       cdecnacoef(1) = 0.0_fp
    endif

    
    path6=0.0
    a0 = stock
    b0 = bond
    d0 = deposit
    h0 = house
    m0 = mortgage
    do t=agein,nT
       iy  = Ypath(t-agein+1)
       irh = Rhpath(t-agein+1)
       ira = Rapath(t-agein+1)
       incomenow = Yvec(iy)!*Yprofile(t)  ! did use Yprofile in fitting, so don't use it here
       hreturn= Rhvec(irh)
       age = t*1.0
       age2=age**2
       predictor  = (/1.0, age, age2, a0+b0, d0, h0, m0, hreturn*h0, incomenow, &
            age*(a0+b0), age*d0, age*h0, age*m0, age*hreturn*h0, age*incomenow, &
            age2*(a0+b0), age2*d0, age2*h0, age2*m0, age2*hreturn*h0, age2*incomenow/)
       predictoradj = (/1.0, age, age2, a0+b0+h0-m0, d0, hreturn*h0, incomenow, &
            age*(a0+b0+h0-m0), age*d0, age*hreturn*h0, age*incomenow, &
            age2*(a0+b0+h0-m0), age2*d0, age2*hreturn*h0, age2*incomenow/)
       !predictoradj=predictor
       ttoT = nT-t+1
       mortshare=(rM**(ttoT-1.0)-rM**ttoT)/(1.0-rM**ttoT)  !assume pay off before nT

       ! First of all, compare vadj with vna
       tpast = max(t-agein-7.0,1.0) !past 7 years or less
       if (sum(path6(tpast:t-agein,7))>0) then  !recently adjusted
          vna=1.0; vadj=0.0
       elseif (t==agein.and. h0/(a0+b0+d0+h0)<0.96) then    !housing share not too high
          vna=1.0; vadj=0.0
       else
          tsnew(1,:) = (/a0+b0,d0,h0,m0,hreturn,incomenow/)  !predictor
          valueafm=valueafmT(:,t)
          valueaim=valueaimT(:,t)
          call fmod(model,1,tsnew, valueafm, valueaim, vadj, sp2new)
          valuenafm=valuenafmT(:,t)
          valuenaim=valuenaimT(:,t)
          call fmod(model,1,tsnew, valuenafm, valuenaim, vna, sp2new)
       endif
       cnadj = max(cbar,sum(cdecnacoef*predictor))

       If (vna.ge.vadj) then
          if (t==agein) then
             c1 = cnadj
          else
             c1 = max(path6(t-agein,3)*0.95, min(path6(t-agein,3)*1.05,cnadj))
          endif
          wprime=a0+b0+Yprofile(t)*incomenow+h0*rentrate-c1-m0*mortshare
          a01 = stkmulti*sum(adecnacoef*predictor); a01=max(Amin,a01)
          b01 = sum(bdecnacoef*predictor); b01=max(Bmin,b01)
          d01 = (1.0_fp/stkmulti)*sum(ddecnacoef*predictor); d01=max(Dmin,d01)
          if (d01>0.and.a01==0.and.b01==0) then
             d01 = 0.3*d01
             a01 = 0.7*d01 !must have some liquid asset
          endif
          if (a01+b01+d01>0.and.wprime>0) then
             a1 = wprime*a01/(a01+b01+d01)
             b1 = wprime*b01/(a01+b01+d01)
             d1 = wprime*d01/(a01+b01+d01)
          else
             vna=0.0;vadj=1.0
             a1=0.0; b1=0.0; c1=0.0
          endif
          h1 = h0
          m1 = m0*(1.0-mortshare)
          adj= 0.0
       endif
       if (vna<vadj) then  !so if wprime<0 above, vna will be reassigned a very small value, i.e.vna<vadj
          a01 = stkmulti*sum(adecacoef*predictoradj); a01=max(Amin,a01)
          b01 = sum(bdecacoef*predictoradj); b01=max(Bmin,b01)
          d01 = (1.0_fp/stkmulti)*sum(ddecacoef*predictoradj); d01=max(Dmin,d01)
          h01 = sum(hdecacoef*predictoradj)
          !c01=max(cbar,cnadj+sum(cdecacoef*predictoradj))
          !if (t==agein) then
          !   c1 = c01
          !else
          !   c1 = max(cbar, max(path6(t-agein,3)*0.95, min(path6(t-agein,3)*1.05,c01)))
          !endif
          if (h01>h0) then  !adjust up, cut consumption
             c1 = max(cbar,cnadj/(1.0_fp+cjump))
          else              !adjust down, increase consumption
             c1 = max(cbar,cnadj*(1.0_fp+cjump))
          endif          
          if (h01.le.0) then  !exiting housing market
             h1=0.0; m1=0.0
             wprime=a0+b0+Yprofile(t)*incomenow+h0*rentrate+(1-sellcost)*h0-m0-c1
             if (a01+b01+d01>0.0) then
                a1 = wprime*a01/(a01+b01+d01)
                b1 = wprime*b01/(a01+b01+d01)
                d1 = wprime*d01/(a01+b01+d01)
             else
                a1=0.0; b1=0.0; c1=0.0
             endif
          elseif (h01>0) then
             wprime=a0+b0+Yprofile(t)*incomenow+h0*rentrate+(1-sellcost)*h0-m0-c1 
             hinv= (downpay+buycost+(1.0-downpay)*mortshare)*h01   !current period inv in housing in order to have h01
             a1 = wprime*a01/(a01+b01+d01+hinv)
             b1 = wprime*b01/(a01+b01+d01+hinv)
             d1 = wprime*d01/(a01+b01+d01+hinv)
             h1 = ( wprime*hinv/(a01+b01+d01+hinv) )/(downpay+buycost+(1.0-downpay)*mortshare)
             m1 = h1*(1.0-downpay)*(1.0-mortshare)
          endif
          adj= 1.0
       endif
       !a1 = max(Amin,a1)
       !b1 = max(Bmin,b1)
       !d1 = max(Dmin,d1)
       path6(t-agein+1,:)=(/a1,b1,c1,d1,h1,m1/)
       a0 = a1*(muA+Ravec(ira))
       b0 = b1*Rb+d0*Rd
       d0 = d1*Rd
       h0 = h1*(muH+hreturn)
       m0 = m1*rM
    enddo
    path6(:,1:6) = path6(:,1:6)*income


  end subroutine singlepath
