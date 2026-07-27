program KDENWeatherAviation

implicit none


!Today ill be creating a program to Analyze atmospheric conditions important to Denver International Airport operations.
!--------------------------------------------------
!Project Info
!--------------------------------------------------


! Denver Airport Operations Assessment
! February 15 2021 Arctic Cold Outbreak at 12 UTC 
! Data: University of Wyoming Sounding Data Reanalysis
! Location: Denver, CO


!--------------------------------------------------
!Data Dictonary 
!--------------------------------------------------

character(len=200) :: line
character(len=20) :: icing_flag,snow_flag,shear_flag,redflag_flag
character(len=30) :: precip_flag
real :: pres,height,temp,dew,rh,mixr,winddir,windspd
real :: sfc_temp
real :: sfc_wind
real :: sfc_height
real :: sfc_rh
real :: temp700
real :: rh700
real :: wind700
real :: dir700
real :: height700
real :: temp550
real :: rh550
real :: wind550
real :: shear700
real :: shear550
real :: dgz_bottom
real :: dgz_top

!--------------------------------------------------
!Initialize Variables
!--------------------------------------------------
!this is where ill give all the variables that are used in a calculaiton a default value so it cant be assumed.

dgz_bottom = 99999.0
dgz_top    = 0.0
sfc_height = 0.0
sfc_temp = -999.0
sfc_wind = -999.0
sfc_rh = -999.0
temp700 = -999.0
rh700 = -999.0
wind700 = -999.0
dir700 = -999.0

temp550 = -999.0
rh550 = -999.0
wind550 = -999.0



!----------------------------------------------------------------------------------------------
! Data Readin Section
!--------------------------------------------------
!This is where ill open the sounding file, skip the header information, then assign names for all the variables found in that data.


open(unit=10,file="DenverSounding.txt",status="old",action="read")


! Skip header until sounding data begins

do
read(10,'(A)',end=100) line
if(index(line,"PRES") > 0) then

read(10,'(A)') line
read(10,'(A)') line
exit


endif
enddo




! Read sounding data



do
read(10,*,end=100) pres,height,temp,dew,rh,mixr,winddir,windspd

! Surface level
    

if(sfc_height == 0.0 .and. pres >= 800.0 .and. pres <= 900.0) then

sfc_temp   = temp
sfc_wind   = windspd
sfc_rh     = rh
sfc_height = height
       
endif




! 700 mb Level


if(pres >=690.0 .and. pres <=710.0) then

temp700 = temp
rh700 = rh
wind700 = windspd
dir700 = winddir
height700 = height
       
endif



! 550 mb Level
   

if(pres >=530.0 .and. pres <=570.0) then

temp550 = temp
rh550 = rh
wind550 = windspd
       
endif



! Dendritic Growth Zone



if(temp <= -12.0 .and. temp >= -18.0) then

if(height < dgz_bottom) then

dgz_bottom = height

endif


if(height > dgz_top) then

dgz_top = height

endif

endif

enddo



100 continue

close(10)


!now Ill open an outputfile
open(unit=20,file="KDENHazardReport.txt", &
     status="replace",action="write")
!----------------------------------------------------------------------------------------------
! Calculations
!----------------------------------------------------------------------------------------------
! This is where I calculate what the shear would be at different heights in the atmosphere.



shear700 = wind700 - sfc_wind


shear550 = wind550 - sfc_wind







!----------------------------------------------------------------------------------------------
! Output Section / Hazard Flags
!----------------------------------------------------------------------------------------------
! This is where ill create what data will be shown in the terminal as well as place them in the output file.



write(*,*)
write(*,*) "------------------------------------------------           "
write(*,*) "              KDEN HAZARD FINDINGS                         "
write(*,*) "------------------------------------------------           "



! Aircraft Icing Flag

if(temp700 <= 0.0 .and. temp700 >= -20.0 .and. rh700 >= 70.0) then

    icing_flag = "POTENTIAL"

else

    icing_flag = "LOW"

endif



! Snow Growth Flag

if(dgz_bottom < 99999.0) then

    snow_flag = "DGZ PRESENT"

else

    snow_flag = "DGZ NOT DETECTED"

endif

! Heavy Precip Flag


if(rh700 >= 90.0 .and. rh550 >= 85.0 .and. sfc_rh >= 95.0) then

    precip_flag = "GROUND STOP POTENTIAL"


else if(rh700 >= 85.0 .and. rh550 >= 80.0 .and. sfc_rh >= 90.0) then

    precip_flag = "HIGH"


else if(rh700 >= 75.0 .and. rh550 >= 70.0) then

    precip_flag = "MODERATE"


else

    precip_flag = "LOW"

endif

! Wind Shear Flag

if(shear550 >= 10.0) then

    shear_flag = "HIGH"


else if(shear550 >= 5.0) then

    shear_flag = "MODERATE"


else

    shear_flag = "LOW"

endif

! Red Flag Fire Weather

if(sfc_temp >= 25.0 .and. &
   sfc_rh <= 15.0 .and. &
   sfc_wind >= 15.0) then

    redflag_flag = "RED FLAG"

else

    redflag_flag = "NO RED FLAG"

endif




write(*,*)
write(*,*) "Aircraft Icing:"
write(*,*) "    ", icing_flag

write(*,*)
write(*,*) "Snow Growth:"
write(*,*) "    ", snow_flag

write(*,*)
write(*,*) "Wind Shear:"
write(*,*) "    ", shear_flag

write(*,*)
write(*,*) "Fire Weather:"
write(*,*) "    ", redflag_flag


write(*,*)
write(*,*) "Heavy Precipitation:"
write(*,*) "    ", precip_flag

!This is where ill write the contents of the output file.
write(20,*)
write(20,*) "------------------------------------------------"
write(20,*) "              KDEN HAZARD FINDINGS"
write(20,*) "------------------------------------------------"

write(20,*)
write(20,*) "Aircraft Icing:"
write(20,*) "    ", icing_flag

write(20,*)
write(20,*) "Snow Growth:"
write(20,*) "    ", snow_flag

write(20,*)
write(20,*) "Wind Shear:"
write(20,*) "    ", shear_flag

write(20,*)
write(20,*) "Fire Weather:"
write(20,*) "    ", redflag_flag

write(20,*)
write(20,*) "Heavy Precipitation:"
write(20,*) "    ", precip_flag

close(20)

End program KDENWeatherAviation