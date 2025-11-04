
      SUBROUTINE LOAD_SPEC(CAP_TXINO,HKIP,BL_SNOW,BL_VL_SNOW,FEJL)

      IMPLICIT NONE

      INTEGER   CAP_TXINO
      INTEGER   I,IPOPV,ILOC,TXINO,VX_SUM,J
      INTEGER   XMIN,XMAX,YMIN(2),YMAX(2)
      INTEGER   BL_SNOW,NBR,W
      REAL      HKIP,BL_VL_SNOW
      REAL      FSCTN
      CHARACTER TEXTX*80,VB0_TXT*15,VX_LIST*16
      LOGICAL   FEJL,BigSpanFacilCheck
      LOGICAL   NATLASTDEF,FSTRUG
      INTEGER   frictionLength_int
      CHARACTER frictionLength_char*6
      INTEGER   FindFrictionLength,InstallationNumber

      INTEGER,EXTERNAL:: CB_BUT1,CB_BUT2,CB_BUT3,CB_BUT4,CB_BUT5,&
                         CB_SNEV_DEF_WREP,CB_SNEV_DEF,CB_SNEV_LS_DEF,&
                         CB_SNEH_DEF_WREP,CB_SNEH_DEF,CB_SNEH_LS_DEF,&
                         CB_SNEG_DEF_WREP,CB_SNEG_DEF,CB_QSNE_DEP_DEF

      INTEGER,EXTERNAL:: CB_BIG_SPAN_WREP

      INCLUDE 'GENERL.INS'
      INCLUDE 'RAMDAT.INS'
      INCLUDE 'FLSKIB.INS'
      INCLUDE 'NATLAST.INS'
      INCLUDE 'LVLAST.INS'
      INCLUDE 'TKLAST.INS'
      INCLUDE 'KLAST.INS'
      INCLUDE 'FID.INS'
      INCLUDE 'LOCAL.INS'

      COMMON /SETTINGS/InstallationNumber

      W=8
      Accelerate1=0
      SS_NLAST_SPEC=2

!      IF (NORM=='EC'.AND.INAX==1) THEN
!        BMP_NAME='snow_taller_building_DS'
!      ELSE
        BMP_NAME='snow_taller_building_EN'
!      ENDIF

!     Multi-aisle building, check for exi of trough shaped roof

      CALL FSTRUGEXI(FSTRUG)

      CALL SET_FLAT_ROOF

      IF (NORM=='EC'.AND.INAX==11.AND.FLAT_ROOF.AND.(BL_SNOW>10000.OR.LL>10000)) THEN
        BS_P5_10=.TRUE.
      ELSE
        BS_P5_10=.FALSE.
      ENDIF
      IF (NORM=='EC'.AND.INAX==11) SNOW_CONDITION=1

!     Code default parameters

      IF (NORM=='RU'.OR.FLAT_ROOF) THEN
        FSCTN=.04
      ELSE
        FSCTN=0.
      ENDIF

      SNOW_COND3_2=.FALSE.
      IF (VLV>=5.) SNOW_COND3_2(1)=.TRUE.
      IF (VLH>=5.) SNOW_COND3_2(2)=.TRUE.

      NATLASTDEF=.FALSE.

    5 CALL CALQB(NORM,INAX,HKIP,TERKLA,VKDIST,VBASIS,WINDZONE,SF_VBASIS,ALTITUDE,&
                 NS_NLAST_SPEC,NS_KOMMUNE,NS_VBASIS,SS_NLAST_SPEC,SS_KOMMUNE,SS_VBASIS,&
                 BS_VBMAP,BS_DSHORE,BS_DTOWN,QVINDN,VB0_TXT)
      CALL DEFQSNE(NORM,INAX,SNOWZONE,ALTITUDE,SF_QSNE,NS_NLAST_SPEC,NS_KOMMUNE,NS_QSNE,&
                   SS_NLAST_SPEC,SS_KOMMUNE,SS_QSNE,OEN_QSNE,QSNEN)

      IF (LASTDEF.OR.NATLASTDEF) THEN

        QVIND=QVINDN
        VINDFAC='B'
        QSNE=QSNEN

        IF (FSHAL=='J') THEN
          FSCT=FSCTN
        ELSE
          SNOW_CONDITION=1
          SNOW_ACC_ALFAS(1)=VLV
          SNOW_ACC_ALFAS(2)=ABS(VLH)
        ENDIF

      ENDIF

      CALL DEFSFORM
      CALL SNOW_EXPO_LEE(NORM,INAX,HALTYP,TERKLA,TOPOGRAFI,BORI,&
                         SNOW_CONDITION(1),SNOW_CONDITION(2),LL,BL_SNOW,BL_VL_SNOW,&
                         HLV,HLH,HKIP,VLV,VLH) ! MYWN

      IF (LASTDEF.OR.NATLASTDEF) THEN
        MY1V=MY1VN
        MY1H=MY1HN
        SNOW_ACC_MY2(1)=SNOW_ACC_MY2N(1)
        SNOW_ACC_MY2(2)=SNOW_ACC_MY2N(2)
        MY4V=MY4VN
        MY4H=MY4HN
        MYW=MYWN         ! Determined in routine SNOW_EXPO_LEE
        SNOW_CA=SNOW_CAN ! Determined in routine SNOW_EXPO_LEE

        BIGSPAN_CHOICE1=1
        BIGSPAN_CHOICE2=1
        VX_importNumber=0

      ENDIF

      L_REA(21)=QSNEN

!     Wind

      L_REA(1)=QVIND
      L_REA(20)=QVINDN
      L_REA(2)=FSCT
      L_REA(43)=FSCTN

      L_LIST16(4)=TEXTX(555)  ! Both
      L_LIST16(5)=TEXTX(530)  ! Left
      L_LIST16(6)=TEXTX(531)  ! Right
      L_LIST16(7)=TEXTX(218)  ! None
      CALL SET_NO_CHR(L_INT(2),'BVHI',VINDFAC)

!     Snow

      L_REA(3)=QSNE

      L_REA(4)=SNOW_ACC_ALFAS(1)
      L_REA(5)=MY1V
      L_REA(6)=SNOW_ACC_MY2(1)
      L_REA(7)=MY1VN
      L_REA(8)=SNOW_ACC_MY2N(1)

      L_REA(9)=SNOW_ACC_ALFAS(2)
      L_REA(10)=MY1H
      L_REA(11)=SNOW_ACC_MY2(2)
      L_REA(12)=MY1HN
      L_REA(13)=SNOW_ACC_MY2N(2)

      L_REA(18)=MYW

      L_REA(22)=MY4V
      L_REA(23)=MY4VN
      L_REA(27)=MY4H
      L_REA(28)=MY4HN

      TYPE_LIST(3)=TEXTX(1288)  ! Normal
      TYPE_LIST(4)=TEXTX(1289)  ! Adjoining roof
      TYPE_LIST(5)=TEXTX(2072)  ! Taller construction work
      TYPE_LIST(6)=TEXTX(2073)  ! Parapet
      TYPE_LIST(7)=TYPE_LIST(3)
      TYPE_LIST(8)=TYPE_LIST(6)

      ! ======================================================================
      ! Big span effect
        BigSpanFacilCheckOutcome = BigSpanFacilCheck(1,9999,HALTYP)

        IF(BigSpanFacilCheckOutcome) THEN ! 3rd argument ignored here

          ! load all required data from existing wind bracing
          CALL LOAD_VX(VX_CODE,VX_ALLDATA,VX_FEJL)

          frictionLength_int=FindFrictionLength()
          WRITE(frictionLength_char, '(I6)') frictionLength_int

          VX_SUM=0
          DO I=1,9
            VX_SUM = VX_SUM+VX_CODE(I,1)
          END DO

          ! IF(VX_SUM>0) THEN
          IF(BIGSPAN_CHOICE1<0.OR.BIGSPAN_CHOICE1>10) THEN
            BIGSPAN_CHOICE1=1
            ! BIGSPAN_CHOICE2=1
          ENDIF
          L_INT(35)=BIGSPAN_CHOICE1
          L_INT(36)=BIGSPAN_CHOICE2

          L_LIST25(1)=TEXTX(3181)  ! None
          L_LIST25(2)=TEXTX(3182)  ! Both
          L_LIST25(3)=TEXTX(3183)  ! Roof
          L_LIST25(4)=TEXTX(3184)  ! Legs

          L_LIST16(25)=TEXTX(3185)  ! Custom

          VX_SUM=0
          J=1
          DO I = 1, 9
            IF (VX_CODE(i,1) /= 0) THEN
              VX_CODE(i,2)=J
              J=J+1
              VX_SUM=VX_SUM+1
              WRITE(VX_LIST, '(A,I1)') 'Import VX. ', I
              L_LIST16(25+VX_SUM)=VX_LIST
              IF(I==VX_importNumber) L_INT(36)=J
            END IF
          END DO

          IF(VX_importNumber/=0.AND.INT(VX_ALLDATA(6,VX_importNumber))/=0) THEN
              VX_DATA(1)=VX_ALLDATA(1,VX_importNumber)
              VX_DATA(2)=VX_ALLDATA(2,VX_importNumber)
              VX_DATA(3)=VX_ALLDATA(3,VX_importNumber)
              VX_DATA(4)=VX_ALLDATA(4,VX_importNumber)
              VX_DATA(5)=VX_ALLDATA(5,VX_importNumber)
              VX_DATA(6)=VX_ALLDATA(6,VX_importNumber)
              VX_DATA(7)=VX_ALLDATA(7,VX_importNumber)
              VX_DATA(8)=VX_ALLDATA(8,VX_importNumber)
              VX_DATA(9)=VX_ALLDATA(9,VX_importNumber)
              VX_DATA(10)=VX_ALLDATA(10,VX_importNumber)
              VX_DATA(11)=VX_ALLDATA(11,VX_importNumber)
          ENDIF

          CALL CB_IMPORTVX_WREP

        ENDIF

      ! ======================================================================

      L_INT(4)=SNOW_CONDITION(1)
      L_INT(9)=SNOW_CONDITION(2)
      L_RB(4)=L_INT(4)-1
      L_RB(9)=L_INT(9)-1

      L_INT(10)=SNOW_CONDITION(3)
      L_INT(11)=SNOW_ACC_BS(1)
      L_INT(12)=SNOW_ACC_B1(1)
!      L_INT(13)=SNOW_ACC_H1(1)
      L_INT(14)=SNOW_ACC_H(1)
      L_INT(15)=SNOW_ACC_LS(1)
      L_INT(21)=SNOW_ACC_LSN(1)

      L_INT(16)=SNOW_ACC_BS(2)
      L_INT(17)=SNOW_ACC_B1(2)
!      L_INT(18)=SNOW_ACC_H1(2)
      L_INT(19)=SNOW_ACC_H(2)
      L_INT(20)=SNOW_ACC_LS(2)
      L_INT(26)=SNOW_ACC_LSN(2)

      L_INT(31)=SNOW_ACC_H(3)
      L_INT(32)=SNOW_ACC_LS(3)
      L_INT(33)=SNOW_ACC_LSN(3)

      L_REA(31)=SNOW_ACC_MYS(1)
      L_REA(32)=SNOW_ACC_MYSN(1)
      L_REA(33)=SNOW_ACC_MYW(1)
      L_REA(34)=SNOW_ACC_MYWN(1)

      L_REA(36)=SNOW_ACC_MYS(2)
      L_REA(37)=SNOW_ACC_MYSN(2)
      L_REA(38)=SNOW_ACC_MYW(2)
      L_REA(39)=SNOW_ACC_MYWN(2)

      L_REA(30)=SNOW_ACC_MYW(3)
      L_REA(40)=SNOW_ACC_MYWN(3)

!     Other load types

      IF (LASTDEF) THEN
        ! CALL USERLOADDEF
        L_REA(44:49)=0.                     ! Roof loads
        L_RB(1:3)=0                         ! Grain, crane, conc. loads
      ELSE
        I=43
        DO ILOC=1,2
          DO IPOPV=1,3
            I=I+1
            L_REA(I)=POPV_ROOF(IPOPV,ILOC)  ! L_REA(44:49)  Roof loads
          ENDDO
        ENDDO
        CALL SET_CHR_RB(LOSVAR,'J',L_RB(1))
        L_RB(2)=TKLAST
        CALL SET_CHR_RB(KLAST,'J',L_RB(3))
      ENDIF

      L_REA(29)=FI_STAMOD_TEMP              ! FID STAMOD temperature

      LASTDEF=.FALSE.
      NATLASTDEF=.FALSE.

  100 L_BUT_NO=0
      W_REPEAT=.FALSE.
      CALL SNOW_EXPO_LEE(NORM,INAX,HALTYP,TERKLA,TOPOGRAFI,BORI,L_INT(4),L_INT(9),&
                         LL,BL_SNOW,BL_VL_SNOW,HLV,HLH,HKIP,VLV,VLH)
      L_REA(19)=MYWN
      L_REA(35)=SNOW_CE

      L_REA(41)=SNOW_CA
      L_REA(42)=SNOW_CAN

      CALL W_WINDOW_OPEN(0,1191)            ! Frames

      CALL W_BOX_OPEN(1,1,'raised')

      IF (FSHAL/='J'.AND.(L_INT(4)==3.OR.L_INT(9)==3))&
      CALL W_BOX_OPEN(2,1,'no_border')      ! BMP

      CALL W_NL(1)
      CALL W_HEADING_TEXT(L_HEAD_TEXT,0,0)  ! Frame n
      CALL W_SPACE(3)
      IF (FID) THEN
        CALL W_HEADING_RED(2599,0,2)        ! (Fire:) STAMOD model
      ELSE
        CALL W_HEADING(1229,0,2)            ! STAMOD model
      ENDIF

      CALL W_HEADING(1949,0,1)              ! Roof loads

      NBR=2
      IF (HALTYP=='S'.OR.HALTYP=='A') THEN
        CALL W_BOX_OPEN(2,1,'raised')
        NBR=3
      ENDIF
      CALL W_BOX_OPEN(4,NBR,'no_border')
      IF (NBR==3) THEN
        CALL W_BC(1)
        CALL W_HEADING(530,0,-3)            ! Left
      ENDIF
      CALL W_BC(1)
      DO TXINO=2650,2652
        CALL W_WRTTX(TXINO,0,-1)            ! Perm./Optional/Variable
      ENDDO
      CALL W_WRTTX(2653,0,0)                ! Distributed roof load
      CALL W_SPACE(3)
      CALL W_BC(1)
      CALL W_INPREA(0,0,L_REA(44),0.D0,0.D0,0,W,' ',0,1,0)
      CALL W_INPREA(0,0,L_REA(45),0.D0,0.D0,0,W,' ',-2,1,0)
      CALL W_INPREA(0,0,L_REA(46),0.D0,0.D0,0,W,'kN/m2',0,1,0)
      CALL W_BC(1)
      IF (HALTYP=='S'.OR.HALTYP=='A') THEN
        CALL W_BOX_CLOSE
        CALL W_BOX_OPEN(3,NBR,'no_border')
        CALL W_HEADING(531,0,-3)            ! Right
        DO TXINO=2650,2652
          CALL W_WRTTX(TXINO,0,-1)          ! Perm./Optional/Variable
        ENDDO
        CALL W_INPREA(0,0,L_REA(47),0.D0,0.D0,0,W,' ',0,1,0)
        CALL W_INPREA(0,0,L_REA(48),0.D0,0.D0,0,W,' ',-2,1,0)
        CALL W_INPREA(0,0,L_REA(49),0.D0,0.D0,0,W,'kN/m2',0,1,0)
        CALL W_BC(1)
        CALL W_BOX_CLOSE
      ENDIF

      NBR=5
      IF (NORM=='EC'.AND.(INAX==1.OR.INAX==8)) NBR=NBR+1
      IF (NORM=='EC'.AND.(INAX==4.OR.INAX==6)) NBR=NBR+1
      IF (FSHAL=='J'.AND.SNOWLEE_LOC>0) NBR=NBR+1
      ! ======================================================================
      IF(BigSpanFacilCheckOutcome) THEN
        CALL W_BOX_OPEN(2,1,'raised')
      ELSE
        CALL W_BOX_OPEN(1,1,'raised')
      ENDIF
      ! ======================================================================

      CALL W_BOX_OPEN(3,NBR,'no_border')

      CALL W_HEADING(1273,0,-3)                                 ! Wind

      CALL W_WRTTX(1279,0,0)                                    ! Basic wind load
      IF (VB0_TXT/=' ') THEN
        CALL W_SPACE(3)
        CALL W_WRTTXT(VB0_TXT,0,0)
        CALL W_SPACE(3)
      ENDIF
      CALL W_BC(1)
      CALL W_RO_REA(0,L_REA(20),0,W,' ',0)                      ! QVINDN
      CALL W_BC(1)
      CALL W_INPREA(0,0,L_REA(1),0.D0,0.D0,0,W,'kN/m2',1,1,0)   ! QVIND
      CALL W_BC(1)

      IF (FSHAL=='J') THEN
        CALL W_RO_REA(1285,L_REA(43),0,W,' ',-1)                ! FSCTN
        CALL W_BC(1)
        CALL W_INPREA(0,0,L_REA(2),0.D0,0.D0,0,W,' ',1,1,0)     ! FSCT
        CALL W_BC(1)
      ELSE
        CALL W_WRTTX(1278,0,-2)                                 ! Wind on facades
        CALL W_INPLS(0,0,L_LIST16(4),4,L_INT(2),0,0,1,0)        ! VINDFAC
        CALL W_BC(1)
      ENDIF

      CALL W_HEADING(1274,0,-3)                                 ! Snow
      CALL W_WRTTX(1286,0,-1)                                   ! Basic snow load
      CALL W_RO_REA(0,L_REA(21),0,W,' ',0)                      ! QSNEN
      CALL W_BC(1)
      CALL W_INPREA_CB(0,0,L_REA(3),0.D0,0.D0,0,W,'kN/m2',1,1,0,CB_QSNE_DEP_DEF) ! QSNE
      CALL W_BC(1)

      IF (NORM=='EC'.AND.(INAX==1.OR.INAX==8))&
      CALL W_RO_REA(1977,L_REA(35),0,W,' ',-3)                  ! SNOW_CE

      IF (NORM=='EC'.AND.(INAX==4.OR.INAX==6)) THEN
        CALL W_WRTTX(2083,0,1)                                  ! Area factor CA
        CALL W_STATUS_TEXT(OENORM_A1_TXT,0,0)
        CALL W_RO_REA(0,L_REA(42),0,W,' ',-2)
        CALL W_INPREA(0,0,L_REA(41),0.D0,0.D0,0,W,' ',1,1,0)
        CALL W_BC(1)
      ENDIF

      ! ======================================================================
      CALL W_BOX_CLOSE

      IF(BigSpanFacilCheckOutcome) THEN ! 3rd argument ignored here

        !----
        CALL W_BOX_OPEN(2,4,'no_border') ! 'shaded'
        CALL W_HEADING(3180,0,0)  ! Wind bracing effect
        CALL W_BC(2)
        CALL W_BOX_EMPTY_CELLS(2)

        CALL W_WRTTX(3186,0,0)    ! location
        CALL W_SPACE(10)
        CALL W_BC(1)
        CALL W_INPLS_CB(0,0,L_LIST25(1),4,L_INT(35),0,2,1,0,CB_BIG_SPAN_WREP)
        CALL W_BC(1)

        IF(L_INT(35)==1) THEN
          BIGSPAN_CHOICE2=1
          CALL W_BC(2)

        ELSE
            ! if N different than 36 it must be also corrected in W_INP_VX
            ! friction length is here for presentation of suggested value regardless of user choices
          CALL W_INP_VX(36,W,VX_SUM,L_INT(36),frictionLength_char)

            ! L_INT(36)=BIGSPAN_CHOICE2

        ENDIF
        
        CALL W_BOX_CLOSE
      ENDIF

      ! ======================================================================

      IF (FSHAL=='J') THEN
        IF (SNOWLEE_LOC>0) THEN
          CALL W_RO_REA(1295,L_REA(19),0,W,' ',-2)
          CALL W_INPREA(0,0,L_REA(18),0.D0,0.D0,0,W,' ',1,1,0)  ! MYW
          CALL W_BC(1)
        ENDIF

        IF (.NOT.(NORM=='EC'.AND.INAX==11))&
        CALL W_BOX_OPEN(1,1,'raised')

      ELSE

        IF (L_INT(4)==3.OR.L_INT(9)==3) THEN
          CALL W_BOX_CLOSE ! BMP
          CALL W_TAB(1)
          CALL W_BOX_OPEN(1,2,'no_border')
          CALL W_BITMAP(BMP_NAME)
          CALL W_BOX_CLOSE
          CALL W_CENTRE
          CALL W_WRTTX(2081,0,0)  ! Principle sketch
          CALL W_BOX_CLOSE
          CALL W_BOX_CLOSE !BMP
        ENDIF

        IF (NORM=='EC'.AND.INAX==11) THEN
          CALL W_BOX_OPEN(2,1,'raised')
        ELSE
          CALL W_BOX_OPEN(3,1,'raised')
        ENDIF
        CALL W_HEADING(557,0,1)                                 ! Left side

        CALL W_INP_SNOW_SIDE(4,W,SNOW_COND3_2(1),SNOWLEE_LOC,&
                             CB_SNEV_DEF_WREP,CB_SNEV_LS_DEF,CB_SNEV_DEF)
        CALL W_BOX_CLOSE

        CALL W_HEADING(558,0,1)                                 ! Right side
        CALL W_INP_SNOW_SIDE(9,W,SNOW_COND3_2(2),SNOWLEE_LOC,&
                             CB_SNEH_DEF_WREP,CB_SNEH_LS_DEF,CB_SNEH_DEF)
        CALL W_BOX_CLOSE
      ENDIF

      IF (.NOT.(NORM=='EC'.AND.INAX==11)) THEN
        CALL W_HEADING(1747,0,1)                                ! Gable

        CALL W_WRTTX(1287,0,0)                                  ! Snow case
        CALL W_SPACE(3)
        CALL W_INPLS_CB(0,0,TYPE_LIST(7),2,L_INT(10),0,2,1,0,CB_SNEG_DEF_WREP)
        IF (L_INT(10)==2) THEN
          CALL W_BOX_OPEN(3,3,'no_border')
          CALL W_WRTTX(2074,0,-1)                               ! Height of obstruction
          CALL W_WRTTXT(' h',0,-1)
          CALL W_INPINT_CB(0,0,L_INT(31),0,0,0,W,' ',0,1,0,CB_SNEG_DEF)
          CALL W_BC(1)

          CALL W_WRTTX(2075,0,-1)                               ! Horizontal extension
          CALL W_WRTTXT(' ls =',0,0)
          CALL W_RO_INT(0,L_INT(33),0,W,' ',0)                  ! LSN
          CALL W_BC(1)
          CALL W_INPINT(0,0,L_INT(32),0,0,0,W,' ',0,1,0)        ! LS
          CALL W_BC(1)

          CALL W_RO_REA(1295,L_REA(40),0,W,' ',-2)              ! Shape factor MYW
          CALL W_INPREA(0,0,L_REA(30),0.D0,0.D0,0,W,' ',0,1,0)  ! MYW
          CALL W_BC(1)
        ENDIF
        CALL W_BOX_CLOSE
      ENDIF

!     FID, Steel temperature

      IF (FID) THEN
        CALL W_HEADING_RED(2602,0,1)                            ! Fire design
        CALL W_INPREA(2663,0,L_REA(29),0.D0,0.D0,1,W,'C',2,1,0) ! FI_STAMOD_TEMP
      ENDIF

!     Other types of load, check marks

!      CALL W_NL(1)
      CALL W_BOX_OPEN(5,1,'no_border')
      CALL W_HEADING(1275,0,1)                                  ! Grain load
      CALL W_RADIO_BUTTON(1,1275,L_RB(1),2)                     ! Grain load
      CALL W_BOX_CLOSE

      CALL W_BOX_SPACE(5)

      CALL W_HEADING(1276,0,1)                                  ! Crane(s)
      CALL W_RADIO_BUTTON(1,1276,L_RB(2),1)                     ! Crane(s)
      CALL W_BOX_CLOSE

      CALL W_BOX_SPACE(5)

      CALL W_HEADING(1299,0,1)                                  ! User-defined loads
      CALL W_RADIO_BUTTON(1,1277,L_RB(3),1)                     ! Concentrated loads
      CALL W_BOX_CLOSE

      CALL W_BOX_CLOSE

      CALL W_CENTRE
      CALL W_BUTTON(-1,1296,0,CB_BUT1,.FALSE.)   ! Reset all
      CALL W_BUTTON(-1,1297,0,CB_BUT2,.FALSE.)   ! Reset nature loads
      CALL W_BUTTON(-1,1322,0,CB_BUT3,.FALSE.)   ! OK, &Plot

      IF(InstallationNumber==24) THEN
        CALL W_BUTTON(-1,3201,2,CB_BUT5,.FALSE.)   ! OK>...>code check
      ELSE
        CALL W_NL(2)
      ENDIF
      
      CALL W_OK_CANCEL_BUTTON(368,CB_BUT4)
      
      CALL W_WINDOW_END

      IF (L_BUT_NO==5) THEN
      Accelerate1=1
      L_BUT_NO=4
      ENDIF

      IF (W_REPEAT) GOTO 100

      IF (L_BUT_NO==0) THEN       ! Cancel
        L_INT(36)=BIGSPAN_CHOICE2
        FEJL=.TRUE.
      	RETURN
      ELSEIF (L_BUT_NO==1) THEN   ! Reset all
        LASTDEF=.TRUE.
        GOTO 5
      ELSEIF (L_BUT_NO==2) THEN   ! Reset nature loads
        NATLASTDEF=.TRUE.
        GOTO 5
      ENDIF
      

!     Data ok

      LASTDEF=.FALSE.
      NATLASTDEF=.FALSE.

!     Roof loads

      I=43
      DO ILOC=1,2
        DO IPOPV=1,3
          I=I+1
          POPV_ROOF(IPOPV,ILOC)=L_REA(I) ! L_REA(43:49)
        ENDDO
      ENDDO

!     Wind

      QVIND=L_REA(1)
      QVINDN=L_REA(20)
      FSCT=L_REA(2)

!     Big span
      BIGSPAN_CHOICE1=L_INT(35)
      BIGSPAN_CHOICE2=L_INT(36)
      IF(BIGSPAN_CHOICE2>1) THEN
        VX_importNumber=VX_DATA(6)
      ELSE
        VX_importNumber=0
      ENDIF

      ! ======================================================================
      IF (BigSpanFacilCheckOutcome) THEN

        BS_FORCEINPUT=0

        IF(VX_DATA(1)<1) THEN
          BS_FORCEINPUT(1) = 1          ! c/c
        ELSE
          BS_FORCEINPUT(1) = VX_DATA(1)   ! c/c
        ENDIF

        BS_FORCEINPUT(2) = VX_DATA(2)     ! q_p
        BS_FORCEINPUT(3) = VX_DATA(3)     ! c_p
        BS_FORCEINPUT(4) = VX_DATA(4)     ! c_fr
        BS_FORCEINPUT(5) = VX_DATA(5)     ! fr_length
        BS_FORCEINPUT(6) = VX_DATA(6)     ! slot number for wind bracing
        BS_FORCEINPUT(7) = VX_DATA(7)     ! Fixation load - number of frames
        BS_FORCEINPUT(8) = VX_DATA(8)     ! Fixation load - cross sectional area of flange
        BS_FORCEINPUT(9) = VX_DATA(9)     ! Fixation load - yield strength
        BS_FORCEINPUT(10) = VX_DATA(10)   ! Skew load - number of frames
        BS_FORCEINPUT(11) = VX_DATA(11)   ! Skew load - load / horizontal projection

        ! remaining slots in BS_FORCEINPUT
        ! BS_FORCEINPUT(12) = VX_DATA(12)   ! PLACEHOLDER
        !   ...
        !   ...
        !   ...
        !   ...
        ! BS_FORCEINPUT(24) = 0             ! placeholder

      ENDIF
      ! ======================================================================

      CALL SET_CHR_NO(VINDFAC,'BVHI',L_INT(2))

!     Snow

      QSNE=L_REA(3)
      SNOW_CA=MAX(1.D0,L_REA(41))
      IF (NORM=='EC'.AND.(INAX==4.OR.INAX==6).AND.L_REA(41)<1.D0)&
      CALL W_INFORM(0,2711) ! CA = minimum value 1.00

      SNOW_ACC_ALFAS(1)=L_REA(4)
      MY1V=L_REA(5)
      SNOW_ACC_MY2(1)=L_REA(6)
      MY1VN=L_REA(7)
      SNOW_ACC_MY2N(1)=L_REA(8)

      SNOW_ACC_ALFAS(2)=L_REA(9)
      MY1H=L_REA(10)
      SNOW_ACC_MY2(2)=L_REA(11)
      MY1HN=L_REA(12)
      SNOW_ACC_MY2N(2)=L_REA(13)

      MYW=L_REA(18)

      MY4VN=L_REA(22)
      MY4HN=L_REA(27)

      SNOW_CONDITION(1)=L_INT(4)
      SNOW_CONDITION(2)=L_INT(9)

      SNOW_CONDITION(3)=L_INT(10)
      SNOW_ACC_BS(1)=L_INT(11)
      SNOW_ACC_B1(1)=L_INT(12)
!      SNOW_ACC_H1(1)=L_INT(13)
      SNOW_ACC_H(1)=L_INT(14)
      SNOW_ACC_LS(1)=L_INT(15)
      SNOW_ACC_LSN(1)=L_INT(21)

      SNOW_ACC_BS(2)=L_INT(16)
      SNOW_ACC_B1(2)=L_INT(17)
!      SNOW_ACC_H1(2)=L_INT(18)
      SNOW_ACC_H(2)=L_INT(19)
      SNOW_ACC_LS(2)=L_INT(20)
      SNOW_ACC_LSN(2)=L_INT(26)

      SNOW_ACC_H(3)=L_INT(31)
      SNOW_ACC_LS(3)=L_INT(32)
      SNOW_ACC_LSN(3)=L_INT(33)

      SNOW_ACC_MYS(1)=L_REA(31)
      SNOW_ACC_MYSN(1)=L_REA(32)
      SNOW_ACC_MYW(1)=L_REA(33)
      SNOW_ACC_MYWN(1)=L_REA(34)

      SNOW_ACC_MYS(2)=L_REA(36)
      SNOW_ACC_MYSN(2)=L_REA(37)
      SNOW_ACC_MYW(2)=L_REA(38)
      SNOW_ACC_MYWN(2)=L_REA(39)

      SNOW_ACC_MYW(3)=L_REA(30)
      SNOW_ACC_MYWN(3)=L_REA(40)

      FI_STAMOD_TEMP=L_REA(29)               ! FID critical steel temperature
      FI_STAMOD_TEMP=MAX(20.,FI_STAMOD_TEMP) ! Not lower than normal temp.

!     Other loadcases

      CALL SET_RB_CHR(L_RB(1),'JN',LOSVAR)
      TKLAST=L_RB(2)
      CALL SET_RB_CHR(L_RB(3),'JN',KLAST)

!     Apply the code snow shape factors for multi-aisle building

      IF (FSHAL=='J') THEN
        IF (.NOT.FSTRUG.OR.(NORM=='EC'.AND.INAX==11)) THEN
          SNOW_CONDITION(1)=1
          SNOW_CONDITION(2)=1
        ELSE
          IF (FSNSSV>0.AND.FSHALTYP(1)/='P') THEN
            SNOW_CONDITION(1)=2
            SNOW_ACC_ALFAS(1)=FSVLH(1)
          ELSE
            SNOW_CONDITION(1)=1
          ENDIF
          IF (FSNSSH>0.AND.FSHALTYP(FSNSSV+1)/='P') THEN
            SNOW_CONDITION(2)=2
            SNOW_ACC_ALFAS(2)=FSVLV(FSNSSV+1)
          ELSE
            SNOW_CONDITION(2)=1
          ENDIF
        ENDIF
        CALL DEFSFORM
        MY1V=MY1VN
        SNOW_ACC_MY2(1)=SNOW_ACC_MY2N(1)
        MY1H=MY1HN
        SNOW_ACC_MY2(2)=SNOW_ACC_MY2N(2)
      ENDIF

      IF (L_BUT_NO==3) THEN   ! Plot
        CALL RAMPLOT(2)
        GOTO 100
      ENDIF

!     Data OK, --> grain, crane and concentrated loads

      IF (LOSVAR=='J') CALL W_INP_GRAIN_LOAD(W)
      IF (TKLAST==1) THEN
        CALL TKLIMITS(XMIN,XMAX,YMIN,YMAX) ! Coordinate limits
        CALL TKLASTINP(S,CAP_TXINO,UDFTYPB,XMIN,XMAX,YMIN,YMAX)
        IF (TKBLAST==0.AND.TKRLAST==0) TKLAST=0
      ENDIF
      IF (KLAST=='J') THEN
        CALL W_INP_CONC_LOAD(W)
        IF (NKLAST==0) KLAST='N'
      ENDIF

      END SUBROUTINE LOAD_SPEC

      SUBROUTINE W_INP_SNOW_SIDE(N,W,SNOW_COND3_2,SNOWLEE_LOC,CB_FUNC_WREP,CB_LS_FUNC,CB_FUNC)

      IMPLICIT NONE

      INTEGER   N,W,SNOWLEE_LOC
      INTEGER   NBR,TXINO
      LOGICAL   SNOW_COND3_2

      INTEGER,EXTERNAL:: CB_FUNC_WREP,CB_LS_FUNC,CB_FUNC

      INCLUDE 'GENERL.INS'
      INCLUDE 'LOCAL.INS'

      IF (.NOT.(NORM=='EC'.AND.INAX==11)) THEN
        CALL W_WRTTX(1287,0,0)  ! Snow case
        CALL W_SPACE(3)
        CALL W_INPLS_CB(0,0,TYPE_LIST(3),4,L_INT(N),0,2,1,0,CB_FUNC_WREP)
      ENDIF

      IF (L_INT(N)==3.AND.SNOW_COND3_2)&
      CALL W_ASSIST(2084,0,0,2) ! Trough roof, one-sided snow acc.

      NBR=0
      IF (L_INT(N)==3) THEN
        IF (.NOT.SNOW_COND3_2) THEN
          IF (.NOT.(NORM=='EC'.AND.INAX==1)) NBR=NBR+1
        ENDIF
        NBR=NBR+1
      ENDIF
      IF (L_INT(N)==2.OR.L_INT(N)==3) NBR=NBR+1
      IF (L_INT(N)==3.OR.L_INT(N)==4) NBR=NBR+1
      IF ((L_INT(N)==3.AND..NOT.SNOW_COND3_2).OR.L_INT(N)==4) NBR=NBR+1
      IF ((HALTYP=='S'.OR.HALTYP=='A').OR.&
          (HALTYP=='P'.AND.N==4.AND..NOT.(L_INT(N)==2.AND.L_INT(9)==2))) NBR=NBR+1

      IF (L_INT(N)==1) THEN
        IF (HALTYP=='S'.OR.HALTYP=='A') THEN
          IF (L_INT(4)==1.AND.L_INT(9)==1) THEN
            IF (NORM=='EC'.AND.(INAX==3.OR.INAX==6.OR.INAX==11).AND.FSHAL=='N') THEN
              NBR=NBR+1
            ELSEIF (N==5*SNOWLEE_LOC-1) THEN
              NBR=NBR+1
            ENDIF
          ENDIF
        ENDIF
      ELSEIF (L_INT(N)==2) THEN
        NBR=NBR+1
      ELSEIF (L_INT(N)==3) THEN
        NBR=NBR+2
      ELSEIF (L_INT(N)==4) THEN
        NBR=NBR+1
      ENDIF

      CALL W_BOX_OPEN(3,NBR,'no_border')

      IF (L_INT(N)==3) THEN
        IF (.NOT.SNOW_COND3_2) THEN
          IF (.NOT.(NORM=='EC'.AND.INAX==1)) THEN
            CALL W_WRTTX(2076,0,-1)                                  ! Width of taller constr. work
            CALL W_WRTTXT('b1',0,-1)
            CALL W_INPINT_CB(0,0,L_INT(N+8),0,0,0,W,' ',0,1,0,CB_LS_FUNC)
           CALL W_BC(1)
          ENDIF
        ENDIF
        CALL W_WRTTX(2077,0,-1)                                      ! Width of adjoining roof surface
        CALL W_WRTTXT(' bs',0,-1)
        CALL W_INPINT_CB(0,0,L_INT(N+7),0,0,0,W,' ',0,1,0,CB_LS_FUNC)
        CALL W_BC(1)
      ENDIF
      IF (L_INT(N)==2.OR.L_INT(N)==3) THEN
        CALL W_WRTTX(1292,0,-1)                                      ! Adjoining roof inclination
        CALL W_WRTTXT(' as',0,-1)
        CALL W_INPREA_CB(0,0,L_REA(N),0.D0,0.D0,0,W,'°',0,L_RB(N),0,CB_LS_FUNC)
        CALL W_BC(1)
      ENDIF
!      IF (L_INT(N)==3.AND.NORM=='EC'.AND.INAX==1)&
!      CALL W_INPINT_CB(2078,0,L_INT(N+9),0,0,1,W,' ',1,1,0,CB_LS_FUNC) ! H1
      IF (L_INT(N)==3.OR.L_INT(N)==4) THEN
        CALL W_WRTTX(2074,0,-1)                                      ! Height of obstruction
        CALL W_WRTTXT(' h',0,-1)
        CALL W_INPINT_CB(0,0,L_INT(N+10),0,0,0,W,' ',0,1,0,CB_LS_FUNC)
        CALL W_BC(1)
      ENDIF
      IF ((L_INT(N)==3.AND..NOT.SNOW_COND3_2).OR.L_INT(N)==4) THEN
        CALL W_WRTTX(2075,0,-1)                                      ! Horizontal extension
        CALL W_WRTTXT(' ls =',0,0)
        CALL W_RO_INT(0,L_INT(N+17),0,W,' ',0)                       ! LSN
        CALL W_BC(1)
        CALL W_INPINT_CB(0,0,L_INT(N+11),0,0,0,W,' ',0,1,0,CB_FUNC)  ! LS
        CALL W_BC(1)
      ENDIF

      IF ((HALTYP=='S'.OR.HALTYP=='A').OR.&
          (HALTYP=='P'.AND.N==4.AND..NOT.(L_INT(N)==2.AND.L_INT(9)==2))) THEN
        IF (NORM=='RU') THEN
          TXINO=1761                                                 ! Shape factor MY
        ELSE
          IF (HALTYP=='P') THEN
            TXINO=1293                                               ! Shape factor MY1
          ELSE
            IF (NORM=='EC'.AND.(INAX==6.OR.INAX==11)) THEN
              TXINO=1293                                             ! Shape factor MY1
            ELSE
              TXINO=1294                                             ! Shape factor MY2
            ENDIF
          ENDIF
        ENDIF
        CALL W_RO_REA(TXINO,L_REA(N+3),0,W,' ',-2)
        CALL W_INPREA(0,0,L_REA(N+1),0.D0,0.D0,0,W,' ',0,1,0)        ! MY1
        CALL W_BC(1)
      ENDIF

      IF (L_INT(N)==1) THEN
        IF (HALTYP=='S'.OR.HALTYP=='A') THEN
          IF (L_INT(4)==1.AND.L_INT(9)==1) THEN
            IF (NORM=='EC'.AND.(INAX==3.OR.INAX==6.OR.INAX==11).AND.FSHAL=='N') THEN
              IF (INAX==6.OR.INAX==11) THEN
                TXINO=1294                                           ! Shape factor MY2
              ELSE
                TXINO=2557                                           ! Shape factor MY5
              ENDIF
              CALL W_RO_REA(TXINO,L_REA(N+19),0,W,' ',-2)            ! MY4
              CALL W_INPREA(0,0,L_REA(N+18),0.D0,0.D0,0,W,' ',0,1,0)
              CALL W_BC(1)
            ELSEIF (N==5*SNOWLEE_LOC-1) THEN ! DS-EN
              CALL W_RO_REA(1295,L_REA(19),0,W,' ',-2)               ! MYW
              CALL W_INPREA(0,0,L_REA(18),0.D0,0.D0,0,W,' ',0,1,0)
              CALL W_BC(1)
            ENDIF
          ENDIF
        ENDIF
      ELSEIF (L_INT(N)==2) THEN
        TXINO=2560 ! Shape factor MY3
        CALL W_RO_REA(TXINO,L_REA(N+4),0,W,' ',-2)
        CALL W_INPREA(0,0,L_REA(N+2),0.D0,0.D0,0,W,' ',0,1,0)        ! MY2
        CALL W_BC(1)
      ELSEIF (L_INT(N)==3) THEN
        IF (NORM=='EC'.AND.INAX==4) THEN ! DIN EN
          CALL W_ASSIST_COL
          CALL W_RO_REA(1295,L_REA(N+30),1,W,' ',1)                  ! Shape factor MYW
          CALL W_RO_REA(2079,L_REA(N+28),1,W,' ',1)                  ! Shape factor MYS
          CALL W_TXT_COL_RESET
          CALL W_BC(3)
          CALL W_RO_REA(2080,L_REA(N+4),0,W,' ',-2)                  ! MY2 = MYW + MYS
          CALL W_INPREA(0,0,L_REA(N+2),0.D0,0.D0,0,W,' ',0,1,0)      ! MY2
          CALL W_BC(1)
        ELSE
          CALL W_RO_REA(1295,L_REA(N+30),0,W,' ',-2)                 ! Shape fact MYW
          CALL W_INPREA(0,0,L_REA(N+29),0.D0,0.D0,0,W,' ',0,1,0)     ! MYW
          CALL W_BC(1)
          CALL W_RO_REA(2079,L_REA(N+28),0,W,' ',-2)                 ! Shape fact MYS
          CALL W_INPREA(0,0,L_REA(N+27),0.D0,0.D0,0,W,' ',0,1,0)     ! MYS
          CALL W_BC(1)
        ENDIF
      ELSEIF (L_INT(N)==4) THEN
        CALL W_RO_REA(1295,L_REA(N+30),0,W,' ',-2)                   ! Shape factor MYW
        CALL W_INPREA(0,0,L_REA(N+29),0.D0,0.D0,0,W,' ',0,1,0)       ! MYW
        CALL W_BC(1)
      ENDIF

      END SUBROUTINE W_INP_SNOW_SIDE


      INTEGER FUNCTION CB_SNEV_DEF_WREP()

      IMPLICIT NONE

      LOGICAL   SET_LS

      INCLUDE 'GENERL.INS'
      INCLUDE 'NATLAST.INS'
      INCLUDE 'LOCAL.INS'

      SET_LS=.TRUE.
      CALL SNE_DEF(NORM,INAX,BS_P5_10,HALTYP,VLV,SNOW_COND3_2(1),SNOW_ACC_B2(1),&
                   SNOW_ACC_B3(1),SNOW_ACC_H3(1),SNOW_ACC_ALFA3(1),&
                   MY1VN,SNOW_ACC_MY2N(1),MY4VN,SNOW_ACC_MYSN(1),SNOW_ACC_MYWN(1),SET_LS,4)

      W_REPEAT=.TRUE.
      CB_SNEV_DEF_WREP=0

      END FUNCTION CB_SNEV_DEF_WREP


      INTEGER FUNCTION CB_SNEH_DEF_WREP()

      IMPLICIT NONE

      LOGICAL   SET_LS

      INCLUDE 'GENERL.INS'
      INCLUDE 'NATLAST.INS'
      INCLUDE 'LOCAL.INS'

      SET_LS=.TRUE.
      CALL SNE_DEF(NORM,INAX,BS_P5_10,HALTYP,VLH,SNOW_COND3_2(2),SNOW_ACC_B2(2),&
                   SNOW_ACC_B3(2),SNOW_ACC_H3(2),SNOW_ACC_ALFA3(2),&
                   MY1HN,SNOW_ACC_MY2N(2),MY4HN,SNOW_ACC_MYSN(2),SNOW_ACC_MYWN(2),SET_LS,9)
      W_REPEAT=.TRUE.
      CB_SNEH_DEF_WREP=0

      END FUNCTION CB_SNEH_DEF_WREP

      INTEGER FUNCTION CB_SNEG_DEF_WREP()

      IMPLICIT NONE

      INTEGER,EXTERNAL:: CB_SNEG_DEF

      INCLUDE 'DUMVAR.INS'
      INCLUDE 'LOCAL.INS'

      DUM_INT=CB_SNEG_DEF()

      W_REPEAT=.TRUE.
      CB_SNEG_DEF_WREP=0

      END FUNCTION CB_SNEG_DEF_WREP


      INTEGER FUNCTION CB_SNEG_DEF()

      IMPLICIT NONE

      REAL      QSNEL,MYWL

      INCLUDE 'NATLAST.INS'
      INCLUDE 'LOCAL.INS'

      QSNEL=L_REA(3)
      CALL SNOW_PARAPET(QSNEL,SNOW_ACC_B2(3),SNOW_ACC_B3(3),SNOW_ACC_H3(3),&
                        L_INT(31),L_INT(33),MYWL)          ! Gable parapet
      L_REA(40)=MYWL

      L_INT(32)=L_INT(33)
      L_REA(30)=L_REA(40)

      CB_SNEG_DEF=1

      END FUNCTION CB_SNEG_DEF


      INTEGER FUNCTION CB_QSNE_DEP_DEF()

      IMPLICIT NONE

      INTEGER,EXTERNAL:: CB_SNEV_LS_DEF,CB_SNEH_LS_DEF,CB_SNEG_DEF

      INCLUDE 'DUMVAR.INS'
      INCLUDE 'LOCAL.INS'

      IF (L_INT(4)>=3) DUM_INT=CB_SNEV_LS_DEF()            ! Tall../parapet, Left
      IF (L_INT(9)>=3) DUM_INT=CB_SNEH_LS_DEF()            ! Tall../parapet, Right

      IF (L_INT(10)==2) DUM_INT=CB_SNEG_DEF()              ! Gable parapet

      CB_QSNE_DEP_DEF=1

      END FUNCTION CB_QSNE_DEP_DEF


      SUBROUTINE SNE_DEF(NORM,INAX,BS_P5_10,HALTYP,VL,SNOW_COND3_2,B2,B3,H3,ALFA3R,&
                         MY1N,MY2N,MY4N,MYSN,MYWN,SET_LS,N)

      IMPLICIT NONE

      INTEGER   INAX,B2,B3,H3,N
      REAL      VL,ALFA3R,MY1N,MY2N,MY4N,MYSN,MYWN
      REAL      V,QSNE,ALFAS,VL_HLP
      LOGICAL   BS_P5_10,SNOW_COND3_2,SET_LS
      CHARACTER NORM*2,HALTYP*1

      INCLUDE 'LOCAL.INS'

      IF (L_INT(N)==1) THEN                                ! Normal
        CALL SNOW_MY1_MY4(NORM,INAX,BS_P5_10,VL,MY1N,MY4N)
!        MY2N=MY1N
        L_REA(N+1)=MY1N
        L_REA(N+3)=MY1N
        L_REA(N+18)=MY4N
        L_REA(N+19)=MY4N
      ELSEIF (L_INT(N)==2) THEN                            ! Adjoining roof
        IF (L_REA(N)<-60.) THEN
          L_REA(N)=-60.
        ELSEIF (60.<L_REA(N)) THEN
          L_REA(N)=60.
        ENDIF
        V=MAX(.5*(VL+L_REA(N)),0.)
        CALL SNOW_MY1(NORM,BS_P5_10,V,MY1N)
        CALL SNOW_MY2(NORM,INAX,V,MY2N)
        L_REA(N+1)=MY1N
        L_REA(N+3)=MY1N
        L_REA(N+2)=MY2N
        L_REA(N+4)=MY2N
      ELSE
        QSNE=L_REA(3)
        IF (N==4.OR.(HALTYP=='S'.OR.HALTYP=='A')) THEN
!          CALL SNOW_MY1(NORM,BS_P5_10,VL,MY1N)
          VL_HLP=0.                                        ! Taller constr. or parapet, no MY1 reduction
          CALL SNOW_MY1(NORM,BS_P5_10,VL_HLP,MY1N)
          L_REA(N+1)=MY1N
          L_REA(N+3)=MY1N
        ENDIF
        IF (L_INT(N)==3) THEN                              ! Taller constr.
          ALFAS=L_REA(N)
          CALL SNOW_TALL_CONSTR(BS_P5_10,QSNE,VL,SNOW_COND3_2,L_INT(N+8),B2,B3,H3,ALFA3R,&
                                L_INT(N+7),ALFAS,L_INT(N+10),L_INT(N+17),MYSN,MYWN,MY2N)
          IF (SET_LS) L_INT(N+11)=L_INT(N+17)
          L_REA(N+2)=MY2N
          L_REA(N+4)=MY2N
          L_REA(N+27)=MYSN
          L_REA(N+28)=MYSN
          L_REA(N+29)=MYWN
          L_REA(N+30)=MYWN
        ELSEIF (L_INT(N)==4) THEN                          ! Parapet
          CALL SNOW_PARAPET(QSNE,B2,B3,H3,L_INT(N+10),L_INT(N+17),MYWN)
          IF (SET_LS) L_INT(N+11)=L_INT(N+17)
          L_REA(N+29)=MYWN
          L_REA(N+30)=MYWN
        ENDIF
      ENDIF
      L_RB(N)=L_INT(N)-1

      END SUBROUTINE SNE_DEF


      INTEGER FUNCTION CB_SNEV_DEF()

      IMPLICIT NONE

      LOGICAL   SET_LS

      INCLUDE 'GENERL.INS'
      INCLUDE 'NATLAST.INS'
      INCLUDE 'LOCAL.INS'

      SET_LS=.FALSE.
      CALL SNE_DEF(NORM,INAX,BS_P5_10,HALTYP,VLV,SNOW_COND3_2(1),SNOW_ACC_B2(1),&
                   SNOW_ACC_B3(1),SNOW_ACC_H3(1),SNOW_ACC_ALFA3(1),&
                   MY1VN,SNOW_ACC_MY2N(1),MY4VN,SNOW_ACC_MYSN(1),SNOW_ACC_MYWN(1),SET_LS,4)
      CB_SNEV_DEF=1

      END FUNCTION CB_SNEV_DEF


      INTEGER FUNCTION CB_SNEV_LS_DEF()

      IMPLICIT NONE

      LOGICAL   SET_LS

      INCLUDE 'GENERL.INS'
      INCLUDE 'NATLAST.INS'
      INCLUDE 'LOCAL.INS'

      SET_LS=.TRUE.
      CALL SNE_DEF(NORM,INAX,BS_P5_10,HALTYP,VLV,SNOW_COND3_2(1),SNOW_ACC_B2(1),&
                   SNOW_ACC_B3(1),SNOW_ACC_H3(1),SNOW_ACC_ALFA3(1),&
                   MY1VN,SNOW_ACC_MY2N(1),MY4VN,SNOW_ACC_MYSN(1),SNOW_ACC_MYWN(1),SET_LS,4)
      CB_SNEV_LS_DEF=1

      END FUNCTION CB_SNEV_LS_DEF


      INTEGER FUNCTION CB_SNEH_DEF()

      IMPLICIT NONE

      LOGICAL   SET_LS

      INCLUDE 'GENERL.INS'
      INCLUDE 'NATLAST.INS'
      INCLUDE 'LOCAL.INS'

      SET_LS=.FALSE.
      CALL SNE_DEF(NORM,INAX,BS_P5_10,HALTYP,VLH,SNOW_COND3_2(2),SNOW_ACC_B2(2),&
                   SNOW_ACC_B3(2),SNOW_ACC_H3(2),SNOW_ACC_ALFA3(2),&
                   MY1HN,SNOW_ACC_MY2N(2),MY4HN,SNOW_ACC_MYSN(2),SNOW_ACC_MYWN(2),SET_LS,9)
      CB_SNEH_DEF=1

      END FUNCTION CB_SNEH_DEF


      INTEGER FUNCTION CB_SNEH_LS_DEF()

      IMPLICIT NONE

      LOGICAL   SET_LS

      INCLUDE 'GENERL.INS'
      INCLUDE 'NATLAST.INS'
      INCLUDE 'LOCAL.INS'

      SET_LS=.TRUE.
      CALL SNE_DEF(NORM,INAX,BS_P5_10,HALTYP,VLH,SNOW_COND3_2(2),SNOW_ACC_B2(2),&
                   SNOW_ACC_B3(2),SNOW_ACC_H3(2),SNOW_ACC_ALFA3(2),&
                   MY1HN,SNOW_ACC_MY2N(2),MY4HN,SNOW_ACC_MYSN(2),SNOW_ACC_MYWN(2),SET_LS,9)
      CB_SNEH_LS_DEF=1

      END FUNCTION CB_SNEH_LS_DEF


      SUBROUTINE DEFSFORM

      IMPLICIT NONE

      REAL      V

      INCLUDE 'GENERL.INS'
      INCLUDE 'NATLAST.INS'

      IF (SNOW_CONDITION(1)==1) THEN
        CALL SNOW_MY1_MY4(NORM,INAX,BS_P5_10,VLV,MY1VN,MY4VN)
        SNOW_ACC_MY2N(1)=MY1VN
      ELSEIF (SNOW_CONDITION(1)==2) THEN
        V=.5*(VLV+SNOW_ACC_ALFAS(1))
        CALL SNOW_MY1_MY4(NORM,INAX,BS_P5_10,V,MY1VN,MY4VN)
        CALL SNOW_MY2(NORM,INAX,V,SNOW_ACC_MY2N(1))
      ELSE
!        CALL SNOW_MY1(NORM,BS_P5_10,VLV,MY1VN)
        V=0.
        CALL SNOW_MY1(NORM,BS_P5_10,V,MY1VN)
        IF (SNOW_CONDITION(1)==3) THEN
          CALL SNOW_TALL_CONSTR(BS_P5_10,QSNE,VLV,SNOW_COND3_2(1),SNOW_ACC_B1(1),SNOW_ACC_B2(1),&
                    SNOW_ACC_B3(1),SNOW_ACC_H3(1),SNOW_ACC_ALFA3(1),&
                    SNOW_ACC_BS(1),SNOW_ACC_ALFAS(1),SNOW_ACC_H(1),SNOW_ACC_LSN(1),&
                    SNOW_ACC_MYSN(1),SNOW_ACC_MYWN(1),SNOW_ACC_MY2N(1))
        ELSE
          CALL SNOW_PARAPET(QSNE,SNOW_ACC_B2(1),SNOW_ACC_B3(1),SNOW_ACC_H3(1),&
                            SNOW_ACC_H(1),SNOW_ACC_LSN(1),SNOW_ACC_MYWN(1))
        ENDIF
      ENDIF

      IF (SNOW_CONDITION(2)==1) THEN
        CALL SNOW_MY1_MY4(NORM,INAX,BS_P5_10,VLH,MY1HN,MY4HN)
        SNOW_ACC_MY2N(2)=MY1HN
      ELSEIF (SNOW_CONDITION(2)==2) THEN
        V=.5*(VLH+SNOW_ACC_ALFAS(2))
        CALL SNOW_MY1_MY4(NORM,INAX,BS_P5_10,V,MY1HN,MY4HN)
        CALL SNOW_MY2(NORM,INAX,V,SNOW_ACC_MY2N(2))
      ELSE
        IF (HALTYP=='S'.OR.HALTYP=='A') THEN
!          CALL SNOW_MY1(NORM,BS_P5_10,VLH,MY1HN)
          V=0.
          CALL SNOW_MY1(NORM,BS_P5_10,V,MY1HN)
        ENDIF
        IF (SNOW_CONDITION(2)==3) THEN
          CALL SNOW_TALL_CONSTR(BS_P5_10,QSNE,VLH,SNOW_COND3_2(2),SNOW_ACC_B1(2),SNOW_ACC_B2(2),&
                    SNOW_ACC_B3(2),SNOW_ACC_H3(2),SNOW_ACC_ALFA3(2),&
                    SNOW_ACC_BS(2),SNOW_ACC_ALFAS(2),SNOW_ACC_H(2),SNOW_ACC_LSN(2),&
                    SNOW_ACC_MYSN(2),SNOW_ACC_MYWN(2),SNOW_ACC_MY2N(2))
        ELSE
          CALL SNOW_PARAPET(QSNE,SNOW_ACC_B2(2),SNOW_ACC_B3(2),SNOW_ACC_H3(2),&
                            SNOW_ACC_H(2),SNOW_ACC_LSN(2),SNOW_ACC_MYWN(2))
        ENDIF
      ENDIF

      IF (SNOW_CONDITION(3)==2)&
      CALL SNOW_PARAPET(QSNE,SNOW_ACC_B2(3),SNOW_ACC_B3(3),SNOW_ACC_H3(3),&
                        SNOW_ACC_H(3),SNOW_ACC_LSN(3),SNOW_ACC_MYWN(3))

      END SUBROUTINE DEFSFORM


      SUBROUTINE SNOW_PARAPET(QSNE,B2,B3,H3,H,LS,MYW)

      IMPLICIT NONE

      INTEGER   B2,B3,H3,H,LS
      REAL      QSNE,MYW,MY2
      REAL      BW,HW,HSW,HSL,LSW,LSL,MYWW,MYWL
      REAL      ALFA,MYWW_MAX,MYWL_MAX
      REAL      EPS

      INCLUDE 'GENERL.INS'
      INCLUDE 'TEST.INS'

      EPS=.001

      IF (NORM=='EC'.AND.INAX==1) THEN ! DS

!     Windward accumulation

        BW=B2+B3
        HW=H3
        HW=MAX(HW,1500.)
        HSW=H
        LSW=MIN(BW,2.*HSW)
        ALFA=MAX(HSW**2/(BW*HW),BW/(25.*HW))
        IF (ALFA<=.2) THEN
          MYWW_MAX=2.
        ELSEIF (ALFA<.4) THEN
          MYWW_MAX=10.*ALFA
        ELSE
          MYWW_MAX=4.
        ENDIF
        IF (QSNE<EPS) THEN
          MYWW=MYWW_MAX
        ELSE
          MYWW=MIN(2.E-3*HSW/QSNE,MYWW_MAX)
        ENDIF

!     Leeward accumulation

        HSL=H
        LSL=MIN(5.*HSL,BW)
        MYWL_MAX=2.
        IF (QSNE<EPS) THEN
          MYWL=MYWL_MAX
        ELSE
          MYWL=MIN(2.E-3*HSL/QSNE,MYWL_MAX)
        ENDIF

        MYW=MAX(.8,MYWW,MYWL)
        LS=MAX(LSW,LSL)

        IF (TST) THEN
          WRITE (TSTIUN,*) '*** SNOW_PARAPET ***'
          WRITE (TSTIUN,*) ' '
          WRITE (TSTIUN,*) 'NORM,INAX,QSNE     :',NORM,INAX,QSNE
          WRITE (TSTIUN,*) 'B2,B3,H3,H         :',B2,B3,H3,H
          WRITE (TSTIUN,*) 'BW,HW,ALFA,MYWW_MAX:',BW,HW,ALFA,MYWW_MAX
          WRITE (TSTIUN,*) 'MYWW,LSW           :',MYWW,LSW
          WRITE (TSTIUN,*) 'MYWL,LSL           :',MYWL,LSL
          WRITE (TSTIUN,*) 'MYW,LS             :',MYW,LS
        ENDIF

      ELSE ! Other than DS

        LS=2*H
        IF (QSNE<EPS) THEN
          MYW=9.99
        ELSE
          MYW=MAX(.8,2.E-3*H/QSNE)
        ENDIF

      ENDIF

      MY2=MYW

      CALL SNOW_ACC_LS_VALIDATE(LS)
      CALL SNOW_ACC_MY_VALIDATE(2,MYW,MY2)

      END SUBROUTINE SNOW_PARAPET


      SUBROUTINE SNOW_TALL_CONSTR(BS_P5_10,QSNE,VL,SNOW_COND3_2,B1,B2,B3,H3,ALFA3R,&
                                  BS,ALFAS,H,LS,MYS,MYW,MY2)

      IMPLICIT NONE

      INTEGER   B1,B2,B3,H3,BS,H,LS
      REAL      QSNE,VL,ALFA3R,ALFAS,MYS,MYW,MY2
      REAL      PI,EPS,ALFASR,BW,HW,HSW,HSL,ALFA3,BFAC
      REAL      MY1S,LSW,LSL,MYWW,MYWL
      REAL      VHLP,VHLP1,VHLP2,MYHLP1,MYHLP2
      REAL      REA,MY1_I,MYW_I,MYW_II,VOL_I,MY_ACC
      REAL      ALFA,MYWW_MAX,MYWL_MAX
      LOGICAL   BS_P5_10,SNOW_COND3_2

      INCLUDE 'GENERL.INS'
      INCLUDE 'TEST.INS'

      PI=4.*ATAN(1.)
      EPS=.001

!     Local snow accumulation at the abutting building, case (i)

      IF (NORM=='EC'.AND.INAX==1) THEN
        ALFA3=ALFA3R*180./PI
        ALFASR=ALFAS*PI/180.

!     Windward accumulation

        IF (ALFA3<30.) THEN
          BW=B2+B3
          HW=H3
        ELSE
          BW=B2
          HW=H3+B3*TAN(ALFA3R)
        ENDIF
        HW=MAX(HW,1500.)
        IF (ALFAS<30.) THEN
          HSW=H
        ELSE
          HSW=H+BS*TAN(ALFASR)
        ENDIF
        LSW=MIN(BW,2.*HSW)
        ALFA=MAX(HSW**2/(BW*HW),BW/(25.*HW))
        IF (ALFA<=.2) THEN
          MYWW_MAX=2.
        ELSEIF (ALFA<.4) THEN
          MYWW_MAX=10.*ALFA
        ELSE
          MYWW_MAX=4.
        ENDIF
        IF (QSNE<EPS) THEN
          MYWW=MYWW_MAX
        ELSE
          MYWW=MIN(2.E-3*HSW/QSNE,MYWW_MAX)
        ENDIF

!     Leeward accumulation

        HSL=H+BS*TAN(ALFASR)
        LSL=MIN(5.*HSL,BW)
        MYWL_MAX=2.
        IF (QSNE<EPS) THEN
          MYWL=MYWL_MAX
        ELSE
          MYWL=MIN(2.E-3*HSL/QSNE,MYWL_MAX)
        ENDIF

        MYW=MAX(.8,MYWW,MYWL)

        LS=MAX(LSW,LSL)

        IF (TST) THEN
          WRITE (TSTIUN,*) '*** SNOW_TALL_CONSTR ***'
          WRITE (TSTIUN,*) ' '
          WRITE (TSTIUN,*) 'NORM,INAX,QSNE:',NORM,INAX,QSNE
          WRITE (TSTIUN,*) 'ALFA3,ALFA3R  :',ALFA3,ALFA3R
          WRITE (TSTIUN,*) 'ALFAS,ALFASR  :',ALFAS,ALFASR
          WRITE (TSTIUN,*) 'MYWW,LSW,VOLW :',MYWW,LSW,.5*MYWW*LSW
          WRITE (TSTIUN,*) 'MYWL,LSL,VOLL :',MYWL,LSL,.5*MYWL*LSL
          WRITE (TSTIUN,*) 'MAX VOLUME    :',.5*MAX(MYWW,MYWL)*LS
        ENDIF

      ELSE

        IF (QSNE<EPS) THEN
          MYW=9.99
        ELSE
          MYW=MAX(.8,2.E-3*H/QSNE)
        ENDIF
        IF (H>100) MYW=MIN(MYW,.5*(B1+B2)/H)

        LS=2*H
      ENDIF

      CALL SNOW_ACC_LS_VALIDATE(LS)

!     Trough roof low roof surface accumulation, case (ii)

      IF (SNOW_COND3_2) THEN

!     Calculate case (i) snow acc. as total minimum for case (ii)

        IF (NORM=='EC'.AND.INAX==4) THEN ! DIN EN
          MYW=MIN(MYW,2.4)
        ELSE
          CALL SNOW_ACC_MY_VALIDATE(3,MYW,REA)
        ENDIF
        MYW_I=MYW
        VHLP2=MIN(VL,60.)
        CALL SNOW_MY1(NORM,BS_P5_10,VHLP2,MY1_I)

        VOL_I=MY1_I*B2
        MY_ACC=MYW_I-MY1_I
        IF (MY_ACC>EPS) THEN
          VOL_I=VOL_I+.5*MY_ACC*LS
          IF (LS>B2) VOL_I=VOL_I-.5*MY_ACC*(LS-B2)**2/LS
        ENDIF

        VHLP1=MIN(ALFAS,60.)
        VHLP=.5*(VHLP1+VHLP2)
        CALL SNOW_MY1(NORM,BS_P5_10,VHLP,MYHLP1)
        CALL SNOW_MY2(NORM,INAX,VHLP,MYHLP2)
        BFAC=1.*BS/B2
        MYW_II=(1.+BFAC)*MYHLP2-BFAC*MYHLP1

!     Validate total acc case (ii) >= case (i)

        MYW=MAX(MYW_II,2.*VOL_I/B2-MYHLP1)

        IF (TST) THEN
          WRITE (TSTIUN,*) '*** SNOW_TALL_CONSTR ***'
          WRITE (TSTIUN,*) ' '
          WRITE (TSTIUN,*) 'SNOW_COND3_2    :',SNOW_COND3_2
          WRITE (TSTIUN,*) 'NORM,INAX       :',NORM,INAX
          WRITE (TSTIUN,*) 'VHLP1,VHLP2,VHLP:',VHLP1,VHLP2,VHLP
          WRITE (TSTIUN,*) 'MYHLP1,MYHLP2   :',MYHLP1,MYHLP2
          WRITE (TSTIUN,*) 'BS,B2,BFAC      :',BS,B2,BFAC
          WRITE (TSTIUN,*) 'MY1_I,MYW_I     :',MY1_I,MYW_I
          WRITE (TSTIUN,*) 'LS (local),VOL_I:',LS,VOL_I
          WRITE (TSTIUN,*) 'MYW_II,MYW      :',MYW_II,MYW
        ENDIF

        LS=B2

      ENDIF

      IF (NINT(ALFAS)<=15) THEN
        MYS=0.
      ELSE
        CALL SNOW_MY1(NORM,BS_P5_10,ALFAS,MY1S)
        MYS=MY1S*BS/LS
      ENDIF

      MY2=MYS+MYW

      CALL SNOW_ACC_MY_VALIDATE(3,MYW,MY2)

      END SUBROUTINE SNOW_TALL_CONSTR


      SUBROUTINE W_INP_CONC_LOAD(W)

      IMPLICIT NONE

      INTEGER   W
      INTEGER   I,IL
      REAL      EPS,XMIN(4),XMAX(4)
      CHARACTER TEXTX*80
      LOGICAL   PYOP,CONFIRM_LOG

      INTEGER,EXTERNAL:: CB_L_REA_ARR_CLEAR,CB_BUT1,CB_BUT2

      INCLUDE 'GENERL.INS'
      INCLUDE 'KLAST.INS'
      INCLUDE 'LOCAL.INS'

      EPS=1.E-6

!     Settle allowable coordinate ranges

      CALL XLIMITS(XMIN(1),XMAX(1),XMIN(3),XMAX(3),XMIN(2),XMAX(2),XMIN(4),XMAX(4))

      L_NLIST16=4
      DO I=1,4
        L_LIST16(I)=TEXTX(1048+I)  ! (Component)
      ENDDO
      IF (HALTYP=='P') THEN
        L_NLIST16=3
        L_LIST16(2)=TEXTX(1048)    ! Girder
      ENDIF

      DO IL=1,NKLAST
        IF (KLKOMP(IL)=='VB') THEN
          L_INT(IL)=1
        ELSEIF (KLKOMP(IL)=='VR') THEN
          L_INT(IL)=2
        ELSEIF (KLKOMP(IL)=='HB') THEN
          L_INT(IL)=3
        ELSE
          IF (HALTYP=='P') THEN
            L_INT(IL)=2
          ELSE
            L_INT(IL)=4
          ENDIF
        ENDIF
        L_REA_ARR(IL,1)=KLKOOR(IL)
        L_REA_ARR(IL,2)=KLASTX(IL)
        L_REA_ARR(IL,3)=KLASTY(IL)
        L_REA_ARR(IL,4)=KLASTZ(IL)
      ENDDO
      DO IL=NKLAST+1,15
        L_INT(IL)=1
        L_REA_ARR(IL,1)=0.D0
        L_REA_ARR(IL,2)=0.D0
        L_REA_ARR(IL,3)=0.D0
        L_REA_ARR(IL,4)=0.D0
      ENDDO

  100 L_BUT_NO=0
      CALL W_WINDOW_OPEN(0,1191)  ! Frames

      CALL W_BOX_OPEN(1,1,'raised')

      CALL W_NL(1)
      CALL W_HEADING_TEXT(L_HEAD_TEXT,0,0)  ! Frame n
      CALL W_SPACE(3)
      CALL W_HEADING(1277,0,2)  ! Concentrated loads


!      CALL W_WRTTX(1336,0,0)  ! Load contribution
!      CALL W_WRTTX(560,1,0)   ! Component
!      CALL W_WRTTX(1071,2,0)  ! Coor.
!      CALL W_WRTTXT('PX  [kN]',1,0)
!      CALL W_WRTTXT('PY  [kN]',1,0)
!      CALL W_WRTTXT('MZ [kNm]',1,2)
!
!      DO IL=1,15
!        CALL W_WRTINT(IL,0,'6',' ',0)
!        CALL W_INPLS(0,0,L_LIST16,L_NLIST16,L_INT(IL),1,0,1,0)
!        CALL W_INPREA(0,0,L_REA_ARR(IL,1),0.D0,0.D0,1,W,' ',0,1,0)
!        CALL W_INPREA(0,0,L_REA_ARR(IL,2),0.D0,0.D0,1,W,' ',0,1,0)
!        CALL W_INPREA(0,0,L_REA_ARR(IL,3),0.D0,0.D0,1,W,' ',0,1,0)
!        CALL W_INPREA(0,0,L_REA_ARR(IL,4),0.D0,0.D0,1,W,' ',1,1,0)
!      ENDDO

      CALL W_BOX_OPEN(6,16,'no_border')

      CALL W_WRTTX(1336,0,-1)  ! Contribution
      CALL W_WRTTX(560,0,-1)   ! Component
      CALL W_WRTTX(1071,0,-1)  ! Coor.
      CALL W_WRTTXT('PX  [kN]',0,-1)
      CALL W_WRTTXT('PY  [kN]',0,-1)
      CALL W_WRTTXT('MZ [kNm]',0,-1)

      DO IL=1,15
        CALL W_WRTINT(IL,0,'6',' ',-1)
        CALL W_INPLS(0,0,L_LIST16,L_NLIST16,L_INT(IL),0,0,1,0)
        CALL W_BC(1)
        CALL W_INPREA(0,0,L_REA_ARR(IL,1),0.D0,0.D0,0,W,' ',0,1,0)
        CALL W_BC(1)
        CALL W_INPREA(0,0,L_REA_ARR(IL,2),0.D0,0.D0,0,W,' ',0,1,0)
        CALL W_BC(1)
        CALL W_INPREA(0,0,L_REA_ARR(IL,3),0.D0,0.D0,0,W,' ',0,1,0)
        CALL W_BC(1)
        CALL W_INPREA(0,0,L_REA_ARR(IL,4),0.D0,0.D0,0,W,' ',0,1,0)
        CALL W_BC(1)
      ENDDO

      CALL W_NL(1)
      CALL W_CENTRE
      CALL W_BUTTON(0,1020,0,CB_L_REA_ARR_CLEAR,.FALSE.)  ! Reset
      CALL W_BUTTON(-1,1322,0,CB_BUT2,.FALSE.)   ! OK, &Plot
      CALL W_BOX_CLOSE

      CALL W_OK_CANCEL_BUTTON(368,CB_BUT1)

      CALL W_WINDOW_END

      IF (L_BUT_NO==0) RETURN

      DO NKLAST=15,1,-1
        DO I=2,4
          IF (ABS(L_REA_ARR(NKLAST,I))>EPS) GOTO 200
        ENDDO
      ENDDO
      RETURN  !NKLAST==0

!     Validate data

  200 PYOP=.FALSE.
      DO IL=1,NKLAST
        IF (L_REA_ARR(IL,1)<XMIN(L_INT(IL)).OR.L_REA_ARR(IL,1)>XMAX(L_INT(IL))) THEN
     	    CALL W_ERROR(1337)  ! Illegal coordinate
     	    GOTO 100
     	  ENDIF
     	  IF (L_REA_ARR(IL,3)>0.D0) PYOP=.TRUE.
      ENDDO

!     If upward directed load ask User to confirme

      IF (PYOP) THEN
        CALL W_CONFIRM_LOGICAL(0,1334,CONFIRM_LOG)
        IF (.NOT.CONFIRM_LOG) GOTO 100
      ENDIF

!     Data OK

      DO IL=1,NKLAST
        IF (L_INT(IL)==1) THEN
          KLKOMP(IL)='VB'
        ELSEIF (L_INT(IL)==2) THEN
          KLKOMP(IL)='VR'
        ELSEIF (L_INT(IL)==3) THEN
          KLKOMP(IL)='HB'
        ELSE
          KLKOMP(IL)='HR'
        ENDIF
      ENDDO
      DO IL=1,15
        KLKOOR(IL)=L_REA_ARR(IL,1)
        KLASTX(IL)=L_REA_ARR(IL,2)
        KLASTY(IL)=L_REA_ARR(IL,3)
        KLASTZ(IL)=L_REA_ARR(IL,4)
      ENDDO

      IF (L_BUT_NO==2) THEN   ! Plot
        CALL RAMPLOT(5)
        GOTO 100
      ENDIF

      END SUBROUTINE W_INP_CONC_LOAD


      SUBROUTINE TKLIMITS(XMIN,XMAX,YMIN,YMAX)

      IMPLICIT NONE

      INTEGER   XMIN,XMAX,YMIN(2),YMAX(2)
      REAL      XC(9),YC(9)

      COMMON /XYC/   XC,YC

      XMIN=NINT(XC(3))
      XMAX=NINT(XC(7))
      YMIN(1)=NINT(YC(1))
      YMAX(1)=NINT(YC(3))
      YMIN(2)=NINT(YC(9))
      YMAX(2)=NINT(YC(7))

      END SUBROUTINE TKLIMITS


      SUBROUTINE W_INP_GRAIN_LOAD(W)

      IMPLICIT NONE

      INTEGER   W
      INTEGER   NBR
      REAL      HMAXV,HMAXH
      CHARACTER TEXTX*80
      LOGICAL   FEJL

      INTEGER,EXTERNAL:: CB_BUT1,CB_BUT2,CB_BUT3,CB_REPEAT_TRUE,CB_REFRESH

      INCLUDE 'GENERL.INS'
      INCLUDE 'NATLAST.INS'
      INCLUDE 'LVLAST.INS'
      INCLUDE 'LOCAL.INS'

      FEJL=.FALSE.

!     Set allowable load heights

      HMAXV=HBV+TAGOPB/COS(VLVRAD)
      HMAXH=HBH+TAGOPB/COS(VLHRAD)

      TYPE_LIST(1)=TEXTX(1300)  ! SBI176
      TYPE_LIST(2)=TEXTX(1301)  ! Lateral coefficient

      L_INT(1)=LVMETODE

      L_REA(1)=MASFYL
      L_REA(2)=FIDEG
      L_REA(3)=DELTADEG
      L_REA(4)=LVKS
      L_REA(5)=HQLV
      L_REA(6)=HQLH
      L_REA(7)=BETADEGV
      L_REA(8)=BETADEGH
      L_REA(9)=LVFAKV
      L_REA(10)=LVFAKH
      L_REA(11)=LVELEMH

      CALL SET_CHR_RB(LV3SID,'J',L_RB(1))

  100 L_BUT_NO=0
      W_REPEAT=.FALSE.
      CALL W_WINDOW_OPEN(0,1191)                                          ! Frames

      CALL W_BOX_OPEN(1,1,'raised')

      CALL W_NL(1)
      CALL W_HEADING_TEXT(L_HEAD_TEXT,0,0)                                ! Frame n
      CALL W_SPACE(3)
      CALL W_HEADING(1313,0,2)                                            ! Grain loads

      CALL W_INPLS_CB(880,0,TYPE_LIST,2,L_INT(1),1,2,1,0,CB_REPEAT_TRUE)  ! LVMETODE

      NBR=9
      IF (L_INT(1)==1) NBR=NBR+2                                          ! SBI
      CALL W_BOX_OPEN(3,NBR,'no_border')

      CALL W_HEADING(1275,0,-3)                                           ! Grain loads

      CALL W_INPREA(1302,0,L_REA(1),0.D0,0.D0,1,W,'kg/m3',-3,1,0)         ! MAS

      IF (L_INT(1)==1) THEN  ! SBI
        CALL W_INPREA(1303,0,L_REA(2),0.D0,0.D0,1,W,'°',-3,1,0)           ! FIDEG
        CALL W_INPREA(1304,0,L_REA(3),0.D0,0.D0,1,W,'°',-3,1,0)           ! DELTA
      ELSE
        CALL W_INPREA(1305,0,L_REA(4),0.D0,0.D0,1,W,' ',-3,1,0)           ! LVKS
      ENDIF

      CALL W_HEADING(1306,0,-1)                                           ! Filling
      CALL W_HEADING(557,0,-1)                                            ! Left side
      CALL W_HEADING(558,0,-1)                                            ! Right side
      CALL W_INPREA2(1307,0,L_REA(5),L_REA(6),0.D0,0.D0,1,W,' ',-3,1,0)   ! HQLV/H
      IF (L_INT(1)==1) THEN  ! SBI
        CALL W_INPREA2(1308,0,L_REA(7),L_REA(8),0.D0,0.D0,1,W,'°',-3,1,0) ! BETAV/H
      ENDIF
      CALL W_INPREA2(1309,0,L_REA(9),L_REA(10),0.D0,0.D0,1,W,' ',-3,1,0)  ! LVFAKV/H

      CALL W_HEADING(1310,0,-3)                                           ! Element
      CALL W_RADIO_BUTTON_CB(1,1311,L_RB(1),-3,CB_REFRESH)                ! LV3SID
      CALL W_INPREA(1312,0,L_REA(11),0.D0,0.D0,1,W,' ',-3,L_RB(1),0)      ! LVELEMH

      CALL W_BOX_CLOSE

      CALL W_CENTRE
      CALL W_BUTTON(-1,1321,0,CB_BUT2,.FALSE.)   ! OK, &Comput
      CALL W_BUTTON(-1,1322,2,CB_BUT3,.FALSE.)   ! OK, &Plot

      CALL W_OK_CANCEL_BUTTON(368,CB_BUT1)

      CALL W_WINDOW_END
      IF (W_REPEAT) GOTO 100

      IF (L_BUT_NO>0) THEN

!     Validate data

        CALL VALIDATE_REA8_MIN(L_REA(1),8,0.D0)
        CALL VALIDATE_REA8_MIN(L_REA(11),1,0.D0)

        IF (L_REA(5)>HMAXV.OR.L_REA(6)>HMAXH) THEN  ! HQLV/H
          CALL W_ERROR(1320)                        ! Height of load exceeds ... sealing
          GOTO 100
        ENDIF

        CALL SET_RB_CHR(L_RB(1),'J',LV3SID)

        LVMETODE=L_INT(1)

        MASFYL=L_REA(1)
        FIDEG=L_REA(2)
        DELTADEG=L_REA(3)
        LVKS=L_REA(4)
        HQLV=L_REA(5)
        HQLH=L_REA(6)
        BETADEGV=L_REA(7)
        BETADEGH=L_REA(8)
        LVFAKV=L_REA(9)
        LVFAKH=L_REA(10)
        IF (LV3SID=='J') THEN
          LVELEMH=L_REA(11)
        ELSE
          LVELEMH=0.
        ENDIF
      ENDIF

      CALL LVLOADS(FEJL)          ! Calculate grain loads
      IF (FEJL) THEN
        FEJL=.FALSE.
        GOTO 100
      ENDIF

      IF (L_BUT_NO==2) THEN       ! Present grain loads
        CALL LVLOADS_PRESENT
        GOTO 100
      ELSEIF (L_BUT_NO==3) THEN   ! Plot grain loads
        CALL RAMPLOT(3)
        GOTO 100
      ENDIF

      END SUBROUTINE W_INP_GRAIN_LOAD


      SUBROUTINE LVLOADS(FEJL)

      IMPLICIT NONE

      REAL      PI,BETAV,BETAH
      REAL      XKIP,RLV,RLH
      LOGICAL   FEJL

      INCLUDE 'GENERL.INS'
      INCLUDE 'LVLAST.INS'

      PI=4.*ATAN(1.)
      FEJL=.FALSE.

      IF (LVMETODE==1) THEN

        BETAV=PI/180.*BETADEGV
        BETAH=PI/180.*BETADEGH
        IF (BETAV+BETAH<1.E-6) THEN
          LVAFSTV=0.
          LVAFSTH=0.
        ELSE
          CALL RIGLELEN(XKIP,RLV,RLH)
          LVAFSTV=(FKOTE+HQLH+S*TAN(BETAH)-HQLV)/(TAN(BETAV)+TAN(BETAH))
          LVAFSTH=S-LVAFSTV
          IF (LVAFSTV<0..OR.LVAFSTV>S) FEJL=.TRUE.
          IF (LVAFSTV<=XKIP) THEN
            IF (HQLV+LVAFSTV*TAN(BETAV)>HBV+LVAFSTV*TAN(VLVRAD)) FEJL=.TRUE.
          ELSE
            IF (HQLH+LVAFSTH*TAN(BETAH)>HBH+LVAFSTH*TAN(VLHRAD)) FEJL=.TRUE.
          ENDIF
          IF (FEJL) THEN
            CALL W_ERROR(1950)    ! The top point must lie indoors
            RETURN
          ENDIF
        ENDIF

!     Lateral pressures incl. of the User-defined load factor

        CALL SBI176(FIDEG,DELTADEG,BETADEGV,LVAFSTV,LVFAKV,LVK1V,LVK2V,LVZ1V)
        CALL SBI176(FIDEG,DELTADEG,BETADEGH,LVAFSTH,LVFAKH,LVK1H,LVK2H,LVZ1H)

      ELSE

!     Lateral pressures incl. of the User-defined load factor

      	LVK1V=LVKS*LVFAKV
      	LVZ1V=HQLV
      	LVK1H=LVKS*LVFAKH
      	LVZ1H=HQLH

      ENDIF

      CALL LVPN(MASFYL,HQLV,LVK1V,LVK2V,LVZ1V,LVPN1V,LVPN2V)
      CALL LVPN(MASFYL,HQLH,LVK1H,LVK2H,LVZ1H,LVPN1H,LVPN2H)

      END SUBROUTINE LVLOADS

!     Grain loads in accordance with SBI-anvisning 176 (DK), 1991
!
!     FIDEG    :  Internal angle of friction [degrees]
!     DELTADEG :  Angle of wall friction     [degrees]
!     BETADEG  :  Slope angle                [degrees]
!     TOPAFST  :  Distance to top
!     LVFAK    :  User load factor
!     KS       :  Coefficient of lateral pressure for inclined surface
!     KV       :  Coefficient of lateral pressure for horizontal surface
!     Z1       :  depth for change in pressure gradient

      SUBROUTINE SBI176(FIDEG,DELTADEG,BETADEG,TOPAFST,LVFAK,KS,KV,Z1)

      IMPLICIT NONE

      INTEGER   I1,I2
      REAL      FIDEG,DELTADEG,BETADEG,TOPAFST,LVFAK,KS,KV,Z1
      REAL      PI,FI,DELTA,BETA
      REAL      KSR(6),KVR(6),KSG(6),KVG(6)
      REAL      KSRU,KVRU,KSGLAT,KVGLAT
      REAL      FIINDEX,FAK1,FAK2

      DATA      KSR/.97,.933,.883,.821,.75,.671/
      DATA      KVR/.64,.516,.415,.333,.266,.211/
      DATA      KSG/1.024,1.009,.975,.925,.86,.782/
      DATA      KVG/0.704,.589,.49,.406,.333,.271/

!     Angles in radians

      PI=4.*ATAN(1.)
      FI=FIDEG*PI/180.
      DELTA=DELTADEG*PI/180.
      BETA=BETADEG*PI/180.

!     Lateral pressure coefficients for given FIDEG
!     Linear interpolation, table 1

      IF (FIDEG<=10.) THEN
        I1=1
        FAK1=1.
      ELSEIF (FIDEG<35.) THEN
        FIINDEX=FIDEG/5.-1.
        I1=INT(FIINDEX)
        FAK1=1.*(I1+1)-FIINDEX
      ELSE
        I1=5
        FAK1=0.
      ENDIF
      I2=I1+1
      FAK2=1.-FAK1

      KSRU=KSR(I1)*FAK1+KSR(I2)*FAK2
      KVRU=KVR(I1)*FAK1+KVR(I2)*FAK2
      KSGLAT=KSG(I1)*FAK1+KSG(I2)*FAK2
      KVGLAT=KVG(I1)*FAK1+KVG(I2)*FAK2

!     Lateral pressure coefficients for given DELTA
!     Linear interpolation between rough and smooth surface

      FAK1=TAN(DELTA)/TAN(FI)
      IF (FAK1>1.) FAK1=1.
      FAK2=1.-FAK1

      KS=KSRU*FAK1+KSGLAT*FAK2
      KV=KVRU*FAK1+KVGLAT*FAK2

!     Depth Z1

      Z1=.5*TOPAFST*KV*TAN(BETA)/(KS-KV)

!     Include the User-defined load factor

      KS=LVFAK*KS
      KV=LVFAK*KV

      END SUBROUTINE SBI176


      SUBROUTINE XLIMITS(XMINVB,XMAXVB,XMINHB,XMAXHB,XMINVR,XMAXVR,XMINHR,XMAXHR)

      IMPLICIT NONE

      REAL      XC(9),YC(9)
      REAL      XMINVB,XMAXVB,XMINHB,XMAXHB,XMINVR,XMAXVR,XMINHR,XMAXHR

      COMMON /XYC/   XC,YC

      INCLUDE 'GENERL.INS'

!     Leg

      XMINVB=0.
      XMAXVB=YC(3)
      XMINHB=0.
      IF (HALTYP=='S') THEN
        XMAXHB=XMAXVB
      ELSE
        XMAXHB=YC(7)-FKOTE
      ENDIF

!     Girder

      XMINVR=XC(3)*COS(VLVRAD)-(HBV-YC(3))*SIN(VLVRAD)
      IF (HALTYP=='S'.OR.HALTYP=='A') THEN
        XMAXVR=XC(5)*COS(VLVRAD)-(HBV-YC(5))*SIN(VLVRAD)
      ELSEIF (HALTYP=='P') THEN
        XMAXVR=XC(7)*COS(VLVRAD)-(HBV-YC(7))*SIN(VLVRAD)
      ENDIF
      IF (HALTYP=='S') THEN
        XMINHR=XMINVR
        XMAXHR=XMAXVR
      ELSE
        XMINHR=(S-XC(7))*COS(VLHRAD)-(FKOTE+HBH-YC(7))*SIN(VLHRAD)
        XMAXHR=(S-XC(5))*COS(VLHRAD)-(FKOTE+HBH-YC(5))*SIN(VLHRAD)
      ENDIF

      END SUBROUTINE XLIMITS


      SUBROUTINE LVPN(MASFYL,HQL,LVK1,LVK2,LVZ1,LVPN1,LVPN2)

      IMPLICIT NONE

      REAL      MASFYL,HQL,LVK1,LVK2,LVZ1,LVPN1,LVPN2
      REAL      RO

      RO=MASFYL*9.82E-6
      IF (LVZ1>HQL-1.) THEN
        LVZ1=HQL
        LVK2=0.
      ENDIF
      LVPN1=RO*LVK1*LVZ1
      LVPN2=LVPN1+RO*LVK2*(HQL-LVZ1)

      END SUBROUTINE LVPN


      SUBROUTINE LVLOADS_PRESENT

      IMPLICIT NONE

      INTEGER   I,I1

      INTEGER,EXTERNAL:: CB_ESC

      INCLUDE 'LVLAST.INS'
      INCLUDE 'LOCAL.INS'

      IF (LVMETODE==1) THEN
      	I1=1314 ! Distances to top point
      ELSE
      	I1=1319 ! Lateral pressure at base
      ENDIF

      CALL W_WINDOW_OPEN(0,1191)            ! Frames

      CALL W_BOX_OPEN(1,1,'raised')

      CALL W_NL(1)
      CALL W_HEADING_TEXT(L_HEAD_TEXT,0,0)  ! Frame n
      CALL W_SPACE(3)
      CALL W_HEADING(1313,0,2)              ! Grain loads

      CALL W_BOX_OPEN(7,1,'no_border')
      CALL W_NL(1)
      DO I=I1,1319
        CALL W_WRTTX(I,0,1)
      ENDDO
      CALL W_BOX_CLOSE

      CALL W_BOX_SPACE(5)

      CALL W_NL(1)
      IF (LVMETODE==1) THEN
      	CALL W_NL(1)
        CALL W_WRTTXT('z1',0,1)
        CALL W_WRTTXT('K1',0,1)
        CALL W_WRTTXT('K2',0,1)
        CALL W_WRTTXT('pn1',0,1)
      ENDIF
      CALL W_WRTTXT('pn2',0,1)
      CALL W_BOX_CLOSE

      CALL W_BOX_SPACE(5)

      CALL W_HEADING(557,0,1)               ! Left side
      IF (LVMETODE==1) THEN
        CALL W_WRTINT(NINT(LVAFSTV),0,'6','mm',1)
        CALL W_WRTINT(NINT(LVZ1V),0,'6','mm',1)
        CALL W_WRTREA(LVK1V,0,'6.2',' ',1)
        CALL W_WRTREA(LVK2V,0,'6.2',' ',1)
        CALL W_WRTREA(LVPN1V,0,'6.2','kN/m2',1)
      ENDIF
      CALL W_WRTREA(LVPN2V,0,'6.2','kN/m2',1)
      CALL W_BOX_CLOSE

      CALL W_BOX_SPACE(5)

      CALL W_HEADING(558,0,1)               ! Right side
      IF (LVMETODE==1) THEN
        CALL W_WRTINT(NINT(LVAFSTH),0,'6','mm',1)
        CALL W_WRTINT(NINT(LVZ1H),0,'6','mm',1)
        CALL W_WRTREA(LVK1H,0,'6.2',' ',1)
        CALL W_WRTREA(LVK2H,0,'6.2',' ',1)
        CALL W_WRTREA(LVPN1H,0,'6.2','kN/m2',1)
      ENDIF
      CALL W_WRTREA(LVPN2H,0,'6.2','kN/m2',1)
      CALL W_BOX_CLOSE

      CALL W_BOX_CLOSE
      CALL W_NL(1)
      CALL W_CENTRE
      CALL W_BUTTON(0,1008,0,CB_ESC,.FALSE.)  ! Close

      CALL W_WINDOW_END

      END SUBROUTINE LVLOADS_PRESENT


      SUBROUTINE SNOW_ACC_LS_VALIDATE(LS)

      IMPLICIT NONE

      INTEGER   LS

      INCLUDE 'GENERL.INS'

      IF (NORM=='RU') THEN
        LS=MAX(5000,LS)
        LS=MIN(LS,16000)
      ELSE
        IF (INAX==6) THEN ! OENORM
          LS=MAX(2000,LS)
          LS=MIN(LS,10000)
        ELSEIF (INAX==8) THEN ! SFS
          LS=MAX(2000,LS)
          LS=MIN(LS,6000)
        ELSE
          LS=MAX(5000,LS)
          LS=MIN(LS,15000)
        ENDIF
      ENDIF

      END SUBROUTINE SNOW_ACC_LS_VALIDATE


      SUBROUTINE SNOW_ACC_MY_VALIDATE(ICOND,MYW,MY2)

      IMPLICIT NONE

      INTEGER   ICOND
      REAL      MYW,MY2

      INCLUDE 'GENERL.INS'

      IF (NORM=='RU') THEN
        IF (ICOND==3) THEN
          MYW=MIN(MYW,4.)
        ELSE
          MYW=MIN(MYW,3.)
        ENDIF
      ELSE
        IF (INAX==1) THEN     ! DS
          MYW=MIN(MYW,4.)
        ELSEIF (INAX==4) THEN ! DIN
          IF (ICOND==3) THEN
            MY2=MIN(MY2,2.4)
          ELSE
            MYW=MIN(MYW,2.)
          ENDIF
        ELSEIF (INAX==6) THEN ! OENORM
          MYW=MIN(MYW,3.)
        ELSEIF (INAX==8) THEN ! SFS
          MYW=MIN(MYW,2.5)
        ELSE
          IF (ICOND==3) THEN
            MYW=MIN(MYW,4.)
          ELSE
            MYW=MIN(MYW,2.)
          ENDIF
        ENDIF
      ENDIF

      END SUBROUTINE SNOW_ACC_MY_VALIDATE


      SUBROUTINE FSTRUGEXI(FSTRUG)

      IMPLICIT NONE

      INTEGER   ISKIB,NSKIB
      LOGICAL   FSTRUG

      INCLUDE 'GENERL.INS'
      INCLUDE 'FLSKIB.INS'

      FSTRUG=.FALSE.
      IF (FSHAL=='J') THEN
        NSKIB=FSNSSV+FSNSSH
      	DO ISKIB=1,NSKIB
          IF (FSHALTYP(ISKIB)/='P') THEN
            FSTRUG=.TRUE.
            RETURN
          ENDIF
        ENDDO
      ENDIF

      END SUBROUTINE FSTRUGEXI

!     Total building roof to be considered as a flat roof?

      SUBROUTINE SET_FLAT_ROOF

      IMPLICIT NONE

      INTEGER   ISKIB,NSKIB

      INCLUDE 'GENERL.INS'
      INCLUDE 'FLSKIB.INS'
      INCLUDE 'NATLAST.INS'

      IF (VLV<5..AND.ABS(VLH)<5.) THEN
        FLAT_ROOF=.TRUE.
        IF (FSHAL=='J') THEN
          NSKIB=FSNSSV+FSNSSH
          DO ISKIB=1,NSKIB
            IF (MAX(FSVLV(ISKIB),FSVLH(ISKIB))>=5.) THEN
              FLAT_ROOF=.FALSE.
              EXIT
            ENDIF
          ENDDO
        ENDIF
      ELSE
        FLAT_ROOF=.FALSE.
      ENDIF

      END SUBROUTINE SET_FLAT_ROOF

      ! ======================================================================
      ! Big span effect
      INTEGER FUNCTION CB_BIG_SPAN_WREP()

      IMPLICIT NONE

      INCLUDE 'DUMVAR.INS'
      INCLUDE 'LOCAL.INS'

      ! DUM_INT=CB_BIG_SPAN()

      W_REPEAT=.TRUE.
      CB_BIG_SPAN_WREP=0

      END FUNCTION CB_BIG_SPAN_WREP
      !=========
      INTEGER FUNCTION CB_IMPORTVX()

      IMPLICIT NONE
      INCLUDE 'GENERL.INS'

      CALL CB_IMPORTVX_WREP

      CB_IMPORTVX=0

      END FUNCTION CB_IMPORTVX

      SUBROUTINE CB_IMPORTVX_WREP

      INTEGER   I,J,VX_NUMBER,FindFrictionLength

      INCLUDE 'GENERL.INS'
      INCLUDE 'RAMDAT.INS'
      INCLUDE 'LOCAL.INS'
      INCLUDE 'NATLAST.INS'

      W_REPEAT=.TRUE.

      J=1
      DO I = 1, 9
          IF (VX_CODE(i,1) /= 0) THEN
            VX_CODE(i,2)=J
            J=J+1
          END IF
      ENDDO

      IF(L_INT(36)>1) THEN
 
        IF(BIGSPAN_CHOICE2==L_INT(36)) THEN
          
          IF (INT(VX_ALLDATA(6,INT(VX_DATA(6))))==INT(VX_DATA(6))) THEN
            VX_NUMBER=INT(VX_DATA(6))
            
            DO J = 1, 18
            VX_DATA(J) = VX_ALLDATA(J,VX_NUMBER)
            ENDDO
            BIGSPAN_CHOICE2=INT(VX_CODE(INT(VX_DATA(6)),2))+1
            ! L_INT(36)=BIGSPAN_CHOICE2
            GOTO 9999
          ENDIF
          
          BIGSPAN_CHOICE2=1
          L_INT(36)=1
          GOTO 8888

        ELSE
          ! BIGSPAN_CHOICE2=L_INT(36)
          DO I = 1, 9
            IF (VX_CODE(I,2) == L_INT(36)-1) THEN
              VX_NUMBER=I

              DO J = 1, 18
              VX_DATA(J) = VX_ALLDATA(J,VX_NUMBER)
              ENDDO
              GOTO 9999
            ENDIF
          ENDDO
        
          GOTO 8888

        ENDIF

      ELSE
        IF(BIGSPAN_CHOICE2==0) THEN
          BIGSPAN_CHOICE1=1
          BIGSPAN_CHOICE2=1
          GOTO 8888
        ELSE
          IF(BIGSPAN_CHOICE2==1) THEN
          BIGSPAN_CHOICE2=1

          VX_DATA(1) = BS_FORCEINPUT(1)   ! c/c
          VX_DATA(2) = BS_FORCEINPUT(2)     ! q_p
          VX_DATA(3) = BS_FORCEINPUT(3)     ! c_p
          VX_DATA(4) = BS_FORCEINPUT(4)     ! c_fr
          VX_DATA(5) = BS_FORCEINPUT(5)     ! fr_length
          VX_DATA(6) = BS_FORCEINPUT(6)     ! slot number for wind bracing
          VX_DATA(7) = BS_FORCEINPUT(7)     ! Fixation load - number of frames
          VX_DATA(8) = BS_FORCEINPUT(8)     ! Fixation load - cross sectional area of flange
          VX_DATA(9) = BS_FORCEINPUT(9)     ! Fixation load - yield strength
          VX_DATA(10) = BS_FORCEINPUT(10)   ! Skew load - number of frames
          VX_DATA(11) = BS_FORCEINPUT(11)   ! Skew load - load / horizontal projection
          ENDIF
        ENDIF

          GOTO 9999

      ENDIF

   8888 CONTINUE

      ! For GF this often is set to 0. Must avoid divide by 0.
      IF(ZR<1) THEN
        VX_DATA(1)=1
      ELSE
        VX_DATA(1)=ZR
      ENDIF
      VX_DATA(2)=QVINDN
      VX_DATA(3)=1
      VX_DATA(4)=0.025
      VX_DATA(5)=FindFrictionLength()
      VX_DATA(6)=0
      IF(ZR<1) THEN
        VX_DATA(7)=1
      ELSE
        VX_DATA(7)=LL/ZR+1
      ENDIF
      VX_DATA(8)=1850
      VX_DATA(9)=235
      IF(ZR<1) THEN
        VX_DATA(10)=1
      ELSE
        VX_DATA(10)=LL/ZR+1
      ENDIF
      VX_DATA(11)=8
      DO I=12,18
        VX_DATA(I) = 0
      ENDDO

      GOTO 9999

   9999 RETURN

      END SUBROUTINE CB_IMPORTVX_WREP

      SUBROUTINE W_INP_VX(N,W,VX_SUM,CHOICE2,frictionLength_char)

      IMPLICIT NONE

      INTEGER   W,N,VX_SUM,CHOICE2
      CHARACTER frictionLength_char*6
      INTEGER,EXTERNAL:: CB_IMPORTVX

      INCLUDE 'GENERL.INS'
      INCLUDE 'LOCAL.INS'
      INCLUDE 'NATLAST.INS'

      CALL W_WRTTX(3187,0,0)  ! Import from windbracing:
      CALL W_BC(1)
      CALL W_INPLS_CB(0,0,L_LIST16(25),VX_SUM+1,L_INT(N),0,1,1,0,CB_IMPORTVX)
      CALL W_BC(1)

      IF(CHOICE2>1) THEN
         CALL W_BOX_OPEN(6,7,'no_border') ! 'shaded'

        CALL W_BOX_EMPTY_CELLS(6) ! this is separator from the previous content

        CALL W_HEADING(589,0,0)  ! General
        CALL W_BC(1)
        CALL W_SPACE(18)
        CALL W_BC(2)
              CALL W_HEADING(1534,1,0)  ! Skew Load
              CALL W_BC(1)
              CALL W_SPACE(18)
              CALL W_BC(2)


        CALL W_WRTTX(3188,0,0)  ! Frame c/c distance
        CALL W_BC(1)
        CALL W_BC(1)
        CALL W_RO_REA(0,VX_DATA(1),0,6,'mm',0)
        CALL W_BC(1)

              CALL W_WRTTX(3196,1,0)  ! Skew load - number of frames
              CALL W_BC(1)
              CALL W_BC(1)
              CALL W_RO_REA(0,VX_DATA(10),0,6,' ',0)
              CALL W_BC(1)

        CALL W_WRTTX(3189,0,0)  ! Basic wind load
        CALL W_BC(1)
        CALL W_BC(1)
        CALL W_RO_REA(0,VX_DATA(2),0,5,'kN/m2',0)
        CALL W_BC(1)

              CALL W_WRTTX(3197,1,0)  ! Skew load - Load/frame, horizontal projection
              CALL W_BC(1)
              CALL W_BC(1)
              CALL W_RO_REA(0,VX_DATA(11),0,6,'kN/m',0)
              CALL W_BC(1)

        CALL W_WRTTX(3190,0,0)  ! Total shape factor
        CALL W_BC(1)
        CALL W_BC(1)
        CALL W_RO_REA(0,VX_DATA(3),0,5,' ',0)
        CALL W_BC(1)

              CALL W_BOX_EMPTY_CELLS(3)

        CALL W_WRTTX(3191,0,0)  ! Friction factor
        CALL W_BC(1)
        CALL W_BC(1)
        CALL W_RO_REA(0,VX_DATA(4),0,5,' ',0)
        CALL W_BC(1)

              CALL W_BOX_EMPTY_CELLS(3)

        CALL W_WRTTX(3192,0,0)  ! Friction zone length
        CALL W_BC(1)
        CALL W_STATUS_TEXT('( '//TRIM(ADJUSTL(frictionLength_char))//' )',-2,0)
        CALL W_SPACE(3)
        CALL W_BC(1)
        CALL W_RO_REA(0,VX_DATA(5),0,6,'mm',0)
        CALL W_BC(1)

              CALL W_BOX_EMPTY_CELLS(3)

      ELSE
        CALL W_BOX_OPEN(6,7,'no_border') ! 'shaded'

        CALL W_BOX_EMPTY_CELLS(6) ! this is separator from the previous content

        CALL W_HEADING(589,0,0)  ! General
        CALL W_BC(1)
        CALL W_SPACE(18)
        CALL W_BC(2)
              CALL W_HEADING(1534,1,0)  ! Skew Load
              CALL W_BC(1)
              CALL W_SPACE(18)
              CALL W_BC(2)

        CALL W_WRTTX(3188,0,0)  ! Frame c/c distance
        CALL W_BC(1)
        CALL W_BC(1)
        CALL W_INPREA(0,0,VX_DATA(1),0.D0,0.D0,0,W,'mm',0,1,0)

        CALL W_SPACE(12) ! additional space for column 3, that is only present in this CELL
        CALL W_BC(1)

              CALL W_WRTTX(3196,1,0)  ! Skew load - number of frames
              CALL W_BC(1)
              CALL W_BC(1)
              CALL W_INPREA(0,0,VX_DATA(10),0.D0,0.D0,0,W,' ',0,1,0)
              CALL W_BC(1)

        CALL W_WRTTX(3189,0,0)  ! Basic wind load
        CALL W_BC(1)
        CALL W_BC(1)
        CALL W_INPREA(0,0,VX_DATA(2),0.D0,0.D0,0,W,'kN/m2',0,1,0)
        CALL W_BC(1)

              CALL W_WRTTX(3197,1,0)  ! Skew load - Load/frame, horizontal projection
              CALL W_BC(1)
              CALL W_BC(1)
              CALL W_INPREA(0,0,VX_DATA(11),0.D0,0.D0,0,W,'kN/m',0,1,0)
              CALL W_BC(1)

        CALL W_WRTTX(3190,0,0)  ! Total shape factor
        CALL W_BC(1)
        CALL W_BC(1)
        CALL W_INPREA(0,0,VX_DATA(3),0.D0,0.D0,0,W,' ',0,1,0)
        CALL W_BC(1)

              CALL W_BOX_EMPTY_CELLS(3)

        CALL W_WRTTX(3191,0,0)  ! Friction factor
        CALL W_BC(1)
        CALL W_BC(1)
        CALL W_INPREA(0,0,VX_DATA(4),0.D0,0.D0,0,W,' ',0,1,0)
        CALL W_BC(1)

              CALL W_BOX_EMPTY_CELLS(3)

        CALL W_WRTTX(3192,0,0)  ! Friction zone length
        CALL W_BC(1)
        CALL W_STATUS_TEXT('( '//TRIM(ADJUSTL(frictionLength_char))//' )',-2,0)
        CALL W_SPACE(3)
        CALL W_BC(1)
        CALL W_INPREA(0,0,VX_DATA(5),0.D0,0.D0,0,W,'mm',0,1,0)
        CALL W_BC(1)

              CALL W_BOX_EMPTY_CELLS(3)

        ENDIF

      END SUBROUTINE W_INP_VX

      ! this reads required data for a selected windbracing slot
      SUBROUTINE READWX(KODE)

        IMPLICIT NONE

        INTEGER   KODE
        INTEGER   FFMT,I,I1,I2
        CHARACTER FFILNAV*255
        LOGICAL   EXI

        INCLUDE 'DKACOM.INS'
        INCLUDE 'UNIT1.INS'
        INCLUDE 'VGIT.INS'

        IOERR=0
        IF (PKODE=='B') THEN
          CALL HALFIL(KODE,'WIN',FFILNAV)
        ELSE
          CALL HALFIL(KODE,'VIN',FFILNAV)
        ENDIF
        INQUIRE (FILE=FFILNAV,EXIST=EXI)
        IF (.NOT.EXI) THEN
          IOERR=1
          RETURN
        ENDIF

        CALL CHKUNIT(NTEMP,29)
        OPEN (NTEMP,FILE=FFILNAV,ACCESS='DIRECT',RECL=24,FORM='UNFORMATTED',ERR=9999)

        READ (NTEMP,REC=1,ERR=9999) FFMT
        IF (FFMT>=2) THEN
          READ (NTEMP,REC=2,ERR=9999) FHNSPR,SSNSPR,VGFAGDEF,VGLASTDEF
        ELSE
          READ (NTEMP,REC=2,ERR=9999) FHNSPR,SSNSPR
          VGFAGDEF=.FALSE.
          VGLASTDEF=.FALSE.
        ENDIF
        READ (NTEMP,REC=3,ERR=9999) CG,CT,FYKD,QBVGIT,FHAREA,FHFYD
        READ (NTEMP,REC=4,ERR=9999) SSLAST,VGSIDE,VGSOJK,WTERKOTE,WSOJFKOT,RB_VG_UDH_CLAD

        I2=10

        DO I=1,I2
          I1=3+2*I
          READ (NTEMP,REC=I1,ERR=9999) RAMAFS(I)
        ENDDO

        CLOSE (UNIT=NTEMP)
        RETURN

   9999 IOERR=2

        END SUBROUTINE READWX

        ! this tells what bracings were calculated
        SUBROUTINE FIND_VX(VX_CODE,VX_LICENSE)

        IMPLICIT NONE

        INTEGER   I,NFIL,GITNUMDEF
        INTEGER   VX_CODE(9,2)
        LOGICAL   VX_LICENSE

        INCLUDE 'DKACOM.INS'
        INCLUDE 'VGIT.INS'
        INCLUDE 'GENERL.INS'

        PROGMOD='WINDLATT'

        CALL WindBracingLicenseCheck(NORM,INAX,36,VX_LICENSE)

        IF (VX_LICENSE) THEN
          DO I=1,9
            VX_CODE(I,1)=0
            VX_CODE(I,2)=0
          ENDDO
        ELSE
          CALL HALFIL_KODE(VX_CODE,NFIL,GITNUMDEF)
          IF (GITNUM<1.OR.9<GITNUM) THEN
            GITNUM=GITNUMDEF
          ENDIF
        ENDIF

        PROGMOD='HALBERI'

        END SUBROUTINE FIND_VX

        SUBROUTINE LOAD_VX(CODE,ALLDATA,VX_LICENSE)
        IMPLICIT NONE

        INTEGER   I,J, FindFrictionLength, frictionLength
        INTEGER   CODE(9,2)
        REAL*8    ALLDATA(18,9)
        LOGICAL   VX_LICENSE
        INCLUDE 'VGIT.INS'
        INCLUDE 'NATLAST.INS'

        ! Friction length is calculated based on current building dimensions
        ! It is NOT imported from windbracing !! This is to ensure the value is calculated correctly
        ! after increasing building length WITHOUT recalculating bracings first.
        frictionLength = FindFrictionLength()

        CALL FIND_VX(CODE,VX_LICENSE)
        ! CODE=VX_CODE

        DO I=1,9
          IF (CODE(I,1) /= 0) THEN
            CALL  READWX(I)
            ALLDATA(1,I)=RAMAFS(1)
            ALLDATA(2,I)=QBVGIT
            ALLDATA(3,I)=CG
            ALLDATA(4,I)=CT
            ALLDATA(5,I)=frictionLength    ! see comment above
            ALLDATA(6,I)=I                 ! slot number for wind bracing
            ALLDATA(7,I)=FHNSPR            ! Fixation load - number of frames
            ALLDATA(8,I)=FHAREA            ! Fixation load - cross sectional area of flange
            ALLDATA(9,I)=FHFYD             ! Fixation load - yield strength
            ALLDATA(10,I)=SSNSPR           ! Skew load - number of frames
            ALLDATA(11,I)=SSLAST           ! Skew load - load / horizontal projection
            ALLDATA(12,I)=0                ! PLACEHOLDER
            ALLDATA(13,I)=0                ! PLACEHOLDER
            ALLDATA(14,I)=0                ! PLACEHOLDER
            ALLDATA(15,I)=0                ! PLACEHOLDER
            ALLDATA(16,I)=0                ! PLACEHOLDER
            ALLDATA(17,I)=0                ! PLACEHOLDER
            ALLDATA(18,I)=0                ! PLACEHOLDER
          ELSE
            DO J=1,18
            ALLDATA(J,I)=0
            ALLDATA(1,I)=100
            ENDDO
          END IF
        END DO
        END SUBROUTINE LOAD_VX

      INTEGER FUNCTION FindFrictionLength()

      IMPLICIT NONE

      REAL      L_FR
      REAL      XKIPL,H_FR

      INCLUDE 'GENERL.INS'

      CALL CALXKIP_LK(XKIPL)
      H_FR=HLV+XKIPL*TAN(VLVRAD)-MAX(0,FKOTE)
      L_FR=MAX(0.,LL-MIN(2.*BL,4.*H_FR))
      L_FR=100*NINT(L_FR/100.)

      FindFrictionLength = NINT(L_FR)

      END FUNCTION FindFrictionLength