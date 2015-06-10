	SUBROUTINE DM_SRCH ( iflno, type, nkey, keyloc, keyval,
     +			     irwcl, iret )
C************************************************************************
C* DM_SRCH								*
C*									*
C* This subroutine searches a DM file for rows or columns which		*
C* match the given input values.					*
C*									*
C* DM_SRCH  ( IFLNO, TYPE, NKEY, KEYLOC, KEYVAL, IRWCL, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	TYPE		CHAR*		Dimension type : ROW or COL	*
C*	NKEY		INTEGER		Number of keys to search	*
C*	KEYLOC (NKEY)	INTEGER		Key locations			*
C*	KEYVAL (NKEY)	INTEGER		Key values			*
C*									*
C* Output parameters:							*
C*	IRWCL		INTEGER		Search location			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file is not open		*
C*					-17 = search criteria not met	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/87						*
C* J. Whistler/NSSFC	 3/95	Changed the search to be more efficient	*
C* m. gamazaychikov/CWS 04/11   Add code for A2DB connectivity          *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
	INCLUDE		'dbcmn.cmn'
C
	CHARACTER*(*)	type
	INTEGER		keyloc (*), keyval (*)
	LOGICAL		done
        INTEGER         intdtf (3), level (2)
        CHARACTER       dttim*20, qtype*8,garea*30,stinfo*25,
     +                  astnstr(4)*10, gdattm (2)*22, parm*14,
     +                  vcord*5, src*21, datauri*254, amodel*21
c        CHARACTER       dbstn*4
C------------------------------------------------------------------------
	irwcl = 0
C
C*      For A2DB requests - mimic the search given the search parms.
C
        IF ( dbread ) THEN
	   print *, 'A2DB requests not implemented!'
	   iret = -17
C           qtype = " "
C           irwcl = 1
C           iret = 0
C           IF ( type .eq. 'ROW' ) THEN
C              CALL TI_CDTM ( keyval(1), keyval(2), dttim, ier)
C              IF ( ier .eq. 0 ) dbdttm = dttim
C            ELSE IF ( type .eq. 'COL' ) THEN
C              IF ( INDEX(dbdatasrc,'grid')  .gt. 0 ) THEN
C                 qtype = "dataURI"
C
C*               Change the two integer time into a three integer 
C*               time and then into a character GEMPAK grid time.
C
C                 CALL TG_FTOI  ( keyval, intdtf, ier )
C                 CALL TG_ITOC  ( intdtf, gdattm (1), ier )
C
C*               Change the second GEMPAK time.
C
C                 CALL TG_FTOI  ( keyval (3), intdtf, ier )
C                 CALL TG_ITOC  ( intdtf, gdattm (2), ier )
C
C*               Move the levels and the vertical coordinate.
C
C                 IF ( nkey .ne. 10 ) THEN
C                    ilevel = 0
C                    ivcord = 0
C                    iparm = 5
C                 ELSE
C                    level (1) = keyval (5)
C                    level (2) = keyval (6)
C                    ivcord    = keyval (7)
C                    ilevel = level(1)
C                    iparm = 8
C                 END IF

C
C*               Change the last three integers into the parameter name.
C
C                 CALL ST_ITOS  ( keyval (iparm), 3, nchar, parm, ier )
C                 CALL LV_CCRD  ( ivcord, vcord, ier )
C                 CALL ST_LCUC ( dbdatasrc, src, ier )
C                 CALL ST_NULL ( qtype,  qtype,  lstr, ier ) 
C                 CALL ST_NULL ( src,    src,    lstr, ier )
C                 CALL ST_NULL ( gdattm, gdattm, lstr, ier )
C                 CALL ST_NULL ( vcord,  vcord,  lstr, ier )
C                 CALL ST_NULL ( parm,   parm,   lstr, ier )
C                 CALL ST_NULL ( dbmodel, amodel,   lstr, ier )
c                 CALL ST_NULL ( evtname, evtname,   lstr, ier )
C                 CALL DB_GETDURI ( qtype, src, amodel, gdattm,
c     +                             vcord, parm, evtname, ilevel, 
C     +                             vcord, parm, ilevel, 
C     +                             datauri, ldt, ier)
C                 IF ( ier .ne. 0 ) THEN 
C                    iret = -17
C                    RETURN
C                 END IF
C                dburi = datauri(:ldt)
C                 iret = 0
C                 RETURN
C              END IF
C              IF ( INDEX(dbdatasrc,'metar')  .gt. 0 ) qtype = "stidqry"
C              IF ( INDEX(dbdatasrc,'bufrua') .gt. 0 ) qtype = "stnmqry"
C              IF ( INDEX(dbdatasrc,'synop')  .gt. 0 ) qtype = "stnmqry"
              
C              IF ( qtype .eq. " " ) RETURN
C              CALL ST_NULL ( qtype, qtype, lqtype, ier ) 
C              CALL DB_GETGAREA (nkey, type, keyval, keyval, garea, ier)
C              CALL ST_NULL ( garea, garea, lgarea, ier)
C              CALL DB_GETSTINFO (qtype,garea,stinfo,lstinfo,ier)
C              IF ( ier .ne. 0 ) THEN
C                 iret = -14
C                 RETURN
C              END IF
C              CALL ST_CLST ( stinfo(:lstinfo), ';', ' ', 4,
C     +                       astnstr, iparts, iret)
C               stnindx = 1
C               dbstid = astnstr(1)
C               CALL ST_NUMB ( astnstr(2), dbstlt, ier)
C               CALL ST_NUMB ( astnstr(3), dbstln, ier)
C               CALL ST_NUMB ( astnstr(4), dbstel, ier)
c              CALL ST_ITOS (keyval, nkey, ncar, dbstn, ier)
c              CALL ST_LSTR ( dbstn, ldbstr, ier )
c              IF ( ldbstr .le. 3 ) THEN
c                  dbstid="K"//dbstn(:ldbstr)
c              ELSE 
c                  dbstid=dbstn(:ldbstr)
c              END IF

C           END IF
           RETURN
        END IF
C
C*	Check that the file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Find headers to search.
C
	IF  ( type .eq. 'ROW' )  THEN
	    istart = 1
	    istop  = klstrw ( iflno )
	  ELSE IF  ( type .eq. 'COL' )  THEN
	    istart = krow ( iflno ) + 1
	    istop  = istart + klstcl ( iflno ) - 1
	  ELSE
	    iret = -17
	    RETURN
	END IF
C
C*	Loop through all headers looking for match.
C
	done  = .false.
	i     = istart
	DO WHILE (( .not. done ) .and. ( i .le. istop ) )
	    done = .true.
	    j = 1
	    DO WHILE (  ( j .le. nkey ) .and. ( done ) ) 
		IF  ( kheadr ( keyloc (j), i, iflno ) .ne. keyval (j) ) 
     +						done = .false.
		j = j + 1
	    END DO
	    IF  ( done ) irwcl = i
	    i = i + 1
	END DO
C
C*	Correct location when using columns.
C
	IF  ( ( irwcl .ne. 0 ) .and. ( type .eq. 'COL' ) )
     +			irwcl = irwcl - krow ( iflno )
	IF (irwcl .eq. 0) iret = -17
C*
	RETURN
	END
