FC = gfortran
FFLAGS = -O2 -march=native -fPIC
CC = gcc
CFLAGS = -DUSE_JPEG2000 -I/home/decker/classes/met212/gempak/jasper-1.900.1/src/libjasper/include -march=native -fPIC
LDFLAGS = -L/home/decker/classes/met212/gempak/jasper-1.900.1/src/libjasper/.libs -ljasper

.SUFFIXES: .o .f90 .f .c

OBJECTS = stlcuc.o \
	stabbr.o \
	stctoi.o \
	lvcord.o \
	inbdta.o \
	gdinit.o \
	stlstr.o \
	strmbl.o \
	dmgetf.o \
	flglun.o \
	ssgsym.o \
	ssenvr.o \
	stuclc.o \
	flinqr.o \
	flflun.o \
	fldopn.o \
	fldsop.o \
	stitos.o \
	dmrstr.o \
	stldsp.o \
	strxbl.o \
	stcls2.o \
	dbinfo.o \
	dmword.o \
	flwrit.o \
	dmnxtc.o \
	flrshr.o \
	flread.o \
	dmrrec.o \
	dmrint.o \
	mvswp4.o \
	dmrflt.o \
	dmrlbl.o \
	stclst.o \
	dmrdmg.o \
	stitoc.o \
	dmrch4.o \
	dmrkey.o \
	dmchkf.o \
	dmfkey.o \
	dmrhda.o \
	dpsetp.o \
	dmrprt.o \
	dmrfil.o \
	dmopen.o \
	strmst.o \
	flpath.o \
	fltinq.o \
	flsopn.o \
	flbksp.o \
	fltdat.o \
	fltbop.o \
	stalnm.o \
	strnan.o \
	flclos.o \
	dpfile.o \
	stnumb.o \
	stcrnd.o \
	stcrnm.o \
	strlst.o \
	dpterm.o \
	inprmf.o \
	dmpart.o \
	dmkeys.o \
	dmrfhr.o \
	dmbegs.o \
	stnull.o \
	dmcond.o \
	dmnext.o \
	stintg.o \
	tgiftm.o \
	tidaym.o \
	tiyy24.o \
	tictoi.o \
	tiidtm.o \
	tisubm.o \
	tisubd.o \
	tgvtof.o \
	tgctoi.o \
	tgitof.o \
	ststoi.o \
	dmrclh.o \
	tgftoi.o \
	tiyymd.o \
	tgyymd.o \
	gdaddt.o \
	gdsrtt.o \
	gdofil.o \
	gdfchk.o \
	dmfwrt.o \
	dpendp.o \
	dmclos.o \
	gdclos.o \
	grrnav.o \
	gditoh.o \
	dmsrch.o \
	dpugrb.o \
	mvsw42.o \
	dpunmc.o \
	dpudif.o \
	dmrpkgc.o \
	gbits.o \
	g2_unpack1.o \
	g2_unpack6.o \
	drstemplates.o \
	g2_unpack5.o \
	pdstemplates.o \
	int_power.o \
	g2_rdieee.o \
	g2_unpack4.o \
	gridtemplates.o \
	g2_unpack3.o \
	g2_unpack2.o \
	simunpack.o \
	comunpack.o \
	specunpack.o \
	dec_jpeg2000.o \
	jpcunpack.o \
	g2_unpack7.o \
	gb2qlin.o \
	gb2ornt.o \
	gb2gmis.o \
	gb2ugem.o \
	dpugb2.o \
	dmrpkgc2.o \
	dmrpkg.o \
	dpunpk.o \
	dmunpk.o \
	dmrdtr.o \
	gdrdat.o \
	fldcre.o \
	dmwstr.o \
	dmwint.o \
	mvev32.o \
	dmwflt.o \
	dmwlbl.o \
	dmwdmg.o \
	dmwch4.o \
	dmwkey.o \
	dmwfil.o \
	dmcnst.o \
	dmwprt.o \
	dmwhda.o \
	dmcret.o \
	dmwrwh.o \
	dmwfhr.o \
	gdcref.o \
	dmsgpk.o \
	dmefre.o \
	dmafre.o \
	dmclop.o \
	dmgspc.o \
	dmwpkg.o \
	dppgrb.o \
	dppdec.o \
	dppdif.o \
	dmpkgd.o \
	dmwdtrc.o \
	dppack.o \
	dmpack.o \
	dmwdtr.o \
	dmwclh.o \
	gdwpgd.o \
	gdwdat.o \
	tiitoc.o \
	ticdtm.o \
	stinch.o \
	tgcftm.o \
	tgitoc.o \
	gdngrd.o \
	gdhtoi.o \
	dmpsrc.o \
	gdsrtl.o \
	gdgidn.o \
	lvccrd.o \
	gemread.o

test: $(OBJECTS)
	$(FC) -o $@ $(FFLAGS) $(OBJECTS) $(LDFLAGS)
	rm -f gemread.o
	f2py -c -m gempakf gemread.f90 *.o -L/home/decker/classes/met212/gempak/jasper-1.900.1/src/libjasper/.libs -ljasper

.f90.o:
	$(FC) -c $(FFLAGS) $<

.f.o:
	$(FC) -c $(FFLAGS) $<

.c.o:
	$(CC) -c $(CFLAGS) $<

clean:
	rm -f *.o *.mod

distclean:
	rm -f *.o *.mod gempakf.so test
