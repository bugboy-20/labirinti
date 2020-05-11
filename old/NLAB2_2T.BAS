DECLARE SUB apri ()
DECLARE SUB ruota ()
DECLARE SUB lread ()
DECLARE SUB presenta ()
DECLARE SUB errdefine ()
DECLARE SUB repaint ()
DECLARE SUB indati ()
DECLARE SUB delline ()
DECLARE SUB prepara ()
DECLARE SUB procedura ()
DECLARE SUB continit ()

COMMON SHARED lx
COMMON SHARED ly
COMMON SHARED ex
COMMON SHARED ey
COMMON SHARED ux
COMMON SHARED uy
COMMON SHARED w
COMMON SHARED z
COMMON SHARED x
COMMON SHARED y
COMMON SHARED prin
COMMON SHARED dimens
COMMON SHARED ta
COMMON SHARED cicli&
COMMON SHARED pst()
COMMON SHARED tabl()
COMMON SHARED l
COMMON SHARED time
COMMON SHARED tv
COMMON SHARED nomefile$
COMMON SHARED nomeinfo$
COMMON SHARED labstatus$
COMMON SHARED soprannome$
COMMON SHARED spes
COMMON SHARED blank
COMMON SHARED xln
COMMON SHARED yln
COMMON SHARED p
COMMON SHARED nfiles
COMMON SHARED numbfile






'ON ERROR GOTO errore
ON TIMER(1) GOSUB printer

'SHELL "del arc\labirint.map"
OPEN "arc\labirint.map" FOR RANDOM AS #3
'a = 0: PUT #3, 1, a: END

labstatus$ = ""


startprg:

prin = 3
dimens = 30
spes = 0

tv = 0
ta = 0
cicli& = 0
time = 0
blank = 0

SCREEN 0
presenta


SCREEN 12
COLOR 14

SELECT CASE p
CASE 1
        indati
        GOSUB dimscreen
       
        ERASE tabl: ERASE pst
        REDIM tabl(lx * ly): ta = 0: cicli& = 0
        REDIM pst(lx + 1, ly + 1)

        FOR a = 0 TO lx + 1
        pst(a, 0) = 1
        pst(a, ly + 1) = 1
        NEXT
        FOR a = 0 TO ly + 1
        pst(0, a) = 1
        pst(lx + 1, a) = 1
        NEXT
       
        prepara
        OPEN "O", 1, nomefile$: PRINT #1, lx; ly; ex; ey; ux; uy: CLOSE #1
        OPEN "O", 1, nomeinfo$: CLOSE #1
CASE 2
        apri
        GOTO startprg
CASE 3
        continit
        GOSUB dimscreen
        repaint
CASE 4
        lread
        GOTO startprg
CASE 5
        ruota
        END
CASE 6
        END
END SELECT



OPEN nomefile$ FOR APPEND AS #1

TIMER ON
time = TIMER - time
GOSUB printer

procedcall:

i$ = INKEY$
IF i$ = CHR$(32) THEN : blank = blank + 1: CLS : LOCATE 12, 26: PRINT "Non senti come pompa il PIPPERO ?"
IF i$ = CHR$(27) THEN
        TIMER OFF
        CLOSE #1
        OPEN "O", 1, nomeinfo$
        PRINT #1, ta; x; y; tv; INT(TIMER - time); cicli&

        FOR a = 1 TO lx * ly - 1
        PRINT #1, CHR$(tabl(a));
        NEXT
        PRINT #1,
        FOR b = 0 TO ly + 1
        FOR a = 0 TO lx + 1
        PRINT #1, CHR$(pst(a, b) + 1);
        NEXT
        PRINT #1, "  ";
        NEXT
        CLOSE #1
        GOTO startprg
END IF
IF blank >= 2 THEN blank = 0: repaint: GOSUB printer
IF blank = 0 THEN procedura

ta = ta + 1
pst(x, y) = 1: cicli& = cicli& + 1


IF x = ux AND y = uy AND ta < lx * ly - 1 THEN GOTO dietro

DESTRA:
IF pst(x + 1, y) = 1 THEN GOTO alto
x = x + 1: tabl(ta) = 1: l = 1: GOTO procedcall

alto:
IF pst(x, y - 1) = 1 THEN GOTO SINISTRA
y = y - 1: tabl(ta) = 2: l = 2: GOTO procedcall

SINISTRA:
IF pst(x - 1, y) = 1 THEN GOTO basso
x = x - 1: tabl(ta) = 3: l = 3: GOTO procedcall

basso:
IF pst(x, y + 1) = 1 THEN GOTO dietro
y = y + 1: tabl(ta) = 4: l = 4: GOTO procedcall

dietro:
ta = ta - 1
cicli& = cicli& + 1
IF ta + 1 = lx * ly AND x = ux AND y = uy THEN GOTO scrive
riprova:
pst(x, y) = 0
IF prin = 1 THEN LOCATE y + w, x + z: PRINT " ";

IF tabl(ta) = 1 THEN x = x - 1: l = 1: GOSUB dellinecall: GOTO alto
IF tabl(ta) = 2 THEN y = y + 1: l = 2: GOSUB dellinecall: GOTO SINISTRA
IF tabl(ta) = 3 THEN x = x + 1: l = 3: GOSUB dellinecall: GOTO basso
IF tabl(ta) = 4 THEN y = y - 1: l = 4: GOSUB dellinecall: GOTO dietro


LOCATE 29, 38: PRINT "FINE";
CLOSE #1
KILL nomeinfo$
labstatus$ = "F"
PUT #3, numbfile * 3 + 1, labstatus$

TIMER OFF
TI = TIMER
IF blank <> 0 THEN
        blank = 0
        i$ = INPUT$(1)
        repaint
END IF

LOCATE 1, 20: PRINT tv; "tavole in"; INT(TI - time); "secondi e"; cicli&; "cicli";

i$ = INPUT$(1): GOTO startprg






scrive:
tv = tv + 1: FOR ST = 1 TO lx * ly - 1: PRINT #1, CHR$(tabl(ST)); : NEXT: PRINT #1, "  "; : ' GOSUB printer
'PRINT CHR$(7);
'FOR a = 1 TO 1000 STEP .01: NEXT
GOTO riprova

dellinecall:
IF blank = 0 THEN delline
RETURN

printer:
IF blank = 0 THEN LOCATE 1, 20: PRINT tv; "tavole in"; INT(TIMER - time); "secondi e"; cicli&; "cicli";
RETURN

dimscreen:
IF dimens * lx > 600 OR dimens * ly > 440 THEN
        IF 600 \ lx < 440 \ ly THEN dimens = 600 \ lx ELSE dimens = 440 \ ly
END IF

w = INT((640 - (lx + 1) * dimens) / 2): z = INT((480 - (ly + 1) * dimens) / 2)
RETURN

errore:
errdefine
RESUME

SUB apri

rt:

'LOCATE 3, 12: PRINT "ษออออออออออออออออออออออออออออออออออออออออออออออออออออออป"
'FOR a = 1 TO 8
'LOCATE a + 3, 12: PRINT "บ                                                      บ"
'NEXT
'LOCATE 12, 12: PRINT "ศออออออออออออออออออออออออออออออออออออออออออออออออออออออผ"

LINE (640 / 80 * 12 - 4, 480 / 30 * 3 - 10)-(640 / 80 * 68 - 12, 480 / 30 * 12 - 10), 13, B

LINE (10, 210)-(630, 470), 13, B

GET #3, 1, nfiles
IF nfiles = 0 THEN i$ = INPUT$(1): GOTO en3
numbfile = 1
p3 = 1
IF nfiles > 8 THEN k = 8 ELSE k = nfiles

GOSUB scr3
COLOR 13
GOSUB scr2

GOSUB disegn

l2:

COLOR 15

DO
i$ = INKEY$
LOOP WHILE i$ = ""


i = ASC(RIGHT$(i$, 1))
SELECT CASE i
        CASE 72
                IF numbfile <> 1 THEN GOSUB scr2: numbfile = numbfile - 1: IF p3 = 1 THEN GOSUB scr3 ELSE p3 = p3 - 1 ELSE GOTO l2
        CASE 80
                IF numbfile <> nfiles THEN GOSUB scr2:  numbfile = numbfile + 1: IF p3 = 8 THEN GOSUB scr3 ELSE p3 = p3 + 1 ELSE GOTO l2
        CASE 13
                GET #3, numbfile * 3 - 1, soprannome$
                GET #3, numbfile * 3, nome$
                GET #3, numbfile * 3 + 1, labstatus$
               
                GOTO en3
        CASE ELSE
                GOTO l2
END SELECT
COLOR 13
GOSUB scr2

GOSUB disegn
GOTO l2





scr2:
GET #3, numbfile * 3 - 1, soprannome$
LOCATE p3 + 3, 15: PRINT soprannome$
RETURN


scr3:
COLOR 15

FOR i = 1 TO k

GET #3, (i + (numbfile - p3)) * 3 - 1, i$
LOCATE i + 3, 15: PRINT SPACE$(50)
LOCATE i + 3, 15: PRINT i$
NEXT
RETURN

disegn:
LINE (10 + 1, 210 + 1)-(630 - 1, 470 - 1), 0, BF

GET #3, numbfile * 3, nomefile$

nomeinfo$ = "arc\" + nomefile$ + ".nfo"
nomefile$ = "arc\" + nomefile$ + ".lbr"

OPEN "i", 4, nomefile$
INPUT #4, lx, ly, ex, ey, ux, uy

d = dimens
IF d * lx > (620 - 20) OR d * ly > (460 - 220) THEN
        IF (620 - 20) \ lx < (460 - 220) \ ly THEN d = (620 - 20) \ lx ELSE d = (460 - 220) \ ly
END IF
w = INT(((620 + 20) - (lx + 1) * d) / 2): z = INT(((460 + 220) - (ly + 1) * d) / 2)


FOR a = 0 TO lx
LINE (a * d + w + (d / 2) - spes, z + (d / 2) - spes)-(a * d + w + (d / 2) + spes, z + ly * d + (d / 2) + spes), 14, BF
NEXT
FOR a = 0 TO ly
LINE (w + (d / 2) + spes, a * d + z + (d / 2) + spes)-(w + lx * d + (d / 2) - spes, a * d + z + (d / 2) - spes), 14, BF
NEXT

LINE (ex * d + w + (d / 2) - spes - 2, ey * d + z + (d / 2) - spes - 2)-(ex * d + w - (d / 2) + spes + 2, ey * d + z - (d / 2) + spes + 2), 10, BF
LINE (ux * d + w + (d / 2) - spes - 2, uy * d + z + (d / 2) - spes - 2)-(ux * d + w - (d / 2) + spes + 2, uy * d + z - (d / 2) + spes + 2), 12, BF
CLOSE #4
RETURN


en3:

END SUB

SUB continit
OPEN "i", 1, nomefile$
INPUT #1, lx, ly, ex, ey, ux, uy
CLOSE #1
OPEN "i", 1, nomeinfo$
INPUT #1, ta, x, y, tv, time, cicli&

ERASE tabl: ERASE pst
REDIM tabl(lx * ly)
REDIM pst(lx + 1, ly + 1)

FOR a = 1 TO lx * ly - 1
i$ = INPUT$(1, #1)
c = ASC(i$)
tabl(a) = c
NEXT

i$ = INPUT$(2, #1)

FOR b = 0 TO ly + 1
FOR a = 0 TO lx + 1
i$ = INPUT$(1, #1)
c = ASC(i$)
pst(a, b) = c - 1
'LOCATE b + 1, a * 4 + 1: PRINT pst(a, b);
NEXT
i$ = INPUT$(2, #1)
NEXT


CLOSE #1

END SUB

SUB delline
'i$ = INPUT$(1)
SELECT CASE prin
CASE 2
drx1 = xln: dry1 = yln: drx2 = x * dimens + w: dry2 = y * dimens + z
IF drx1 > drx2 THEN SWAP drx1, drx2
IF dry1 > dry2 THEN SWAP dry1, dry2
LINE (drx1 - spes, dry1 - spes)-(drx2 + spes, dry2 + spes), 0, BF
xln = x * dimens + w: yln = y * dimens + z

CASE 3

SELECT CASE l
CASE 1
LINE ((x - 1 + 1) * dimens + w + (dimens / 2) - spes, y * dimens + z + (dimens / 2 - 1) - spes)-((x - 1 + 1) * dimens + w + (dimens / 2) + spes, y * dimens + z - (dimens / 2 - 1) + spes), , BF
CASE 2
LINE (x * dimens + w + (dimens / 2 - 1) - spes, (y - 1) * dimens + z + (dimens / 2) - spes)-(x * dimens + w - (dimens / 2 - 1) + spes, (y - 1) * dimens + z + (dimens / 2) + spes), , BF
CASE 3
LINE ((x + 1 - 1) * dimens + w - (dimens / 2) - spes, y * dimens + z + (dimens / 2 - 1) - spes)-((x + 1 - 1) * dimens + w - (dimens / 2) + spes, y * dimens + z - (dimens / 2 - 1) + spes), , BF
CASE 4
LINE (x * dimens + w + (dimens / 2 - 1) - spes, (y + 1) * dimens + z - (dimens / 2) - spes)-(x * dimens + w - (dimens / 2 - 1) + spes, (y + 1) * dimens + z - (dimens / 2) + spes), , BF

END SELECT
END SELECT

END SUB

SUB errdefine
PRINT "errore!!"
END
END SUB

SUB indati

CLS
LOCATE 5, 16: PRINT "Inserisci il nome che vuoi assegnare al labirinto"

innome:
LOCATE 7, 13: PRINT "["; : LOCATE 7, 67: PRINT "]";
LOCATE 7, 15: INPUT "", soprannome$

nomefile$ = STR$(TIMER * 100)
nomefile$ = STRING$(6, CHR$(169)) + LTRIM$(nomefile$)
nomefile$ = RIGHT$(nomefile$, 8)


labstatus$ = "N"
GET #3, 1, nfiles: LOCATE 4, 5
nfiles = nfiles + 1
PUT #3, 1, nfiles
PUT #3, nfiles * 3 - 1, soprannome$
PUT #3, nfiles * 3, nomefile$
PUT #3, nfiles * 3 + 1, labstatus$

nomeinfo$ = "arc\" + nomefile$ + ".nfo"
nomefile$ = "arc\" + nomefile$ + ".lbr"


CLS

1
LOCATE 5, 35: PRINT SPACE$(44);
LOCATE 5, 35: INPUT "lunghezza x ", lx
IF lx <> INT(lx) OR lx <= 0 THEN 1

2
LOCATE 6, 35: PRINT SPACE$(44);
LOCATE 6, 35: INPUT "lunghezza y ", ly
IF ly <> INT(ly) OR ly <= 0 THEN 2

3
LOCATE 7, 35: PRINT SPACE$(44);
LOCATE 7, 35: INPUT "entrata (x) ", ex: x = ex
IF ex <> INT(ex) OR ex <= 0 OR ex > lx THEN 3

4
LOCATE 8, 35: PRINT SPACE$(44);
LOCATE 8, 35: INPUT "entrata (y) ", ey: y = ey
IF ey <> INT(ey) OR ey <= 0 OR ey > ly THEN 4

5
LOCATE 9, 35: PRINT SPACE$(44);
LOCATE 9, 35: INPUT "uscita (x)  ", ux
IF ux <> INT(ux) OR ux <= 0 OR ux > lx THEN 5

6
LOCATE 10, 35: PRINT SPACE$(44);
LOCATE 10, 35: INPUT "uscita (y)  ", uy
IF uy <> INT(uy) OR uy <= 0 OR uy > ly OR (ux = ex AND uy = ey) THEN 6

END SUB

SUB lread
prin = 3
disx = 0: disy = 0

CLS
dimens = 8
spax = 2
spay = 2
spes = 0

time = TIMER

OPEN "i", 1, nomefile$: INPUT #1, lx, ly, ex, ey, ux, uy

cambio:
IF INKEY$ <> "" THEN GOTO en2
w = dimens * (lx + spax) * disx
z = dimens * (ly + spay) * disy
x = ex: y = ey
IF EOF(1) THEN GOTO en2

'prepara
SELECT CASE prin

CASE 2
PSET (ex * dimens + w, ey * dimens + z)

CASE 3
FOR n = 0 TO lx
LINE (n * dimens + w + (dimens / 2) - spes, z + (dimens / 2) - spes)-(n * dimens + w + (dimens / 2) + spes, z + ly * dimens + (dimens / 2) + spes), 14, BF
NEXT
FOR n = 0 TO ly
LINE (w + (dimens / 2) + spes, n * dimens + z + (dimens / 2) + spes)-(w + lx * dimens + (dimens / 2) - spes, n * dimens + z + (dimens / 2) - spes), 14, BF
NEXT

LINE (ex * dimens + w + (dimens / 2) - spes - 2, ey * dimens + z + (dimens / 2) - spes - 2)-(ex * dimens + w - (dimens / 2) + spes + 2, ey * dimens + z - (dimens / 2) + spes + 2), 10, BF
LINE (ux * dimens + w + (dimens / 2) - spes - 2, uy * dimens + z + (dimens / 2) - spes - 2)-(ux * dimens + w - (dimens / 2) + spes + 2, uy * dimens + z - (dimens / 2) + spes + 2), 12, BF

END SELECT



FOR a = 1 TO lx * ly - 1

i$ = INPUT$(1, #1)
l = ASC(i$)

SELECT CASE l
CASE 1
x = x + 1
CASE 2
y = y - 1
CASE 3
x = x - 1
CASE 4
y = y + 1
END SELECT


'procedura

SELECT CASE prin

CASE 2
LINE -(x * dimens + w, y * dimens + z)
CASE 3
SELECT CASE l

CASE 1
LINE ((x - 1) * dimens + w + (dimens / 2) - spes, y * dimens + z + (dimens / 2 - 1) - spes)-((x - 1) * dimens + w + (dimens / 2) + spes, y * dimens + z - (dimens / 2 - 1) + spes), 0, BF
CASE 2
LINE (x * dimens + w + (dimens / 2 - 1) - spes, y * dimens + z + (dimens / 2) - spes)-(x * dimens + w - (dimens / 2 - 1) + spes, y * dimens + z + (dimens / 2) + spes), 0, BF
CASE 3
LINE ((x + 1) * dimens + w - (dimens / 2) - spes, y * dimens + z + (dimens / 2 - 1) - spes)-((x + 1) * dimens + w - (dimens / 2) + spes, y * dimens + z - (dimens / 2 - 1) + spes), 0, BF
CASE 4
LINE (x * dimens + w + (dimens / 2 - 1) - spes, y * dimens + z - (dimens / 2) - spes)-(x * dimens + w - (dimens / 2 - 1) + spes, y * dimens + z - (dimens / 2) + spes), 0, BF

END SELECT
END SELECT

NEXT
i$ = INPUT$(2, #1)

disx = disx + 1
IF disx >= INT((639 + (spax * dimens)) / ((lx + spax) * dimens)) THEN disx = 0: disy = disy + 1
IF disy >= INT((479 + (spay * dimens)) / ((ly + spay) * dimens)) THEN
        i$ = INPUT$(1)
        CLS
        disx = 0: disy = 0
END IF
GOTO cambio

en2:
CLOSE #1
i$ = INPUT$(1)
END SUB

SUB prepara
CLS

SELECT CASE prin

CASE 2
xln = ex * dimens + w: yln = ey * dimens + z

CASE 3
FOR a = 0 TO lx
LINE (a * dimens + w + (dimens / 2) - spes, z + (dimens / 2) - spes)-(a * dimens + w + (dimens / 2) + spes, z + ly * dimens + (dimens / 2) + spes), 14, BF
NEXT

FOR a = 0 TO ly
LINE (w + (dimens / 2) + spes, a * dimens + z + (dimens / 2) + spes)-(w + lx * dimens + (dimens / 2) - spes, a * dimens + z + (dimens / 2) - spes), 14, BF
NEXT

LINE (ex * dimens + w + (dimens / 2) - spes - 2, ey * dimens + z + (dimens / 2) - spes - 2)-(ex * dimens + w - (dimens / 2) + spes + 2, ey * dimens + z - (dimens / 2) + spes + 2), 10, BF
LINE (ux * dimens + w + (dimens / 2) - spes - 2, uy * dimens + z + (dimens / 2) - spes - 2)-(ux * dimens + w - (dimens / 2) + spes + 2, uy * dimens + z - (dimens / 2) + spes + 2), 12, BF

END SELECT

LOCATE 30, 20: PRINT "Premere -SPAZIO- per oscurare lo schermo";

END SUB

SUB presenta

CLS
LOCATE 1, 33: PRINT "Labirint Maker"
LOCATE 2, (80 - LEN(soprannome$)) \ 2: PRINT soprannome$;

COLOR 14, 0: LOCATE 4, 31: PRINT "N"; : COLOR 11: PRINT "uovo labirinto"
COLOR 14, 0: LOCATE 6, 31: PRINT "A"; : COLOR 11: PRINT "pri archivio"

IF labstatus$ = "F" OR labstatus$ = "" THEN COLOR 7, 0: LOCATE 8, 31: PRINT "Continua labirinto" ELSE COLOR 14, 0: LOCATE 8, 31: PRINT "C"; : COLOR 11: PRINT "ontinua labirinto"
IF labstatus$ = "" THEN COLOR 7, 0: LOCATE 10, 31: PRINT "Vista" ELSE COLOR 14, 0: LOCATE 10, 31: PRINT "V"; : COLOR 11: PRINT "ista"
IF labstatus$ = "N" OR labstatus$ = "" THEN COLOR 7, 0: LOCATE 12, 31: PRINT "Ruota" ELSE COLOR 14, 0: LOCATE 12, 31: PRINT "R"; : COLOR 11: PRINT "uota"

COLOR 14, 0: LOCATE 14, 31: PRINT "U"; : COLOR 11: PRINT "scita"

p = 1

l:
COLOR 9, 15

GOSUB scr

DO
i$ = INKEY$
LOOP WHILE i$ = ""

COLOR 14, 0

SELECT CASE UCASE$(i$)
        CASE "N"
                GOSUB scr
                p = 1
                GOTO en
        CASE "A"
                GOSUB scr
                p = 2
                GOTO en
        CASE "C"
                IF labstatus$ = "F" OR labstatus$ = "" THEN GOTO l
                GOSUB scr
                p = 3
                GOTO en
        CASE "V"
                IF labstatus$ = "" THEN GOTO l
                GOSUB scr
                p = 4
                GOTO en
        CASE "R"
                'IF labstatus$ = "N" OR labstatus$ = "" THEN GOTO l
                GOSUB scr
                p = 5
                GOTO en
        CASE "U"
                GOSUB scr
                p = 6
                GOTO en
        CASE ELSE
                i = ASC(RIGHT$(i$, 1))
                SELECT CASE i
                        CASE 72
                                GOSUB scr
                                IF p = 1 THEN p = 6 ELSE p = p - 1
                                IF labstatus$ = "F" THEN IF p = 3 THEN p = p - 1
                                IF labstatus$ = "N" THEN IF p = 5 THEN p = p - 1
                                IF labstatus$ = "" THEN IF p = 5 THEN p = p - 3
                        CASE 80
                                GOSUB scr
                                IF p = 6 THEN p = 1 ELSE p = p + 1
                                IF labstatus$ = "F" THEN IF p = 3 THEN p = p + 1
                                IF labstatus$ = "N" THEN IF p = 5 THEN p = p + 1
                                IF labstatus$ = "" THEN IF p = 3 THEN p = p + 3
                        CASE 13
                                GOTO en
                END SELECT
END SELECT
GOTO l

scr:
SELECT CASE p
        CASE 1
        LOCATE 4, 31: PRINT "N"; : COLOR 11: PRINT "uovo labirinto"
        CASE 2
        LOCATE 6, 31: PRINT "A"; : COLOR 11: PRINT "pri archivio"
        CASE 3
        LOCATE 8, 31: PRINT "C"; : COLOR 11: PRINT "ontinua labirinto"
        CASE 4
        LOCATE 10, 31: PRINT "V"; : COLOR 11: PRINT "ista"
        CASE 5
        LOCATE 12, 31: PRINT "R"; : COLOR 11: PRINT "uota"
        CASE 6
        LOCATE 14, 31: PRINT "U"; : COLOR 11: PRINT "scita"
END SELECT
RETURN
en:

END SUB

SUB procedura
'i$ = INPUT$(1)
SELECT CASE prin

CASE 2
drx1 = xln: dry1 = yln: drx2 = x * dimens + w: dry2 = y * dimens + z
IF drx1 > drx2 THEN SWAP drx1, drx2
IF dry1 > dry2 THEN SWAP dry1, dry2
LINE (drx1 - spes, dry1 - spes)-(drx2 + spes, dry2 + spes), , BF
xln = x * dimens + w: yln = y * dimens + z

CASE 3
SELECT CASE l
CASE 1
LINE ((x - 1) * dimens + w + (dimens / 2) - spes, y * dimens + z + (dimens / 2 - 1) - spes)-((x - 1) * dimens + w + (dimens / 2) + spes, y * dimens + z - (dimens / 2 - 1) + spes), 0, BF
CASE 2
LINE (x * dimens + w + (dimens / 2 - 1) - spes, y * dimens + z + (dimens / 2) - spes)-(x * dimens + w - (dimens / 2 - 1) + spes, y * dimens + z + (dimens / 2) + spes), 0, BF
CASE 3
LINE ((x + 1) * dimens + w - (dimens / 2) - spes, y * dimens + z + (dimens / 2 - 1) - spes)-((x + 1) * dimens + w - (dimens / 2) + spes, y * dimens + z - (dimens / 2 - 1) + spes), 0, BF
CASE 4
LINE (x * dimens + w + (dimens / 2 - 1) - spes, y * dimens + z - (dimens / 2) - spes)-(x * dimens + w - (dimens / 2 - 1) + spes, y * dimens + z - (dimens / 2) + spes), 0, BF

END SELECT
END SELECT

END SUB

SUB repaint
CLS

'prepara

SELECT CASE prin

CASE 2
xln = ex * dimens + w: yln = ey * dimens + z

CASE 3
FOR a = 0 TO lx
LINE (a * dimens + w + (dimens / 2) - spes, z + (dimens / 2) - spes)-(a * dimens + w + (dimens / 2) + spes, z + ly * dimens + (dimens / 2) + spes), 14, BF
NEXT
FOR a = 0 TO ly
LINE (w + (dimens / 2) + spes, a * dimens + z + (dimens / 2) + spes)-(w + lx * dimens + (dimens / 2) - spes, a * dimens + z + (dimens / 2) - spes), 14, BF
NEXT

LINE (ex * dimens + w + (dimens / 2) - spes - 2, ey * dimens + z + (dimens / 2) - spes - 2)-(ex * dimens + w - (dimens / 2) + spes + 2, ey * dimens + z - (dimens / 2) + spes + 2), 10, BF
LINE (ux * dimens + w + (dimens / 2) - spes - 2, uy * dimens + z + (dimens / 2) - spes - 2)-(ux * dimens + w - (dimens / 2) + spes + 2, uy * dimens + z - (dimens / 2) + spes + 2), 12, BF

END SELECT

xt = ex: yt = ey

FOR a = 1 TO ta

lt = tabl(a)

SELECT CASE lt
CASE 1
xt = xt + 1
CASE 2
yt = yt - 1
CASE 3
xt = xt - 1
CASE 4
yt = yt + 1
END SELECT

'equivalente di procedura

SELECT CASE prin

CASE 2
drx1 = xln: dry1 = yln: drx2 = xt * dimens + w: dry2 = yt * dimens + z
IF drx1 > drx2 THEN SWAP drx1, drx2
IF dry1 > dry2 THEN SWAP dry1, dry2
LINE (drx1 - spes, dry1 - spes)-(drx2 + spes, dry2 + spes), , BF
xln = xt * dimens + w: yln = yt * dimens + z

CASE 3
SELECT CASE lt

CASE 1
LINE ((xt - 1) * dimens + w + (dimens / 2) - spes, yt * dimens + z + (dimens / 2 - 1) - spes)-((xt - 1) * dimens + w + (dimens / 2) + spes, yt * dimens + z - (dimens / 2 - 1) + spes), 0, BF
CASE 2
LINE (xt * dimens + w + (dimens / 2 - 1) - spes, yt * dimens + z + (dimens / 2) - spes)-(xt * dimens + w - (dimens / 2 - 1) + spes, yt * dimens + z + (dimens / 2) + spes), 0, BF
CASE 3
LINE ((xt + 1) * dimens + w - (dimens / 2) - spes, yt * dimens + z + (dimens / 2 - 1) - spes)-((xt + 1) * dimens + w - (dimens / 2) + spes, yt * dimens + z - (dimens / 2 - 1) + spes), 0, BF
CASE 4
LINE (xt * dimens + w + (dimens / 2 - 1) - spes, yt * dimens + z - (dimens / 2) - spes)-(xt * dimens + w - (dimens / 2 - 1) + spes, yt * dimens + z - (dimens / 2) + spes), 0, BF

END SELECT
END SELECT

NEXT

LOCATE 30, 20: PRINT "Premere -SPAZIO- per oscurare lo schermo";

END SUB

SUB ruota
giro = 2
specul = 1

nomert$ = "ruota"
nomert$ = "arc\" + nomert$ + ".lbr"

OPEN "i", 1, nomefile$
OPEN "o", 2, "arc\labrt1.tmp"

INPUT #1, lx, ly, ex, ey, ux, uy
PRINT lx; ly; ex; ey; ux; uy

PRINT giro
GOSUB makegiro

PRINT specul
GOSUB makespecul

PRINT lx; ly; ex; ey; ux; uy
'PRINT #2, lx, ly, ex, ey, ux, uy



i$ = INPUT$(1)
nlab = 0
viaz:
    
IF INKEY$ <> "" THEN END
IF EOF(1) THEN GOTO ruotariordin

FOR a = 1 TO lx * ly - 1
i$ = INPUT$(1, #1)
lt = ASC(i$)

'giro
lt = lt + giro
IF lt > 4 THEN lt = lt - 4

'specul
SELECT CASE specul
        CASE 1
        IF lt = 1 THEN lt = 3 ELSE IF lt = 3 THEN lt = 1
        CASE 2
        IF lt = 2 THEN lt = 4 ELSE IF lt = 4 THEN lt = 2
END SELECT

'PRINT CHR$(lt);
PRINT #2, CHR$(lt);

NEXT a
PRINT #2, "  "; : ' PRINT

i$ = INPUT$(2, #1)

nlab = nlab + 1
GOTO viaz




makegiro:
IF giro = 1 OR giro = 3 THEN
        SWAP lx, ly
        SWAP ex, ey
        SWAP ux, uy
       
        IF giro = 1 THEN
                ey = ly - ey + 1
                uy = ly - uy + 1
        ELSE
                ex = lx - ex + 1
                ux = lx - ux + 1
        END IF
END IF

IF giro = 2 THEN
        ex = lx - ex + 1
        ux = lx - ux + 1
        ey = ly - ey + 1
        uy = ly - uy + 1
END IF
RETURN

makespecul:
IF specul = 1 THEN
        ex = lx - ex + 1
        ux = lx - ux + 1
END IF
IF specul = 2 THEN
        ey = ly - ey + 1
        uy = ly - uy + 1
END IF
RETURN

ruotariordin:
CLOSE #2
PRINT nlab: i$ = INPUT$(1)

REDIM copia(nlab)
REDIM ordin(nlab, 2)
                       
ordw = 1

FOR a = 1 TO nlab
ordin(a, 1) = a
NEXT a


FOR b = lx * ly - 2 TO 1 STEP -1

LOCATE 30, 8: PRINT b;

seq = 1
OPEN "i", 2, "arc\labrt1.tmp"
i$ = INPUT$(b - 0, #2)
i$ = INPUT$(1, #2)
copia(1) = ASC(i$)

FOR a = 2 TO nlab
i$ = INPUT$(lx * ly - 1 + 1, #2)
i$ = INPUT$(1, #2)
copia(a) = ASC(i$)
NEXT a
CLOSE #2

FOR a = 1 TO 4
FOR c = 1 TO nlab
'PRINT ordin(c, ordw); copia(ordin(c, ordw)): i$ = INPUT$(1)
IF copia(ordin(c, ordw)) = a THEN
        ordin(seq, 2 / ordw) = ordin(c, ordw)
        seq = seq + 1
END IF
NEXT c
NEXT a

ordw = 2 / ordw
NEXT b

mostra:
PRINT : PRINT "caz"
'FOR a = 1 TO nlab
'PRINT ordin(a, ordw)
'NEXT

FOR b = 1 TO nlab

OPEN "i", 2, "arc\labrt1.tmp"
FOR a = 1 TO ordin(b, ordw)
i$ = INPUT$((lx * ly - 1 + 2), #2)
NEXT a

PRINT i$
CLOSE #2
NEXT b


END SUB

