DECLARE SUB tronca ()
DECLARE SUB indati ()
DECLARE SUB delline ()
DECLARE SUB prepara ()
DECLARE SUB procedura ()
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
COMMON SHARED cicli
COMMON SHARED pst()
COMMON SHARED l

ON ERROR GOTO errore
ON TIMER(10) GOSUB printer

prin = 3
dimens = 20
nomefile$ = "arcfile.txt"
nomefile$ = "arc\" + nomefile$

SCREEN 12
COLOR 14
indati
CLS
prepara

DIM TABL(lx * ly): ta = 0: cicli = 0
DIM pst(lx + 1, ly + 1)

FOR a = 0 TO lx + 1
pst(a, 0) = 1
pst(a, ly + 1) = 1
NEXT
FOR a = 0 TO ly + 1
pst(0, a) = 1
pst(lx + 1, a) = 1
NEXT

TIMER ON
time = TIMER

OPEN "O", 1, nomefile$: PRINT #1, lx, ly, ex, ey, ux, uy: CLOSE
OPEN nomefile$ FOR APPEND AS #1

procedcall:
IF INKEY$ <> "" THEN tronca
procedura
IF x = ux AND y = uy AND ta < lx * ly - 1 THEN GOTO dietro

destra:
IF pst(x + 1, y) = 1 THEN GOTO alto
x = x + 1: TABL(ta) = 1: l = 1: GOTO procedcall

alto:
IF pst(x, y - 1) = 1 THEN GOTO sinistra
y = y - 1: TABL(ta) = 2: l = 2: GOTO procedcall

sinistra:
IF pst(x - 1, y) = 1 THEN GOTO basso
x = x - 1: TABL(ta) = 3: l = 3: GOTO procedcall

basso:
IF pst(x, y + 1) = 1 THEN GOTO dietro
y = y + 1: TABL(ta) = 4: l = 4: GOTO procedcall

dietro:
ta = ta - 1
IF ta + 1 = lx * ly AND x = ux AND y = uy THEN GOTO scrive
riprova:
pst(x, y) = 0
IF prin = 1 THEN LOCATE y + w, x + z: PRINT " ";

IF TABL(ta) = 1 THEN x = x - 1: l = 1: GOSUB dellinecall: GOTO alto
IF TABL(ta) = 2 THEN y = y + 1: l = 2: GOSUB dellinecall: GOTO sinistra
IF TABL(ta) = 3 THEN x = x + 1: l = 3: GOSUB dellinecall: GOTO basso
IF TABL(ta) = 4 THEN y = y - 1: l = 4: GOSUB dellinecall: GOTO dietro
LOCATE 30, 50: PRINT "FINE"; : i$ = INPUT$(1): END

scrive:
FOR t = 1 TO 100000: NEXT
TV = TV + 1: FOR ST = 1 TO lx * ly - 1: PRINT #1, TABL(ST); : NEXT: PRINT #1, : GOSUB printer: GOTO riprova

errore:
PRINT "ERRORE!!!": END

dellinecall:
delline
RETURN

printer:
LOCATE 1, 30: PRINT TV; "tavole", INT(TIMER - time); "sec.", cicli; "cicli";
RETURN

SUB delline
'i$ = INPUT$(1)
SELECT CASE prin
CASE 2
LINE -(x * dimens + w, y * dimens + z), 0
CASE 3
SELECT CASE l
CASE 1
LINE ((x - 1 + 1) * dimens + w + (dimens / 2), y * dimens + z + (dimens / 2 - 1))-((x - 1 + 1) * dimens + w + (dimens / 2), y * dimens + z - (dimens / 2 - 1))
CASE 2
LINE (x * dimens + w + (dimens / 2 - 1), (y - 1) * dimens + z + (dimens / 2))-(x * dimens + w - (dimens / 2 - 1), (y - 1) * dimens + z + (dimens / 2))
CASE 3
LINE ((x + 1 - 1) * dimens + w - (dimens / 2), y * dimens + z + (dimens / 2 - 1))-((x + 1 - 1) * dimens + w - (dimens / 2), y * dimens + z - (dimens / 2 - 1))
CASE 4
LINE (x * dimens + w + (dimens / 2 - 1), (y + 1) * dimens + z - (dimens / 2))-(x * dimens + w - (dimens / 2 - 1), (y + 1) * dimens + z - (dimens / 2))
END SELECT
END SELECT

END SUB

SUB indati
LOCATE 5, 35: INPUT "lunghezza x ", lx
LOCATE 6, 35: INPUT "lunghezza y ", ly
LOCATE 7, 35: INPUT "entrata (x) ", ex: x = ex
LOCATE 8, 35: INPUT "entrata (y) ", ey: y = ey
LOCATE 9, 35: INPUT "uscita (x)  ", ux
LOCATE 10, 35: INPUT "uscita (y)  ", uy
END SUB

SUB prepara
SELECT CASE prin
CASE 1
z = INT((80 - lx) / 2): w = INT((30 - ly) / 2) + 1
LOCATE w + 1, z + 1: PRINT CHR$(218);
LOCATE ly + w, lx + z: PRINT CHR$(217);
CASE 2
w = INT((640 - lx * dimens) / 2): z = INT((480 - ly * dimens) / 2)
PSET (ex * 10 + w, ey * 10 + z)
CASE 3
w = INT((640 - (lx + 1) * dimens) / 2): z = INT((480 - (ly + 1) * dimens) / 2)
FOR n = 0 TO lx
LINE (n * dimens + w + (dimens / 2), z + (dimens / 2))-(n * dimens + w + (dimens / 2), z + ly * dimens + (dimens / 2))
NEXT
FOR n = 0 TO ly
LINE (w + (dimens / 2), n * dimens + z + (dimens / 2))-(w + lx * dimens + (dimens / 2), n * dimens + z + (dimens / 2))
NEXT
END SELECT
END SUB

SUB procedura
'i$ = INPUT$(1)
SELECT CASE prin
CASE 1
LOCATE y + w, x + z: PRINT CHR$(2);
CASE 2
LINE -(x * dimens + w, y * dimens + z)
CASE 3
SELECT CASE l
CASE 1
LINE ((x - 1) * dimens + w + (dimens / 2), y * dimens + z + (dimens / 2 - 1))-((x - 1) * dimens + w + (dimens / 2), y * dimens + z - (dimens / 2 - 1)), 0
CASE 2
LINE (x * dimens + w + (dimens / 2 - 1), y * dimens + z + (dimens / 2))-(x * dimens + w - (dimens / 2 - 1), y * dimens + z + (dimens / 2)), 0
CASE 3
LINE ((x + 1) * dimens + w - (dimens / 2), y * dimens + z + (dimens / 2 - 1))-((x + 1) * dimens + w - (dimens / 2), y * dimens + z - (dimens / 2 - 1)), 0
CASE 4
LINE (x * dimens + w + (dimens / 2 - 1), y * dimens + z - (dimens / 2))-(x * dimens + w - (dimens / 2 - 1), y * dimens + z - (dimens / 2)), 0
END SELECT
END SELECT
ta = ta + 1
pst(x, y) = 1: cicli = cicli + 1

END SUB

SUB tronca
DIM group(lx + 1, ly + 1)
ngroup = 1

FOR b = 0 TO ly + 1
FOR a = 0 TO lx + 1
LOCATE 20, 1: PRINT a; ";"; b,
IF pst(a, b) = 0 THEN PRINT "e' vuoto" ELSE PRINT "e' pieno"
group(a, b) = -pst(a, b)

'i$ = INPUT$(1)
NEXT
NEXT


FOR b = 1 TO ly
LOCATE b, 1: PRINT "                           "
FOR a = 1 TO lx
IF group(a, b) = -1 THEN GOTO nxt

IF group(a, b) > 0 THEN ng = group(a, b) ELSE ng = ngroup: ngroup = ngroup + 1: group(a, b) = ng

IF group(a + 1, b) = 0 THEN group(a + 1, b) = ng
IF group(a, b + 1) = 0 THEN group(a, b + 1) = ng
IF group(a - 1, b) = 0 THEN group(a - 1, b) = ng
IF group(a, b - 1) = 0 THEN group(a, b - 1) = ng

LOCATE b, 3 * a: PRINT group(a, b);


'i$ = INPUT$(1)
nxt:
NEXT
NEXT
i$ = INPUT$(1)


END SUB

