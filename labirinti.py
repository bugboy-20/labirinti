#!/usr/bin/env python3


# Public dimx
# Public dimy
# Public inx
# Public iny
# Public outx
# Public outy
# Public lab()
# Public mov()
# Public arr()
# Public px
# Public py
# Public ta


from datetime import datetime

def labirinto(dimx, dimy, inx, iny, outx, outy):

    orai = datetime.now()
    lout = 0
        
    #qui crea l'array con le direzioni
    #lab[][] #dimx + 1, dimy + 1
    lab = [[0 for i in range(dimy + 2)] for j in range(dimx + 2)]
    for a in range(0, dimx + 1):
        lab[a][0] = -1
        lab[a][dimy + 1] = -1

    for a in range(0, dimy + 1):
        lab[0][a] = -1
        lab[dimx + 1][a] = -1

    #qui crea l'array sequenziale
    #ReDim arr(dimx + 1, dimy + 1)
    arr = [[0 for i in range(dimy + 2)] for j in range(dimx + 2)]
    for a in range(0, dimx + 1):
        arr[a][0] = -1
        arr[a][dimy + 1] = -1

    for a in range(0, dimy + 1):
        arr[0][a] = -1
        arr[dimx + 1][a] = -1

    #ReDim mov(dimx * dimy)
    mov = [0 for i in range(0, dimx * dimy + 1)]

    ta = 1
    px = inx
    py = iny
    ci = 0


    #comincia a girare
    indietro = False
    while True:
        #chiodello per evitare che passi sull'uscita
        if px == outx and py == outy:
            indietro = True
        elif lab[px][py] < 1 and lab[px + 1][py] == 0:
            lastmove = 1
            lab[px][py] = lastmove
            mov[ta] = lastmove
            arr[px][py] = ta
            #Call Disegna
            ta = ta + 1
            px = px + 1
        elif lab[px][py] < 2 and lab[px][py + 1] == 0:
            lastmove = 2
            lab[px][py] = lastmove
            mov[ta] = lastmove
            arr[px][py] = ta
            #Call Disegna
            ta = ta + 1
            py = py + 1
        elif lab[px][py] < 3 and lab[px - 1][py] == 0:
            lastmove = 3
            lab[px][py] = lastmove
            mov[ta] = lastmove
            arr[px][py] = ta
            #Call Disegna
            ta = ta + 1
            px = px - 1
        elif lab[px][py] < 4 and lab[px][py - 1] == 0:
            lastmove = 4
            lab[px][py] = lastmove
            mov[ta] = lastmove
            arr[px][py] = ta
            #Call Disegna
            ta = ta + 1
            py = py - 1
        else:
            indietro = True
        


        if indietro:
            if px == outx and py == outy and ta == dimx * dimy:
                #OK
                print(",".join(str(x) for x in mov))
                lout = lout + 1

            #torna indietro
            if ta == 1:
                print("FINE " + str(lout) + " " + str((datetime.now() - orai).seconds))
                return
            m = mov[ta - 1]
            if m == 1:
                lab[px][py] = 0
                mov[ta] = 0
                arr[px][py] = ""
                #Call Disegna
                ta = ta - 1
                px = px - 1
            elif m == 2:
                lab[px][py] = 0
                mov[ta] = 0
                arr[px][py] = ""
                #Call Disegna
                ta = ta - 1
                py = py - 1
            elif m == 3:
                lab[px][py] = 0
                mov[ta] = 0
                arr[px][py] = ""
                #Call Disegna
                ta = ta - 1
                px = px + 1
            elif m == 4:
                lab[px][py] = 0
                mov[ta] = 0
                arr[px][py] = ""
                #Call Disegna
                ta = ta - 1
                py = py + 1
            else:
                print("ANTANI")
                return

            
            ci = ci + 1
            # if ci % 10 == 0:
            #     #Call DisegnaPercorso
            #     #Call RiduciIsole
            #     Call RiduciVicini
            # if ci % 100 == 0:
            #     #Call DisegnaPercorso
            #     Call RiduciIsole
            #     #Call RiduciVicini
            
            indietro = False








""" 

Private Sub RiduciVicini()

'Qui analizza i vicini e trova il valore minore tra i maggiori confinanti con un buco

ma = 0
For x = 1 To dimx
    For y = 1 To dimy
        if arr(x, y) = "" Then
            
            'qui calcola i vicini
            f1 = arr(x + 1, y)
            f2 = arr(x, y + 1)
            f3 = arr(x - 1, y)
            f4 = arr(x, y - 1)
            u = 0
            if f1 = "" Then u = u + 1
            if f2 = "" Then u = u + 1
            if f3 = "" Then u = u + 1
            if f4 = "" Then u = u + 1
            if f1 = "" Then f1 = 0
            if f2 = "" Then f2 = 0
            if f3 = "" Then f3 = 0
            if f4 = "" Then f4 = 0
            
            if (x = outx and y = outy) Then umax = 0 else umax = 1
            if u <= umax Then
                v = Max(f1, f2, f3, f4)
                if ma = 0 Or v < ma Then
                    ma = v
                '    if ma = f1 Then
                '        md = 3
                '    elif ma = f2 Then
                '        md = 4
                '    elif ma = f3 Then
                '        md = 1
                '    elif ma = f3 Then
                '        md = 2
                '    else
                '        md = 0
                '    End if
                End if
            End if
                    
        End if
    Next
Next
'Range("AN1") = ma
'Range("AN2") = md

'indietreggia
if ma <> 0 Then

While ta > ma
    Select Case mov(ta - 1)
    Case 1
        lab(px, py) = 0
        mov(ta) = 0
        arr(px, py) = ""
        'Call Disegna
        ta = ta - 1
        px = px - 1
    Case 2
        lab(px, py) = 0
        mov(ta) = 0
        arr(px, py) = ""
        'Call Disegna
        ta = ta - 1
        py = py - 1
    Case 3
        lab(px, py) = 0
        mov(ta) = 0
        arr(px, py) = ""
        'Call Disegna
        ta = ta - 1
        px = px + 1
    Case 4
        lab(px, py) = 0
        mov(ta) = 0
        arr(px, py) = ""
        'Call Disegna
        ta = ta - 1
        py = py + 1
    Case else
         MsgBox "ANTANI"
    Exit Sub
    End Select
Wend
End if


End Sub






Private Sub RiduciIsole()


'Range("z1..ah9") = arr

'Qui analizza i buchi e trova il valore minore tra i maggiori confinanti con un buco
'copia l 'array
Dim aa()
ReDim aa(dimx + 1, dimy + 1)
ma = 0
For x = 0 To dimx + 1
    For y = 0 To dimy + 1
        aa(x, y) = arr(x, y)
    Next
Next
'trova le isole
For x = 1 To dimx
    For y = 1 To dimy
        if aa(x, y) = "" Then
            v = floodfill(aa, x, y)
            if ma = 0 Or v < ma Then ma = v
        End if
    Next
Next

'indietreggia
if ma <> 0 Then
While ta > ma
    Select Case mov(ta - 1)
    Case 1
        lab(px, py) = 0
        mov(ta) = 0
        arr(px, py) = ""
        'Call Disegna
        ta = ta - 1
        px = px - 1
    Case 2
        lab(px, py) = 0
        mov(ta) = 0
        arr(px, py) = ""
        'Call Disegna
        ta = ta - 1
        py = py - 1
    Case 3
        lab(px, py) = 0
        mov(ta) = 0
        arr(px, py) = ""
        'Call Disegna
        ta = ta - 1
        px = px + 1
    Case 4
        lab(px, py) = 0
        mov(ta) = 0
        arr(px, py) = ""
        'Call Disegna
        ta = ta - 1
        py = py + 1
    Case else
         MsgBox "ANTANI"
    Exit Sub
    End Select
Wend
End if





End Sub

Private Function floodfill(aa, x, y)

if aa(x, y) = -1 Then
    floodfill = 0
elif aa(x, y) = "" Then
    aa(x, y) = -2
    f1 = floodfill(aa, x + 1, y)
    f2 = floodfill(aa, x, y + 1)
    f3 = floodfill(aa, x - 1, y)
    f4 = floodfill(aa, x, y - 1)
    floodfill = Max(f1, f2, f3, f4)
else
    floodfill = aa(x, y)
End if
End Function

function max(a,b,c,d)
	m=a
	if b>m then m=b
	if c>m then m=c
	if d>m then m=d	
end function
 """




if __name__ == '__main__':
    #labirinto(6, 6, 4, 6, 3, 6) #948 9
    #labirinto(8, 8, 1, 4, 1, 5)
    #labirinto(7, 7, 1, 1, 7, 7)
    #labirinto(6, 6, 1, 1, 6, 1) #1770 19
    labirinto(6, 8, 1, 1, 6, 1)

