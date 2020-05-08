Public dimx
Public dimy
Public inx
Public iny
Public outx
Public outy
Public lab()
Public mov()
Public arr()
Public px
Public py
Public ta


call Labirinto







Sub Labirinto()

orai = Now

dimx = 6
dimy = 6
inx = 3
iny = 6
outx = 4
outy = 6

lout = 13
    
'qui crea l'array con le direzioni
ReDim lab(dimx + 1, dimy + 1)
For a = 0 To dimx + 1
    lab(a, 0) = -1
    lab(a, dimy + 1) = -1
Next
For a = 0 To dimy + 1
    lab(0, a) = -1
    lab(dimx + 1, a) = -1
Next
'qui crea l'array sequenziale

ReDim arr(dimx + 1, dimy + 1)
For a = 0 To dimx + 1
    arr(a, 0) = -1
    arr(a, dimy + 1) = -1
Next
For a = 0 To dimy + 1
    arr(0, a) = -1
    arr(dimx + 1, a) = -1
Next

'lab(outx, outy) = -2


ReDim mov(dimx * dimy)


For a = 0 To dimx + 1
For b = 0 To dimy + 1
'ActiveSheet.Cells(b + 1, a + 1) = lab(a, b)
Next
Next

ta = 1
px = inx
py = iny
ci = 0


'comincia a girare
indietro=false
While True
'chiodello per evitare che passi sull'uscita
If px = outx And py = outy Then
    indietro=true
ElseIf lab(px, py) < 1 And lab(px + 1, py) = 0 Then
    lastmove = 1
    lab(px, py) = lastmove
    mov(ta) = lastmove
    arr(px, py) = ta
    'Call Disegna
    ta = ta + 1
    px = px + 1
ElseIf lab(px, py) < 2 And lab(px, py + 1) = 0 Then
    lastmove = 2
    lab(px, py) = lastmove
    mov(ta) = lastmove
    arr(px, py) = ta
    'Call Disegna
    ta = ta + 1
    py = py + 1
ElseIf lab(px, py) < 3 And lab(px - 1, py) = 0 Then
    lastmove = 3
    lab(px, py) = lastmove
    mov(ta) = lastmove
    arr(px, py) = ta
    'Call Disegna
    ta = ta + 1
    px = px - 1
ElseIf lab(px, py) < 4 And lab(px, py - 1) = 0 Then
    lastmove = 4
    lab(px, py) = lastmove
    mov(ta) = lastmove
    arr(px, py) = ta
    'Call Disegna
    ta = ta + 1
    py = py - 1
Else
    indietro=true
end if


if indietro then
    If px = outx And py = outy And ta = dimx * dimy Then
    'OK

    'Range(Cells(lout, 1), Cells(lout, dimx * dimy - 1)) = mov
    wscript.echo join(mov,",")
    lout = lout + 1
    
    
    'MsgBox "ok"
    End If
    'torna indietro
    If ta = 1 Then
        'MsgBox "FINE " & CStr(lout - 13) & " " & Format(Now - orai, "hh.mm.ss")
        wscript.echo "FINE " & CStr(lout - 13) & " " & int((Now - orai)*24*60*60)
        Exit Sub
    End If
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
    Case Else
         MsgBox "ANTANI"
    Exit Sub
    End Select
    
    ci = ci + 1
    If ci Mod 10 = 0 Then
        'Call DisegnaPercorso
        'Call RiduciIsole
        Call RiduciVicini
    End If
    If ci Mod 100 = 0 Then
        'Call DisegnaPercorso
        Call RiduciIsole
        'Call RiduciVicini
    End If
    
    indietro=false
End If

Wend


End Sub

Private Sub RiduciVicini()

'Range("w1").Range(Cells(1, 1), Cells(dimx + 2, dimy + 2)) = arr
'Qui analizza i vicini e trova il valore minore tra i maggiori confinanti con un buco

ma = 0
For x = 1 To dimx
    For y = 1 To dimy
        If arr(x, y) = "" Then
            
            'qui calcola i vicini
            f1 = arr(x + 1, y)
            f2 = arr(x, y + 1)
            f3 = arr(x - 1, y)
            f4 = arr(x, y - 1)
            u = 0
            If f1 = "" Then u = u + 1
            If f2 = "" Then u = u + 1
            If f3 = "" Then u = u + 1
            If f4 = "" Then u = u + 1
            If f1 = "" Then f1 = 0
            If f2 = "" Then f2 = 0
            If f3 = "" Then f3 = 0
            If f4 = "" Then f4 = 0
            
            If (x = outx And y = outy) Then umax = 0 Else umax = 1
            If u <= umax Then
                v = Max(f1, f2, f3, f4)
                If ma = 0 Or v < ma Then
                    ma = v
                '    If ma = f1 Then
                '        md = 3
                '    ElseIf ma = f2 Then
                '        md = 4
                '    ElseIf ma = f3 Then
                '        md = 1
                '    ElseIf ma = f3 Then
                '        md = 2
                '    Else
                '        md = 0
                '    End If
                End If
            End If
                    
        End If
    Next
Next
'Range("AN1") = ma
'Range("AN2") = md

'indietreggia
If ma <> 0 Then

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
    Case Else
         MsgBox "ANTANI"
    Exit Sub
    End Select
Wend
End If


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
        If aa(x, y) = "" Then
            v = floodfill(aa, x, y)
            If ma = 0 Or v < ma Then ma = v
        End If
    Next
Next

'indietreggia
If ma <> 0 Then
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
    Case Else
         MsgBox "ANTANI"
    Exit Sub
    End Select
Wend
End If





End Sub

Private Function floodfill(aa, x, y)

If aa(x, y) = -1 Then
    floodfill = 0
ElseIf aa(x, y) = "" Then
    aa(x, y) = -2
    f1 = floodfill(aa, x + 1, y)
    f2 = floodfill(aa, x, y + 1)
    f3 = floodfill(aa, x - 1, y)
    f4 = floodfill(aa, x, y - 1)
    floodfill = Max(f1, f2, f3, f4)
Else
    floodfill = aa(x, y)
End If
End Function

function max(a,b,c,d)
	m=a
	if b>m then m=b
	if c>m then m=c
	if d>m then m=d	
end function
