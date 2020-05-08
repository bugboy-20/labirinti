#!/usr/bin/env python3

from datetime import datetime
import os


def labirinto(dimx, dimy, inx, iny, outx, outy):
    os.makedirs("labs", exist_ok=True)
    fname = "labs/" + "-".join(str(x) for x in [dimx, dimy, inx, iny, outx, outy]) + ".lab"
    
    print("Labyrint " + " ".join(str(x) for x in [dimx, dimy, inx, iny, outx, outy]))
    if os.path.exists(fname):
       print("Existing. Skip")
       return

    orai = datetime.now()
    lout = 0
        
    #Creates array with directions
    lab = [[0 for i in range(dimy + 2)] for j in range(dimx + 2)]
    for a in range(0, dimx + 1):
        lab[a][0] = -1
        lab[a][dimy + 1] = -1

    for a in range(0, dimy + 1):
        lab[0][a] = -1
        lab[dimx + 1][a] = -1

    #Creates the sequential array
    arr = [[0 for i in range(dimy + 2)] for j in range(dimx + 2)]
    for a in range(0, dimx + 1):
        arr[a][0] = -1
        arr[a][dimy + 1] = -1

    for a in range(0, dimy + 1):
        arr[0][a] = -1
        arr[dimx + 1][a] = -1

    #Creates the array with all steps
    mov = [0 for i in range(0, dimx * dimy + 1)]

    ta = 1
    px = inx
    py = iny
    ci = 0


    #start
    indietro = False
    f = open(fname + ".part", "w")

    while True:
        #to avoid it passes to the exit point
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
                #Labyrinth ok!
                #print(",".join(str(x) for x in mov[1:-1]))
                f.write("".join(str(x) for x in mov[1:-1]) + "\n")
                lout = lout + 1

            #go back
            if ta == 1:
                #End!
                break
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
                print("Errore!")
                return

            
            ci = ci + 1
            #optimizations
            if ci % 10 == 0:
                #analyze the neighbours and finds the minimum value of the maximum neighbour with a hole
                ma = 0
                for x in range(1, dimx + 1):
                    for y in range(1, dimy + 1):
                        if arr[x][y] == "":
                            
                            #calculate the neighbours
                            f1 = arr[x + 1][y]
                            f2 = arr[x][y + 1]
                            f3 = arr[x - 1][y]
                            f4 = arr[x][y - 1]
                            u = 0
                            if f1 == "":
                                u = u + 1
                                f1 = 0
                            if f2 == "":
                                u = u + 1
                                f2 = 0
                            if f3 == "":
                                u = u + 1
                                f3 = 0
                            if f4 == "":
                                u = u + 1
                                f4 = 0
                            
                            if (x == outx and y == outy):
                                umax = 0
                            else:
                                umax = 1
                            if u <= umax:
                                v = max(f1, f2, f3, f4)
                                if ma == 0 or v < ma:
                                    ma = v

                #go back
                if ma != 0:
                    while ta > ma:
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
                            print("Errore!")
                            return
                            

            if ci % 100 == 0:
                #Analyze the holes and finds the minimum value in the maximum contiguous with a hole
                ma = 0
                #copy the array
                aa = [[arr[j][i] for i in range(dimy + 2)] for j in range(dimx + 2)]
                #find the isles
                for x in range(1, dimx + 1):
                    for y in range(1, dimy + 1):
                        if aa[x][y] == "":
                            v = floodfill(aa, x, y)
                            if ma == 0 or v < ma:
                                ma = v
                #print(aa)

                #go back
                if ma != 0:
                    while ta > ma:
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
                            print("Error!")
                            return


            indietro = False

    f.close()
    os.rename(fname + ".part", fname)

    print("Fine " + str(lout) + " " + str((datetime.now() - orai).seconds))







def floodfill(aa, x, y):

    if aa[x][y] == -1:
        return 0
    elif aa[x][y] == "":
        aa[x][y] = -2
        f1 = floodfill(aa, x + 1, y)
        f2 = floodfill(aa, x, y + 1)
        f3 = floodfill(aa, x - 1, y)
        f4 = floodfill(aa, x, y - 1)
        return max(f1, f2, f3, f4)
    else:
        return aa[x][y]



if __name__ == '__main__':

    labirinto(6, 6, 4, 6, 3, 6) # 948 0

    # labirinto(3, 3, 1, 1, 3, 3)
    # labirinto(3, 3, 1, 1, 1, 3)
    # labirinto(4, 4, 1, 1, 1, 4)
    # labirinto(4, 4, 1, 2, 1, 3)
    # labirinto(5, 5, 1, 1, 1, 5)
    # labirinto(5, 5, 1, 1, 5, 5)
    # labirinto(5, 5, 1, 1, 3, 3)
    # labirinto(5, 5, 1, 3, 5, 3)
   
    # labirinto(7, 7, 1, 1, 7, 7) # 111712 97
    # labirinto(6, 6, 1, 1, 6, 1) # 1770 2
    # labirinto(6, 8, 1, 1, 6, 1) # 59946
    # labirinto(4, 12, 1, 1, 4, 1) # 15024 6
    # labirinto(5, 9, 1, 1, 5, 1) # 10444 7
    # labirinto(5, 9, 1, 1, 1, 9) # 28002 13
    # labirinto(5, 9, 1, 1, 5, 9) # 28417 13

    #test big more big
    # labirinto(8, 8, 1, 4, 1, 5) # 3199463 2762 ??? to be fixed
    # labirinto(8, 8, 4, 1, 5, 1) # 2910720 2356# ??? to be fixed


    #test derived
    # labirinto(6, 8, 1, 1, 2, 1) # 22866 ???? to be fixed
    # labirinto(6, 8, 6, 1, 5, 1) # 32675
    # labirinto(6, 8, 6, 8, 5, 8) # 32675
    # labirinto(6, 8, 1, 8, 2, 8) # 32675

    # labirinto(8, 6, 1, 1, 1, 2) # 32675
    # labirinto(8, 6, 1, 6, 1, 5) # 32675
    # labirinto(8, 6, 8, 6, 8, 5) # 32675
    # labirinto(8, 6, 8, 1, 8, 2) # 32675

    # labirinto(6, 8, 2, 1, 1, 1) # 32675
    # labirinto(6, 8, 5, 1, 6, 1) # 32675
    # labirinto(6, 8, 5, 8, 6, 8) # 32675
    # labirinto(6, 8, 2, 8, 1, 8) # 32675

    # labirinto(8, 6, 1, 2, 1, 1) # 32675
    # labirinto(8, 6, 1, 5, 1, 6) # 32675
    # labirinto(8, 6, 8, 5, 8, 6) # 32675
    # labirinto(8, 6, 8, 2, 8, 1) # 32675

    