#!/usr/bin/env python3

from datetime import datetime
import os


def labirinto(labdef):
    fol = "labs/" + "-".join(str(x) for x in getbase(labdef))
    os.makedirs(fol, exist_ok=True)  
    fname = fol + "/" + "-".join(str(x) for x in labdef) + ".lab"

    print("Labyrint " + " ".join(str(x) for x in labdef))

    if os.path.exists(fname):
       print("Existing. Skip")
       return

    dimx, dimy, inx, iny, outx, outy = labdef

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








def genpopular(ax, ay):
    evenx = (ax % 2 == 0)
    eveny = (ay % 2 == 0)
    l=[]

    l.append([ax, ay, 1, 1, ax, 1]) #CornerX
    l.append([ax, ay, 1, 1, 1, ay]) #CornerY
    l.append([ax, ay, 1, 1, 2, 1]) #SideX
    l.append([ax, ay, 1, 1, 1, 2]) #SideY

    if evenx:
        l.append([ax, ay, ax//2, 1, ax//2+1, 1]) #CloseX
    else:
        l.append([ax, ay, (ax+1)//2, 1, (ax+1)//2, ay]) #CenterX
    

    if eveny:
        l.append([ax, ay, 1, ay//2, 1, ay//2+1]) #CloseY
    else:
        l.append([ax, ay, 1, (ay+1)//2, ax, (ay+1)//2]) #CenterY
        

    if not (evenx and eveny): #at least 1 odd
        l.append([ax, ay, 1, 1, ax, ay]) #Diagonal

    # if not (evenx or eveny): #both odd
        l.append([ax, ay, 1, 1, (ax+1)//2, (ay+1)//2]) #Center
    #     l.append([ax, ay, (ax+1)//2, 1, (ax+1)//2, (ay+1)//2]) #CenterSideX
    #     l.append([ax, ay, 1, (ay+1)//2, (ax+1)//2, (ay+1)//2]) #CenterSideY

    r = sorted(labuniq([getbase(x) for x in labuniq(l)]))
    return(r)



def labuniq(lab):
    res = [] 
    [res.append(x) for x in lab if x not in res]
    return(res)


# def genfriends2(lab):
#     dimx, dimy, inx, iny, outx, outy = lab
#     l=[]

#     l.append([dimx, dimy, inx, iny, outx, outy])
#     l.append([dimx, dimy, dimx - inx + 1, iny, dimx - outx + 1, outy])
#     l.append([dimx, dimy, dimx - inx + 1, dimy - iny + 1, dimx - outx + 1, dimy - outy + 1])
#     l.append([dimx, dimy, inx, dimy - iny + 1, outx, dimy - outy + 1])

#     l.append([dimx, dimy, outx, outy, inx, iny])
#     l.append([dimx, dimy, dimx - outx + 1, outy, dimx - inx + 1, iny])
#     l.append([dimx, dimy, dimx - outx + 1, dimy - outy + 1, dimx - inx + 1, dimy - iny + 1])
#     l.append([dimx, dimy, outx, dimy - outy + 1, inx, dimy - iny + 1])
    

#     l.append([dimy, dimx, iny, inx, outy, outx])
#     l.append([dimy, dimx, dimy - iny + 1, inx, dimy - outy + 1, outx])
#     l.append([dimy, dimx, dimy - iny + 1, dimx - inx + 1, dimy - outy + 1, dimx - outx + 1])
#     l.append([dimy, dimx, iny, dimx - inx + 1, outy, dimx - outx + 1])

#     l.append([dimy, dimx, outy, outx, iny, inx])
#     l.append([dimy, dimx, dimy - outy + 1, outx, dimy - iny + 1, inx])
#     l.append([dimy, dimx, dimy - outy + 1, dimx - outx + 1, dimy - iny + 1, dimx - inx + 1])
#     l.append([dimy, dimx, outy, dimx - outx + 1, iny, dimx - inx + 1])

#     return(sorted(labuniq(l)))

def genfriends(lab):
    l=[]

    for x in range(0,16):
        l.append(transformlab(lab,x&1,x&2,x&4,x&8))

    return(sorted(labuniq(l)))

def getbase(lab):
    return(sorted(labuniq(genfriends(lab)))[0])


def transformlab(lab, flh = False, flv = False, inv = False, ro = False):
    dimx, dimy, inx, iny, outx, outy = lab
    if flh:
        dimx, dimy, inx, iny, outx, outy = dimx, dimy, dimx - inx + 1, iny, dimx - outx + 1, outy
    if flv:
        dimx, dimy, inx, iny, outx, outy = dimx, dimy, inx, dimy - iny + 1, outx, dimy - outy + 1
    if inv:
        dimx, dimy, inx, iny, outx, outy = dimx, dimy, outx, outy, inx, iny
    if ro:
        dimx, dimy, inx, iny, outx, outy = dimy, dimx, iny, inx, outy, outx
    return([dimx, dimy, inx, iny, outx, outy])


def transformdata(lab, flh = False, flv = False, inv = False, ro = False):
    fol = "labs/" + "-".join(str(x) for x in getbase(lab))
    fname = fol + "/" + "-".join(str(x) for x in lab) + ".lab"

    with open(fname) as f:
        l=[]
        for s in f.readlines():
            if flh:
                s = s.translate(str.maketrans({'1':'3', '3':'1'}))
            if flv:
                s = s.translate(str.maketrans({'2':'4', '4':'2'}))
            if inv:
                s = s.translate(str.maketrans({'1':'3', '2':'4', '3':'1', '4':'2'}))[::-1]
            if ro:
                s = s.translate(str.maketrans({'1':'2', '2':'1', '3':'4', '4':'3'}))
            l.append(s)

        l.sort()

        fname = fol + "/" + "-".join(str(x) for x in transformlab(lab, flh, flv, inv, ro)) + ".lab"
        with open(fname, 'w') as f:
            f.writelines(l)


if __name__ == '__main__':
    # sidesum = 16
    # l=[]
    # for ax in range(5, sidesum - 3):
    #     for ay in range(ax, sidesum - ax + 1):
    #         print(ax, ay)
    #         l = l + genpopular(ax, ay)

    # for m in l:
    #     l = l + genfriends(m)
    # l = sorted(labuniq(l))

    

    #test friends errors
    # l = genfriends([7,7,1,1,7,7]) #ok
    # l = genfriends([6,8,1,1,1,8]) #ok
    # l = genfriends([6,8,1,1,2,1]) #err
    # l = genfriends([6,6,4,6,3,6]) #err
    # l = genfriends([7,7,1,1,4,4]) #ok

    
    
    # print ('\n'.join([str(x) for x in l]))

    # for arrlab in l:
    #     labirinto(arrlab)

    # lab = [4,4,2,1,1,1]
    lab = [6,8,1,8,2,8]
    labirinto(lab)
    transformdata(lab, 0, 1, 1, 0)

    for x in range(1,16):
        print(transformlab(lab,x&1,x&2,x&4,x&8))
        transformdata(lab,x&1,x&2,x&4,x&8)
