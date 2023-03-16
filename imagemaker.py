import sys
#   pn  w   h   sx  sy  fx  fy  path
#   0   1   2   3   4   5   6   7

w = int(sys.argv[1])
h = int(sys.argv[2])
sx = int(sys.argv[3]) -1
sy = int(sys.argv[4]) -1
fx = int(sys.argv[5]) -1
fy = int(sys.argv[6]) -1
path = [int(x)-1 for x in sys.argv[7]]

#      X
#    ---->
#    |
#    | Y
#    |
#    v

#       3

#   2      0

#       1

def move(m,x,y):
    if m == 0:
        return (x+1,y)
    if m == 1:
        return (x,y+1)
    if m == 2:
        return (x-1,y)
    else: # m==4
        return (x,y-1)

# evry cell has 1 bit for evry wall
grid = [[0b1111 for _ in range(h)] for _ in range(w)]

i,j = sx,sy
# let's break walls
for m in path:
    grid[i][j] &= ~(1 << m) # binary arcane spell
    i,j = move(m,i,j)
    grid[i][j] &= ~(1 << ((m+2)%4)) # stronger binary arcane spell


def ascii_print(grid):
    ascii_map = [['+' for _ in range(2*h +1)] for _ in range(2*w +1)]

    for j in range(0,2*h,2):
        for i in range(0,2*w,2):
            ascii_map[i+1][j+1] = ' '

            if grid[i//2][j//2] & (1 << 0) == 0 :
                ascii_map[i+2][j+1] = ' '
            else:
                ascii_map[i+2][j+1] = '|'

            if grid[i//2][j//2] & (1 << 1) == 0 :
                ascii_map[i+1][j+2] = ' '
            else:
                ascii_map[i+1][j+2] = '-'

            if grid[i//2][j//2] & (1 << 2) == 0 :
                ascii_map[i+0][j+1] = ' '
            else:
                ascii_map[i+0][j+1] = '|'

            if grid[i//2][j//2] & (1 << 3) == 0 :
                ascii_map[i+1][j+0] = ' '
            else:
                ascii_map[i+1][j+0] = '-'

    ascii_map[2*sx+1][2*sy+1]='s'
    ascii_map[2*fx+1][2*fy+1]='f'



    for j in range(2*h+1):
        for i in range(2*w+1):
            print(ascii_map[i][j],end='')
        print()


# <Down>
#
# def svg_rect(x,y,w,h):
#     #print((x,y,w,h))
#     return "<rect x=\"" + str(x) + "\" y=\"" + str(y) + "\" width=\"" + str(w) + "\" height=\"" + str(h) + "\" fill=\"#000000\">"
#
#
# def svg_maker(grid,scale):
#     wall_thickness = 0.1 # % over path largeness
#
#
#     out = "<svg width=\"" + str(w*scale) + "\" height=\"" + str(h*scale) + "\">"
#
#     # horizontal squares
#     for j in range(h):
#         for i in range(w):
#
#             if grid[i][j] & 0b0101 :
#                 print(0,end='')
#             else:
#                 print('Ø',end='')
#
#         print()
#
#        # if i < rx:
#        #     out += svg_rect(x=i*scale+scale*wall_thickness,
#        #                     y=j*scale+scale*wall_thickness,
#        #                     w=(rx-i)*scale - 2*scale*wall_thickness,
#        #                     h= 1*scale - 2*scale*wall_thickness)
#
#             
# 
#         
# 
#     out += "</svg>"
#     return out

def flip(d):
    a,b = d
    return (b,a)


def wall(x,y,d):
    if d == 0: #right
        return ((x+1,y),(x+1,y+1))
    if d == 1: #bottom
        return ((x,y+1),(x+1,y+1))
    if d == 2: #left
        return ((x,y),(x,y+1))
    else: #top
        return ((x,y),(x+1,y))

def wall_str(w):
    return str(w[0][0]) + "," + str(w[0][1]) + " " + str(w[1][0]) + "," + str(w[1][1])

def pathpoligon(x,y,i=0):
    if i==len(path):
        return []

    d = path[i]
    pd= path[i-1]

    #x,y = move(d,x,y)
    p = pathpoligon(*move(d,x,y),i+1)

    if i!=0 and d != pd:
        w1=p


#       if d == (pd+1)%4:
#           w1 = [*wall(x,y,(d-1)%4)]
#           w2 = [*wall(x,y,(d-2)%4)]
#           w1 = p + w2 + w1
#       else:
#           w1 = [*wall(x,y,(d+1)%4)]
#           w2 = [*wall(x,y,(d+2)%4)]
#           w1 = w2 + w1 + p
    else:
        w1 = [*wall(x,y,(d-1)%4)]
        w2 = [*wall(x,y,(d+1)%4)]
        w2.reverse()
        w1 = w1 + p + w2


    return w1





def svg_maker(scale):
    p = pathpoligon(sx,sy)

    out = "<svg width=\"" + str(w*scale) + "\" height=\"" + str(h*scale) + "\">"
    out += "<polygon points=\""

    for i in p:
        x,y = i
        out += str(x*scale) + "," + str(y*scale) + " "


    out += "\" style=\"fill:none;stroke:purple;stroke-width:1\" />"
    out += "<rect width=\"600\" height=\"800\" style=\"fill:none;stroke:purple;stroke-width:1\">"
    out += "</svg>"
    return out

def process():
    if len(path) == 0:
        return None
    svg = svg_maker(25)
    path.pop()
    process()
    print(svg)

if __name__ == "__main__":
    process()

#ascii_print(grid)
#print(svg_maker(100))
