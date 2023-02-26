import turtle
import sys

SIZE = 100

path = map(int, sys.argv[1])

s = turtle.getscreen()

t = turtle.Turtle()
t.pensize(5)
t.turtlesize(1)
t.hideturtle()
t.speed(10)

for d in path:
    if d == 1:
        t.right(90)
        t.forward(10)
        t.left(90)
    if d == 2:
        t.back(10)
    if d == 3:
        t.left(90)
        t.forward(10)
        t.right(90)
    if d == 4:
        t.forward(10)


input()
