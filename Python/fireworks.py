'''
FIREWORKS SIMULATION WITH TKINTER

*self-containing code
*to run: simply type python simple.py in your console
*compatible with both Python 2 and Python 3
*Dependencies: tkinter, Pillow (only for background image)
*The design is based on high school physics, with some small twists only for aesthetics purpose

'''
import tkinter as tk
#from tkinter import messagebox
#from tkinter import PhotoImage
from PIL import Image, ImageTk
from time import time, sleep
from random import choice, uniform, randint
from math import sin, cos, radians

# gravity, act as our constant g, you can experiment by changing it
GRAVITY = 0.05
# list of color, can choose randomly or use as a queue (FIFO)
colors = ['red', 'blue', 'yellow', 'white', 'green', 'orange', 'purple', 'seagreen','indigo', 'cornflowerblue']

'''
Generic class for particles

particles are emitted almost randomly on the sky, forming a round of circle (a star) before falling and getting removed
from canvas

Attributes:
    - id: identifier of a particular particle in a star
    - x, y: x,y-coordinate of a star (point of explosion)
    - vx, vy: speed of particle in x, y coordinate
    - total: total number of particle in a star
    - age: how long has the particle last on canvas
    - color: self-explantory
    - cv: canvas
    - lifespan: how long a particle will last on canvas

'''
class part:
    def __init__(self, cv, idx, total, explosion_speed, x=0., y=0., vx = 0., vy = 0., size=2., color = 'red', lifespan = 2, **kwargs):
        self.id = idx
        self.x = x
        self.y = y
        self.initial_speed = explosion_speed
        self.vx = vx
        self.vy = vy
        self.total = total
        self.age = 0
        self.color = color
        self.cv = cv
        self.cid = self.cv.create_oval(
            x - size, y - size, x + size,
            y + size, fill=self.color)
        self.lifespan = lifespan

    def update(self, dt):
        self.age += dt

        # particle expansions
        if self.alive() and self.expand():
            move_x = cos(radians(self.id*360/self.total))*self.initial_speed
            move_y = sin(radians(self.id*360/self.total))*self.initial_speed
            self.cv.move(self.cid, move_x, move_y)
            self.vx = move_x/(float(dt)*1000)

        # falling down in projectile motion
        elif self.alive():
            move_x = cos(radians(self.id*360/self.total))
            # we technically don't need to update x, y because move will do the job
            self.cv.move(self.cid, self.vx + move_x, self.vy+GRAVITY*dt)
            self.vy += GRAVITY*dt

        # remove article if it is over the lifespan
        elif self.cid is not None:
            cv.delete(self.cid)
            self.cid = None

    # define time frame for expansion
    def expand (self):
        return self.age <= 1.2

    # check if particle is still alive in lifespan
    def alive(self):
        return self.age <= self.lifespan

'''
Firework simulation loop:
Recursively call to repeatedly emit new fireworks on canvas

a list of list (list of stars, each of which is a list of particles)
is created and drawn on canvas at every call, 
via update protocol inside each 'part' object 
'''
def simulate(cv):
    t = time()
    explode_points = []
    wait_time = randint(10,100)
    numb_explode = randint(6,10)
    # create list of list of all particles in all simultaneous explosion
    for point in range(numb_explode):
        objects = []
        x_cordi = randint(50,550)
        y_cordi = randint(50, 150)
        speed = uniform (0.5, 1.5)          
        size = uniform (0.5,3)
        color = choice(colors)
        explosion_speed = uniform(0.2, 1)
        total_particles = randint(10,50)
        for i in range(1,total_particles):
            r = part(cv, idx = i, total = total_particles, explosion_speed = explosion_speed, x = x_cordi, y = y_cordi, 
                vx = speed, vy = speed, color=color, size = size, lifespan = uniform(0.6,1.75))
            objects.append(r)
        explode_points.append(objects)

    total_time = .0
    # keeps undate within a timeframe of 1.8 second
    while total_time < 1.8:
        sleep(0.01)
        tnew = time()
        t, dt = tnew, tnew - t
        for point in explode_points:
            for item in point:
                item.update(dt)
        cv.update()
        total_time += dt
    # recursive call to continue adding new explosion on canvas
    root.after(wait_time, simulate, cv)

def close(*ignore):
    """Stops simulation loop and closes the window."""
    global root
    root.quit()
    
if __name__ == '__main__':
    root = tk.Tk()
    cv = tk.Canvas(root, height=600, width=600)
    # use a nice background image
    image = Image.open("image.jpg")
    photo = ImageTk.PhotoImage(image)
    cv.create_image(0, 0, image=photo, anchor='nw')

    cv.pack()
    root.protocol("WM_DELETE_WINDOW", close)

    root.after(100, simulate, cv)

    root.mainloop()
