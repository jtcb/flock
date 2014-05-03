import random
import numpy as np

filename = "../simulator/test.txt"


num_wolves = 6
num_sheep = 20
T = 500

fieldc = [100, 0, 0, 0, 0, 100, 100, 100]
penc = [25, 0, 0, 0, 0, 25, 25, 25]
field = "100 0 0 0 0 100 100 100"
pen = "25 0 0 0 0 0 25 25 25"

wall_force = 10
wall_dist = 10
sheep_attract = 1
sheep_repulse = 20
max_v = 1

wolf_distance = 40
wolf_force = 10

class Run:

    def __init__(self):
        self.sx = [random.random() * fieldc[0]/2 + fieldc[0]/2 for i in range(num_sheep)]
        self.sy = [random.random() * fieldc[0] for i in range(num_sheep)]
        self.wx = [random.random() * fieldc[0]/2 for i in range(num_wolves)]
        self.wy = [random.random() * fieldc[0] for i in range(num_wolves)]

    def plan(self):
        pass


    def update_sheep(self):

        fx = np.zeros(num_sheep)
        fy = np.zeros(num_sheep)

        for i in range(num_sheep):
            x =self.sx[i]; y =self.sy[i]
            if (x-wall_dist) < 0:
                fx[i] += wall_force / (x+.01)
            if (x+wall_dist) > 100:
                fx[i] += wall_force / (99-x+.01) * -1
            if (y-wall_dist) < 0:
                fy[i] += wall_force / (y+.01)
            if (y+wall_dist) > 100:
                fy[i] += wall_force / (99-y+.01) * -1


            for j in range(num_sheep):
                if i != j:
                    xd =self.sx[j] - x
                    yd =self.sy[j] - y
                    sqdist = xd*xd+yd*yd
                    if sqdist < wall_dist*wall_dist:
                        fx[i] += sheep_repulse / (xd+.01) * -1
                        fy[i] += sheep_repulse / (yd+.01) * -1
                    else:
                        fx[i] += xd/100.0
                        fy[i] += yd/100.0


            for j in range(num_wolves):
                xd =self.wx[j] - x
                yd =self.wy[j] - y

                sqdist = xd*xd+yd*yd
                if sqdist < wolf_distance*wolf_distance:
                    fx[i] += wolf_force * (1/(xd+.01)) * -1
                    fy[i] += wolf_force * (1/(yd+.01)) * -1



            # IF SHEEP IS IN PEN, KEEP IT THERE!
            if x < 25 and y < 25:
                fx[i] = -100.0
                fy[i] = -100.0


            if fx[i] > 0:
                fx_f = np.min([fx[i], max_v])
            else:
                fx_f = np.max([fx[i], -1 * max_v])
            if fy[i] > 0:
                fy_f = np.min([fy[i], max_v])
            else:
                fy_f = np.max([fy[i], -1 * max_v])

            self.sx[i] += fx_f
            self.sy[i] += fy_f
            if self.sx[i] < 0:
               self.sx[i] = 0
            if self.sy[i] < 0:
               self.sy[i] = 0
            if self.sx[i] > 99:
               self.sx[i] = 99
            if self.sy[i] > 99:
               self.sy[i] = 99

    def update_wolves(self, plan):

        for i in range(num_wolves):

            (xv, yv) = plan.execute(self.wx, self.wy, self.sx, self.sy, 25, i)

            self.wx[i] += xv
            self.wy[i] += yv

            if self.wx[i] < 0:
               self.wx[i] = 0
            if self.wy[i] < 0:
               self.wy[i] = 0
            if self.wx[i] > 99:
               self.wx[i] = 99
            if self.wy[i] > 99:
               self.wy[i] = 99


    def fitness(self, plan, filename, save=True):

        if save:
            file = open("results/"+filename, 'w')
            file.write(field + '\n')
            file.write(pen + '\n')
            file.write(str(num_wolves) + ' ' + str(num_sheep) + ' ' + str(T) + '\n')

        for t in range(T):

            if save:
                for i in range(num_wolves):
                    file.write(str(self.wx[i]) + ' ' + str(self.wy[i]) + ' ')
                for i in range(num_sheep):
                    file.write(str(self.sx[i]) + ' ' + str(self.sy[i]) + ' ')
                file.write('\n')

            self.update_sheep()
            self.update_wolves(plan)

        # Calculate fitness:
        num = 0
        for i in range(num_sheep):
            if self.sx[i] < 25 and self.sy[i] < 25:
                num += 1
        #print str(num)+"/"+str(len(self.sx))

        return num / float(num_sheep)



def main():
    r = Run()
    r.fitness(1)

if __name__ == "__main__":
    main()