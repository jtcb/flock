import numpy as np
import math








class Plan:

    '''
    Set field 1 of condition: Must be a Test
    Set field 2 of condition: Must be an Action
    Set field 3 of condition: Must be an Action

    Add action (to top level): Must be an Action or Condition


    '''
    legal = {}

    def __init__(self):

        self.Conditionals = [DirectionCondition]
        self.Expressions = [DirectionExpression]
        self.Observations = [self.get_sheep_cm, self.get_nearest_wolf, self.get_own_pos, self.get_goal]
        self.Actions = [DirectionAction("N"), DirectionAction("E"), DirectionAction("S"), DirectionAction("W"), DirectionAction("Stay")]

        self.this_conditionals = []
        self.this_expressions = []

        self.sx = None
        self.sy = None
        self.wx = None
        self.wy = None
        self.pen = None
        self.wolf = None

        self.action = None



    def __str__(self):
        return str(self.action)


    def execute(self, wx, wy, sx, sy, pen_size, wolf_index):
        self.sx = sx
        self.sy = sy
        self.wx = wx
        self.wy = wy

        pen_coord = Coordinate(pen_size/2, pen_size/2)

        self.pen = pen_coord
        self.wolf = wolf_index

        return self.action.execute()

    '''
    Prebuilt expressions:

    def get_wolf_direction_to(self, c2):
        return self.get_direction_to(self.get_own_pos(), c2)

    def get_wolf_direction_to_sheep(self):
        wdts = self.get_direction_to(self.get_own_pos(), self.get_sheep_cm())
        #print "Wolf dir to sheep is", wdts
        return wdts

    def get_sheep_direction_to_pen(self):
        sdtp = self.get_direction_to(self.get_sheep_cm(), self.pen)
        #print "Sheep dir to pen is", sdtp
        return sdtp

    def get_closest_wolf_direction(self):
        return self.get_direction_to(self.get_own_pos(), self.get_nearest_wolf())

    '''




    '''
    OBSERVATIONS
    Primitive functions: (return Coordinates)
    '''

    def get_sheep_cm(self):
        x = np.sum(self.sx) / len(self.sx)
        y = np.sum(self.sy) / len(self.sy)
        return Coordinate(x, y)

    def get_nearest_wolf(self):
        mindist = float("inf")
        x = 0
        y = 0
        for i in range(len(self.wx)):
            if i != self.wolf:
                dist = self.wx[i]*self.wx[i] + self.wy[i]*self.wy[i]
                if dist < mindist:
                    mindist = dist
                    x = self.wx[i]
                    y = self.wy[i]
        return Coordinate(x, y)

    def get_own_pos(self):
        return Coordinate(self.wx[self.wolf], self.wy[self.wolf])

    def get_goal(self):
        return self.pen





'''
EXPRESSIONS
Directions
'''

class DirectionExpression():

    def __init__(self):
        self.obs1 = None
        self.obs2 = None
        self.depth = 0
        self.marked = 0

        self.obs_map = None
        self.ptemp = None

    # Awkward but necessary:
    def set_map(self, ptemp):
        self.ptemp = ptemp
        self.obs_map = {self.ptemp.get_sheep_cm:"Sheep center of mass",
                        self.ptemp.get_nearest_wolf: "Nearest wolf",
                        self.ptemp.get_own_pos: "Own position",
                        self.ptemp.get_goal: "Goal position"}


    def __str__(self):
        ret = "Direction of " + self.obs_map[self.obs1] + " to " + self.obs_map[self.obs2] + '\n'
        return ret

    # Direction of coordinate c1 to c2
    def evaluate(self):

        c1 = self.obs1()
        c2 = self.obs2()

        dx = abs(c2.x - c1.x)
        dy = abs(c2.y - c1.y)

        if c2.y > c1.y:
            if c2.x > c1.x:
                if dy > dx:
                    return "N"
                else:
                    return "E"
            else:
                if dy > dx:
                    return "N"
                else:
                    return "W"
        else:
            if c2.x > c1.x:
                if dy > dx:
                    return "S"
                else:
                    return "E"
            else:
                if dy > dx:
                    return "S"
                else:
                    return "W"


'''
CONDITIONALS / CONDITIONS
'''

class DirectionCondition:

    def __init__(self):
        self.expression = None
        self.options = {"N":None, "E":None, "S":None, "W":None}
        self.depth = 0
        self.marked = 0
        self.isAction = False

    def __str__(self):
        ret = "Evaluate:\n\t" + str(self.expression) + '\n'
        ret += "If this is N:\n\t" + str(self.options["N"]) + '\n'
        ret += "If this is E:\n\t" + str(self.options["E"]) + '\n'
        ret += "If this is S:\n\t" + str(self.options["S"]) + '\n'
        ret += "If this is W:\n\t" + str(self.options["W"]) + '\n'
        return ret

    def execute(self):
        val = self.expression.evaluate()
        return self.options[val].execute()



'''
ACTIONS
'''

class DirectionAction:

    def __init__(self, direction, velocity=1.0):
        self.dir = direction
        self.v = velocity
        self.depth = 0
        self.isAction = True

    def __str__(self):
        if self.dir == "Stay":
            ret = "Stay"
        else:
            ret = "Move " + str(self.dir) + " with velocity " + str(self.v) + '\n'
        return ret

    def execute(self):
        if self.dir == "Stay":
            return(0, 0)
        elif self.dir == "N":
            return (0, self.v)
        elif self.dir == "E":
            return (self.v, 0)
        elif self.dir == "S":
            return (0, -1 * self.v)
        else:
            return (-1 * self.v, 0)




#class DistanceCondition:
    #def __init__(self, test, ?)




class Coordinate:
    def __init__(self, x, y):
        self.x = x
        self.y = y








def main():

    import run
    import TestPlan

    p = TestPlan.myPlan()

    r = run.Run()
    print r.fitness(p)


if __name__ == "__main__":
    main()