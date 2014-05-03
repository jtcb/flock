'''
Isaac Julien
Evolution for decision tree model of shepherding problem





Basic setup:
Plan = Conditionals + Expressions + Actions + Observations
Conditionals = Expression + Actions (based on evaluation of expression)
    - classified into DirectionConditionals only for now
Expressions = evaluate to some Type of value
    - Right now, this value is a direction resulting a relation b/n coordinates
Observations = return some Type of info
    - Coordinate only for now
Actions
    - Move the wolf

So conditionals, expressions, observations, and actions are TYPED
- Right now, only types are:
    - DIRECTIONAL conditionals (could also add DISTANCE, for ex.)
    - DIRECTIONAL expressions (Note: they must match the conditional type)
    - COORDINATE observations
- Actions are always the same type though (~"void")


Generating random plans:
---------------------------------------------------------------------
- need to pick a conditional first
- choose 1 of C conditionals UAR -- call this c
- choose expression for c UAR out of expressions
    - if we're at max depth, choose terminal expression
    - ow, choose compound OR terminal expression
    - Choose resulting actions for c
        - If we're at max depth, must choose an Action
        - Otherwise, can choose another Conditional OR an Action


Crossover (t1, t2):
---------------------------------------------------------------------
1. Pick something from t1 to crossover
    - Pick from [Conditional, Expression, Observation, Action] UAR
2. Then pick a specific one
3. Pick the SAME type from t2
4. Switch them and kill ti if depth of ti is now too great

Mutation(t):
---------------------------------------------------------------------
1. Pick something to mutate, UAR as in xover
2. Mutate:
    - Conditional: Substitue another conditional
    - Expression: Substitute another expression
    - Observation: substitute another observation
        - Must match type of observation
    - Action: substitute any other action



Note:
rationale for limiting depth of tree:
shepherd only has so long to make a decision - must react
so, limiting depth of trees = making quick, good (efficient) decision


'''


import plan
from plan import Plan
import random
import run
from run import Run
import copy
import numpy as np


class EvolutionProblem():

    def __init__(self, POPULATION=20, MAXDEPTH=5, P_MUTATE=.1, P_XOVER=.3, GENS=20):

        self.pop = POPULATION       # Population size
        self.max = MAXDEPTH         # Max depth of tree
        self.p_mutate = P_MUTATE    # Mutation probability
        self.p_xover = P_XOVER      # Crossover probability
        self.gens = GENS            # Number of generations



    def run(self):

        population = self.initial_population()

        for i in range(self.gens):

            print "Generation " + str(i)

            # Evaluate fitnesses of all in population:
            fitnesses = np.zeros(self.pop)
            best_fitness = 0.0
            best = population[0]
            for j in range(self.pop):
                specimen = population[j]
                r = Run()
                fit = r.fitness(specimen, "", save=False)
                fitnesses[j] = fit
                if fit > best_fitness:
                    best_fitness = fit
                    best = specimen

            print "Mean fitness = " + str(np.mean(fitnesses))
            print "Best individual: ----------------------------------------------------------------"

            try:
                print str(best)
            except:
                print "FAILED TO PRINT FOR SOME REASON!"

            b_fit = Run().fitness(specimen, "Best_of_gen_"+str(i), save=True)
            print "Fitness of best = " + str(b_fit)
            print "---------------------------------------------------------------------------------"


            children = []
            numAdded= 0

            elites = []

            for j in range(self.pop):
                if fitnesses[j] > 0:
                    children.append(population[j])
                    numAdded += 1
                    elites.append(j)

            while numAdded < self.pop:
                extra = population[random.randint(0, self.pop-1)]
                if random.random() < self.p_mutate:
                    extra = self.mutate(extra)
                children.append(extra)
                numAdded += 1

            for i in range(self.pop/2):
                pos1 = 2*i
                pos2 = 2*i + 1

                if pos1 in elites or pos2 in elites:
                    continue

                if random.random() < self.p_xover:
                    new1, new2 = self.crossover(children[pos1], children[pos2])
                    children[pos1] = new1
                    children[pos2] = new2

            population = children







    '''
    Create initial population of plans:
    '''
    def initial_population(self):
        init_pop = []
        for i in range(self.pop):
            p = self.random_plan()
            init_pop.append(p)
        return init_pop



    '''
    Return a mutation of p1
    '''
    def mutate(self, p1):

        # I <3 python copy() method!! :)
        mutant = copy.deepcopy(p1)

        # Make change:
        for c in mutant.this_conditionals:
            # Possible change observations for each conditional:
            if random.random() < self.p_mutate:
                c.expression.obs1 = random.choice(mutant.Observations)
            if random.random() < self.p_mutate:
                c.expression.obs2 = random.choice(mutant.Observations)
            # Possibly change action for each conditional:
            for possibility in c.options.keys():
                if c.options[possibility].isAction:
                    if random.random() < self.p_mutate:
                        c.options[possibility] = random.choice(mutant.Actions)


        return mutant



    '''
    Crossover plans p1 and p2:
    '''
    def crossover(self, p1, p2):

        mutant1 = copy.deepcopy(p1)
        mutant2 = copy.deepcopy(p2)

        r = random.random()
        # Crossover at a conditional ONLY for now
        if r < 1.000:

            # Also, for now, NO CROSSOVER AT THE ROOT:
            c1 = random.choice(mutant1.this_conditionals)
            c2 = random.choice(mutant2.this_conditionals)

            key1 = random.choice(c1.options.keys())
            key2 = random.choice(c2.options.keys())

            new_c1_child = c2.options[key2]
            new_c2_child = c1.options[key1]

            c1.options[key1] = new_c1_child
            c2.options[key2] = new_c2_child

            # Remove if it was a conditional:
            self.clear(mutant1, new_c2_child)
            self.add(mutant1, new_c1_child)
            self.clear(mutant2, new_c1_child)
            self.add(mutant2, new_c2_child)

        return mutant1, mutant2


    # Clear/Add plan from conditional
    def clear(self, p, c):
        if c.isAction:
            return
        else:
            p.this_conditionals.remove(c)
            for key in c.options.keys():
                self.clear(p, c.options[key])
    def add(self, p, c):
        if c.isAction:
            return
        else:
            p.this_conditionals.append(c)
            for key in c.options.keys():
                self.add(p, c.options[key])

    '''
    Generate a random plan up to max depth:
    '''
    def random_plan(self):
        p = Plan()

        # Choose start condition AND INSTANTIATE IT:
        start_condition = random.choice(p.Conditionals)()
        start_condition.height = 1

        p.action = start_condition
        p.this_conditionals.append(start_condition)


        done = False
        while not done:
            done = True

            # Update conditionals if not marked (marked = already dealt with)
            # and stop when hitting the max depth
            for c in p.this_conditionals:
                if c.marked == 0:
                    done = False
                    c.marked = 1

                    # Add a random expression:
                    expression = random.choice(p.Expressions)()
                    expression.obs1 = random.choice(p.Observations)
                    expression.obs2 = random.choice(p.Observations)
                    expression.depth = c.depth + 1
                    expression.marked = 1
                    expression.set_map(p)
                    c.expression = expression

                    #  Must add terminal actions if we hit the max depth (minus 2)
                    if c.depth == self.max-2:
                        for key in c.options.keys():
                            axn = random.choice(p.Actions)
                            axn.depth = c.depth + 1
                            c.options[key] = axn

                    # Otherwise, we can add either a terminal action OR another conditional:
                    else:
                        for key in c.options.keys():
                            condition_or_action = random.random()
                            if condition_or_action < .5:
                                new_c = random.choice(p.Conditionals)()
                                new_c.depth = c.depth + 1
                                c.options[key] = new_c
                                p.this_conditionals.append(new_c)
                            else:
                                c.options[key] = random.choice(p.Actions)

        return p







def main():

    population = 30
    max = 4
    pm = .5
    px = .5
    gens = 100

    ep = EvolutionProblem(POPULATION=population, MAXDEPTH=max, P_MUTATE=pm, P_XOVER=px, GENS=gens)
    ep.run()




if __name__ == "__main__":
    main()