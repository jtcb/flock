import plan
from plan import *

'''
Custom-made plan for wolves:
'''
def myPlan():

    p = Plan()


    NNN = DirectionAction("N")
    NNE = DirectionAction("N")
    NNS = DirectionAction("N")
    NNW = DirectionAction("N")
    NEN = DirectionAction("W")
    NEE = DirectionAction("W")
    NEW = DirectionAction("W")
    NES = DirectionAction("W")
    NSN = DirectionAction("N")
    NSE = DirectionAction("N")
    NSS = DirectionAction("N")
    NSW = DirectionAction("N")
    NWN = DirectionAction("E")
    NWE = DirectionAction("E")
    NWS = DirectionAction("E")
    NWW = DirectionAction("E")

    ENN = DirectionAction("S")
    ENE = DirectionAction("S")
    ENS = DirectionAction("S")
    ENW = DirectionAction("S")
    EEN = DirectionAction("E")
    EEE = DirectionAction("E")
    EES = DirectionAction("E")
    EEW = DirectionAction("E")
    ESN = DirectionAction("N")
    ESE = DirectionAction("N")
    ESS = DirectionAction("N")
    ESW = DirectionAction("N")
    EWN = DirectionAction("E")
    EWE = DirectionAction("E")
    EWS = DirectionAction("E")
    EWW = DirectionAction("E")

    SNN = DirectionAction("S")
    SNE = DirectionAction("S")
    SNS = DirectionAction("S")
    SNW = DirectionAction("S")
    SEN = DirectionAction("W")
    SEE = DirectionAction("W")
    SES = DirectionAction("W")
    SEW = DirectionAction("W")
    SSN = DirectionAction("S")
    SSE = DirectionAction("S")
    SSS = DirectionAction("S")
    SSW = DirectionAction("S")
    SWN = DirectionAction("E")
    SWE = DirectionAction("E")
    SWS = DirectionAction("E")
    SWW = DirectionAction("E")

    WNN = DirectionAction("S")
    WNE = DirectionAction("S")
    WNS = DirectionAction("S")
    WNW = DirectionAction("S")
    WEN = DirectionAction("W")
    WEE = DirectionAction("W")
    WES = DirectionAction("W")
    WEW = DirectionAction("W")
    WSN = DirectionAction("N")
    WSE = DirectionAction("N")
    WSS = DirectionAction("N")
    WSW = DirectionAction("N")
    WWN = DirectionAction("W")
    WWE = DirectionAction("W")
    WWS = DirectionAction("W")
    WWW = DirectionAction("W")



    NN = DirectionCondition(p.get_closest_wolf_direction, NNN, NNE, NNS, NNW)
    NE = DirectionCondition(p.get_closest_wolf_direction, NEN, NEE, NES, NEW)
    NS = DirectionCondition(p.get_closest_wolf_direction, NSN, NSE, NSS, NSW)
    NW = DirectionCondition(p.get_closest_wolf_direction, NWN, NWE, NWS, NWW)

    EN = DirectionCondition(p.get_closest_wolf_direction, ENN, ENE, ENS, ENW)
    EE = DirectionCondition(p.get_closest_wolf_direction, EEN, EEE, EES, EEW)
    ES = DirectionCondition(p.get_closest_wolf_direction, ESN, ESE, ESS, ESW)
    EW = DirectionCondition(p.get_closest_wolf_direction, EWN, EWE, EWS, EWW)

    SN = DirectionCondition(p.get_closest_wolf_direction, SNN, SNE, SNS, SNW)
    SE = DirectionCondition(p.get_closest_wolf_direction, SEN, SEE, SES, SEW)
    SS = DirectionCondition(p.get_closest_wolf_direction, SSN, SSE, SSS, SSW)
    SW = DirectionCondition(p.get_closest_wolf_direction, SWN, SWE, SWS, SWW)

    WN = DirectionCondition(p.get_closest_wolf_direction, WNN, WNE, WES, WEW)
    WE = DirectionCondition(p.get_closest_wolf_direction, WEN, WEE, WES, WEW)
    WS = DirectionCondition(p.get_closest_wolf_direction, WSN, WSE, WSS, WSW)
    WW = DirectionCondition(p.get_closest_wolf_direction, WWN, WWE, WWS, WWW)




    N = DirectionCondition(p.get_sheep_direction_to_pen, NN, NE, NS, NW)
    E = DirectionCondition(p.get_sheep_direction_to_pen, EN, EE, ES, EW)
    S = DirectionCondition(p.get_sheep_direction_to_pen, SN, SE, SS, SW)
    W = DirectionCondition(p.get_sheep_direction_to_pen, WN, WE, WS, WW)


    c = DirectionCondition(p.get_wolf_direction_to_sheep, N, E, S, W)

    p.action = c

    return p