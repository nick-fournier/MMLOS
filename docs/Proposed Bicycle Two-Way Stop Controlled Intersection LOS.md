# BICYCLE MODE SIGNALIZED INTERSECTION LOS

> - The following bicycle delay calculation methodology is a modified from the Highway Capacity Manual's pedestrian delay calculation for Two-Way Stop Controlled Intersections, Kittelson & Associates, Inc. NCHRP Project 17-87 Appendix C: "Revised Model for Predicting the Pedestrian Delay at Signalized Intersections" and Appendix D: "Revised Model for Predicting the Pedestrian Delay at Uncontrolled Intersections".
> - The reasoning behind these adaptations is that a left turning bicyclist experiences similar delay due to gap acceptance that a pedestrian would when crossing the uncontrolled approaches of a TWSC intersection, and that a two-stage left turn for a bicycle is the same as a pedestrian's two-stage turn when trying to move diagonally across an intersection. Thus, the delay calculations are analogous with modification.

Although bicycles may experience some amount of queuing in locations of particularly high bicycle usage, bicycle delay is largely unaffected by capacity at present bicycle usage levels in the United States. The primary source of bicycle delay, *in addition to signal delay*, is due to left-turn maneuvers at intersections, lane blockage by automobiles, and crossing two-way STOP-controlled (TWSC) intersections. Thus the calculation of intersection delay for bicycles will be performed using the following proposed steps:

1. Determine Critical Headway
2. Estimate Probability of a Delayed Crossing
3. Calculate Average Delay to Wait for Adequate Gap
4. Estimate Delay Reduction due to Yielding Vehicles

<img src="https://raw.github.com/nick-fournier/complete-streets-los/master/docs/Bike-twsc-conflicts.svg?sanitize=true" width=600>

## Step 1: Determine Critical Headway

The critical headway is the time in seconds below which a bicycle will not attempt to cross traffic. Bicyclists use their judgment to determine whether the available headway between conflicting vehicles is long enough for a safe crossing. If the available headway is greater than the critical headway, it is assumed that the bicycle will cross, but if the available headway is less than the critical headway, it is assumed that the bicycle will not cross.

$$
t_{cb} = \frac{L}{S_b} + t_{sb}
$$
where
$t_{lb}$ = critical headway for a single left-turning bicycle (s),
$S_p$ = average bicycle crossing speed (ft/s) (*note: this will be lower than cruising speed*),
$L$ = width of street crossed (ft), and
$t_{sb}$ = bicycle start‐up time and end clearance time (s).

If bicycle platooning is observed in the field, then the spatial distribution of bicyclists should be computed. If no platooning is observed, the spatial distribution is assumed to be 1.
$$
N_{b} = Max\left[\frac{4.0N_{cb}}{W_{bl}}, 1.0\right]
$$
where 
$N_{b}$ = spatial distribution of bicycles (bikes),
$N_{cb}$ = total number of bicycles in the crossing platoon,
$W_{bl}$ = width of bike lane (ft), and
4.0 = default clear effective width used by a single bicycle (ft).


To compute spatial distribution, the analyst must make field observations or estimate the platoon size by using:
$$
N_{cb} = \frac{v_b e^{v_b t_{cb}} + v e^{-vt_{cb}}}{(v_b + v) e^{v_b - v)t_{cb}}}
$$
where
$N_{cb}$ = total number of bicycles in the crossing platoon (bikes),
$v_b$ = bicycle flow rate (bikes/s),
$v$ = vehicular flow rate (veh/s), and
$t_{cb}$ = single bicycle critical headway (s).

Bicycle group critical headway is determined with:
$$
t_{cb,G} = t_{cb}+2(N_b -1)
$$
where
$t_{cb,G}$ = group critical headway (s),
$t_c$ = critical headway for a single bicycle (s), and
$N_b$ = spatial distribution of bicycles (bikes).


### Step 2: Estimate Probability of a Delayed Crossing
On the basis of calculation of the critical headway $t_G$, the probability that a bicycle will not incur any turning delay is equal to the likelihood that a bicycle will encounter a gap greater than or equal to the critical headway immediately upon arrival at the intersection. Assuming random arrivals of vehicles on the major street, and equal distribution of vehicles among all through lanes on the major street, the probability of encountering a headway exceeding the critical headway in any given lane can be estimated by using a Poisson distribution. The likelihood that a gap in a given lane does not exceed the critical headway is thus the complement as shown in the equation. Because traffic is assumed to be distributed independently in each through lane, the equation below shows the probability that a bicycle incurs nonzero delay.
$$
P_b = 1 - e^\frac{-t_{cb,G}v_v}{L}\\
P_d = 1 - (1-P_b)^L
$$
where
$P_b$ = probability of a blocked lane,
$P_d$ = probability of a delayed crossing,
$L$ = number of through lanes crossed,
$t_{cb,G}$ = group critical headway (s), and
$v$ = vehicular flow rate (veh/s).


### Step 3: Calculate Average Delay to Wait for Adequate Gap

Assuming that no motor vehicles yield and the bicycle is forced to wait for an adequate gap, depends on the critical headway, the vehicular flow rate of the subject crossing, and the mean vehicle headway. Thus, bicyclists making the a left turn crossing are subject to the same delay calculation as for pedestrians making a crossing uncontrolled intersection approaches. The average delay per bicycle to wait for an adequate gap is given by:
$$
d_{bg} = \frac{1}{v_v} \left( e^{vt_{cb,G}} - v_v t_{cb,G} -1 \right) 
$$
where
$d_{bg}$ = average bicycle gap delay (s),
$t_{cb,G}$ = bicycle group critical headway (s), and
$v$ = vehicular flow rate (veh/s).


The average delay for any bicycle who is unable to cross immediately upon reaching the intersection (e.g., any pedestrian experiencing nonzero delay) is thus a function of $P_d$ and $d_g$, as shown in:
$$
d_{bgd} = \frac{d_{b}}{P_d}
$$
where
$d_{bgd}$ = average gap delay for bicycles who incur nonzero delay,
$d_{bg}$ = average bicycle gap delay (s), and
$P_d$ = probability of a delayed crossing.


### Step 4: Estimate Delay Reduction due to Yielding Vehicles
Thus far the equations estimate bicycle delay when motorists on the major approaches do not yield to pedestrians. Where motorist yield rates are significantly higher than zero,  pedestrians will experience considerably less delay. Although automobiles are generally ***not*** legally required to stop for bicycles, actual motorist yielding behavior varies considerably. Motorist yield rates are influenced by a range of factors, including roadway geometry, travel speeds, roadway treatments, local culture, and law enforcement practices. When a bicycle arrives at a crossing and finds an inadequate gap, that bicycle is delayed until one of two situations occurs: (a) a gap greater than the critical headway is available, or (b) motor vehicles yield and allow the bicycle to cross. 


It is possible for bicycles to incur less actual delay than $d_g$ because of yielding vehicles. The likelihood of this situation occurring is a function of vehicle volumes, motorist yield rates, and number of through lanes on the major street. Consider a left-turn bicycle waiting for a crossing opportunity at an intersection, with vehicles in each conflicting through lane arriving every $h$ seconds. The headways that the bicycles are assessing during this delay period are always less than the group critical headway. The following equation should be used to compute the appropriate headway $h$ needed by the methodology (Bonneson and McCoy 1993).
$$
h = \frac{\frac{1}{v}-\left(t_{cb,G}+\frac{1}{v}\right)e^{-vt_{cb,G}}}{1 - e^{-vt_{cb,G}}}
$$
where
$h$ = average headway of all headways less than the group critical gap (s);
$t_{cb,G}$ = group critical headway (s), and
$v$ = conflicting vehicular flow rate (veh/s) (combined flows for one-stage crossings; separate flows for two-stage crossings).


On average, a potential yielding event will occur every $h$ seconds, where $P(Y)$ represents the probability of motorists yielding for a given event. As vehicles are assumed to arrive randomly, each potential yielding event is considered to be independent. For any given yielding event, each through lane is in one of two states:


1. Clear—no vehicles are arriving within the critical headway window, or
2. Blocked—a vehicle is arriving within the critical headway window. The bicycle may cross only if vehicles in each blocked lane choose to yield. If not, the bicycle must wait an additional $h$ seconds for the next yielding event. On average, this process will be repeated until the wait exceeds the expected delay required for an adequate gap in traffic ($d_{bgd}$), at which point the average bicycle will receive an adequate gap in traffic and will be able to cross the street without having to depend on yielding motorists. Thus, average bicycle delay can be calculated 

$$
d_{bL1} = \sum\limits^n_{i=0}h(i-0.5) P(Y_i) + \left(P_d - \sum\limits^n_{i=0}P(Y_i) \right) d_{bgd}
$$

where
$d_{bL}$ = average bicycle delay when turning left (s),
$i$ = crossing event ($i = 1$ to $n$),
$h$ = average headway for each through lane,
$P(Y_i)$ = probability that motorists yield to pedestrian on crossing event i, and
$n=Int\left(\frac{1}{e^{-vt_{bc,G}}} \right)$, average number of crossing events before an adequate gap is available.

The first term in the equation represents expected delay from crossings occurring when motorists yield, and the second term represents expected delay from crossings where pedestrians wait for an adequate gap. The equation requires the calculation of $P(Y_i)$. The probabilities $P(Y_i)$ that motorists will yield for a given crossing event are considered below for bicycle crossings of one, two, three, and four through lanes.


#### *One-Lane Crossing*

Under the scenario in which a bicycle crosses one lane, $P(Y_i)$ is found simply. When $i = 1$, $P(Y_i)$ is equal to the probability of a delayed crossing $P_d$ multiplied by the motorist yield rate, $M_y$. For $i = 2$, $P(Y_i)$ is equal to $M_y$ multiplied by the probability that the second yielding event occurs (i.e., that the bicycle did not cross on the first yielding event). For any $i$.
$$
P(Y_i) = P_d M_y (1-M_y)^{i-1}
$$
where
$M_y$ = motorist yield rate (decimal), and
$i$ = crossing event ($i$ = 1 to $n$).


#### *Two-Lane Crossing*


For a two‐lane (one in each direction) bicycle left turn crossing, $P(Y_i)$ requires either (a) motorists in both lanes to yield simultaneously if both lanes are blocked, or (b) a single motorist to yield if only one lane is blocked. Because these cases are mutually exclusive, where $i = 1$, $P(Y_i)$ is equal to:
$$
P(Y_i) = \left[ P_d - \sum\limits^{i-1}_{j=0}P(Y_j) \right]\left[ \frac{2P_b(1-P_b)M_y + P_b^2 M_y^2 }{P_d} \right]
$$
where $P(Y_0)=0$


#### *Three-Lane Crossing*


A three‐lane crossing follows the same principles as a two‐lane crossing. The calculation for P(Yi) with three lanes is:
$$
P(Y_i) = \left[ P_d - \sum\limits^{i-1}_{j=0}P(Y_j) \right]\left[ \frac{P_b^3 M_y^3 + 3P_b^2(1-P_b)M_y^2 + 3P_b(1-P_b)^2 M_y}{P_d} \right]
$$
where $P(Y_0)$ = 0.


#### *Four-Lane Crossing*


A four‐lane crossing follows the same principles as above. The calculation for $P(Y_i)$ with four-lanes is:
$$
P(Y_i) = \left[ P_d - \sum\limits^{i-1}_{j=0}P(Y_j) \right]\times\left[ \frac{P_b^4 M_y^4 + 4P_b^3(1-P_b)M_y^3 + 6P_b^2(1-P_b)^2 M_y^2 + 4P_b(1-P_b^3)M_y}{P_d} \right]
$$
where $P(Y_0)$ = 0.
