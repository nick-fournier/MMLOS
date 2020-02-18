# BICYCLE MODE SIGNALIZED INTERSECTION LOS

> - The following bicycle delay calculation methodology is a modified from the HCM's two-way STOP-controlled intersection delay and from the Kittelson & Associates, Inc. NCHRP Project 17-876 Working Paper "Model for Predicting the Pedestrian Delay at Signalized Intersections". 
> - The reasoning being that a left turning bicyclist experiences similar delay due to gap acceptance that a pedestrian would when crossing the uncontrolled approaches of a TWSC intersection, and that a two-stage left turn for a bicycle is the same as a pedestrian's two-stage turn when trying to move  diagonally across an intersection. Thus, the delay calculations are analogous with modification.

Although bicycles may experience some amount of queuing in locations of particularly high bicycle usage, bicycle delay is largely unaffected by capacity at present bicycle usage levels in the United States. The primary source of bicycle delay, *in addition to signal delay*, is due to left-turn maneuvers at intersections, lane blockage by automobiles, and crossing two-way STOP-controlled (TWSC) intersections. Thus the calculation of intersection delay for bicycles will be performed using the following proposed steps:


1. Determine Signal Delay for Through Maneuvers


2. Determine Left Turn Maneuver Delay


   a. Identify left turn maneuver


   b. Determine single-phase turn delay


   c. Determine two-stage turn delay


   ​	i.  Determine Critical Headway


   ​	ii. Estimate Probability of a Delayed Crossing


   ​	iii. Calculate Average Delay to Wait for Adequate Gap


   ​	iv. Estimate Delay Reduction due to Yielding Vehicles


   ​	v. Calculate Average Pedestrian Delay and Determine LOS


3. Determine average intersection Delay and Bicycle LOS Score for Intersection


*If no control, use TWSC intersection delay. If stop controlled, intersection delay is 0.


## Step 1: Determine Signal Delay for Through Maneuvers


This step describes a procedure for evaluating the performance of one intersection approach. It is repeated for each approach of interest. Bicycle delay can be calculated only for intersection approaches that have an on‐street bicycle lane or a shoulder that can be used by bicyclists as a bicycle lane. Bicyclists who share a lane with automobile traffic will incur the same delay as the automobiles.


### *A. Compute Bicycle Lane Capacity*


A wide range of capacities and saturation flow rates have been reported by many countries for bicycle lanes at intersections. Research indicates that the base saturation flow rate may be as high as 2,600 bicycles/h (31). However, few intersections provide base conditions for bicyclists, and current information is insufficient to calibrate a series of appropriate saturation flow adjustment factors. Until such factors are developed, it is recommended that a saturation flow rate of 2,000 bicycles/h be used as an average value achievable at most intersections. A saturation flow rate of 2,000 bicycles/h assumes that right‐turning motor vehicles yield the right‐of‐way to through bicyclists. Where aggressive right turning
traffic exists, 2,000 bicycles/h may not be achievable. Local observations to determine a saturation flow rate are recommended in such cases. The capacity of the bicycle lane at a signalized intersection may be computed with:
$$
c_b = s_b \frac{g_b}{C}
$$
where
$c_b$ = capacity of the bicycle lane (bicycles/h),
$s_b$ = saturation flow rate of the bicycle lane = 2,000 (bicycles/h),
$g_b$ = effective green time for the bicycle lane (s), and
$C$ = cycle length (s).


The effective green time for the bicycle lane can be assumed to equal that for the adjacent motor‐vehicle traffic stream that is served concurrently with the subject bicycle lane (i.e., $g_b = D_p – l_1 – l_2$).


### *B. Compute Bicycle Delay*


Signalized intersection bicycle delay is computed with:
$$
d_{bS} = \frac{0.5(1-\frac{g_b}{C})^2}{1 - min\left[\frac{v_{bic}}{c_b},1.0\right]\frac{g_b}{C}}
$$
where
$d_{bS}$ is bicycle delay (s/bicycle) from the signal itself,
$v_{bic}$ is bicycle flow rate (bicycles/h), and other variables are as previously defined.

This delay equation is based on the assumption that there is no bicycle incremental delay or initial queue delay. Bicyclists will not normally tolerate an oversaturated condition and will select other routes or ignore traffic regulations to avoid the associated delays.


At most signalized intersections, the only delay to through bicycles is caused by the signal, because bicycles have the right‐of‐way over right‐turning vehicles during the green indication. Bicycle delay could be longer than that obtained from Equation when (a) bicycles are forced to weave with right‐turning traffic during the green indication, or (b) drivers do not acknowledge the bicycle right‐of‐way because of high flows of right‐turning vehicles. 


The delay obtained from Equation  can be used to make some judgment about intersection performance. Bicyclists tend to have about the same tolerance for delay as pedestrians. They tend to become impatient when they experience a delay in excess of 30 s/bicycle. In contrast, they are very likely to comply with the signal indication if their expected delay is less than 10 s/bicycle.






## Step 2: Determine Left-Turn Maneuver Delay


The main issue to determine the appropriate turning maneuver. While a small percentage of "strong and fearless" (Dill and McNeil, 2013) bicyclists may feel comfortable performing a single-phased left turn even at large intersections, this does not represent the majority of "concerned" bicyclists. Most bicyclists however, may perform a two-staged turn at large or busy intersections. Some bicyclists may even transition to a pedestrian to make uncomfortable crossings, but this transition will incur significant additional delay and inconvenience. 


When bicycles make a two-staged left, bicycle delay should be estimated separately for each stage of the crossing by using the procedures described in Step 2.C. To determine bicycle LOS, the bicycle delay for each stage should be summed in addition to the signal delay to establish the average bicycle delay associated with the entire crossing. This service measure is used to determine bicycle LOS for a signalized intersection with two‐stage crossings.


### A: Identify Left Turn Maneuvers


At signalized intersections, bicycles typically perform a left turn using one of two maneuvers.

<img src="https://raw.github.com/nick-fournier/complete-streets-los/master/docs/Bike-left-turn-conflicts.svg?sanitize=true" width=600>

- **Single-phased permissive left using acceptable gaps in traffic.**
  Calculation of delay in this case is analogous to a pedestrian crossing at a TWSC intersection where delay is encountered when waiting for an acceptable gap in each traffic lane crossed. These maneuvers are typically performed at most intersections with small or moderate traffic volumes. Even upstream mixing lanes or centerline left turn lanes (e.g., Scott St. between Fell St. and Oak St. in San Francisco) still require a bicyclist to cross a lane of traffic before making a permissive left. Mitigation of this includes advanced bicycle/pedestrian signal phasing or two-staged left turn maneuvers.
- **two-staged maneuver where the bicycle moves parallel with traffic in each signal phase.**
  Delay calculation for this maneuver is analogous to two-staged (diagonal) pedestrian crossing. These maneuvers are typically performed at larger intersections with high volume and/or multiple traffic lanes that makes permissive left turns difficult or impossible to perform safely. Infrastructure such as "left-turn queue boxes" and "protected intersections" attempt to encourage this maneuver. 


### B: Determine Single-Phase Left-Turn Delay


#### i. Determine Critical Headway


The procedure for estimating the critical headway is similar to that described Critical headway for pedestrians. for automobiles. The critical headway is the time in seconds below which a pedestrian will not attempt to begin crossing the street. Pedestrians use their judgment to determine whether the available headway between conflicting vehicles is long enough for a safe crossing. If the available headway is greater than the critical headway, it is assumed that the pedestrian will cross, but if the available headway is less than the critical headway, it is assumed that the pedestrian will not cross.


$$
t_{cb} = \frac{L}{S_b} + t_{sb}
$$


where
$t_{lb}$ = critical headway for a single left-turning bicycle (s),
$S_p$ = average bicycle crossing speed (ft/s) (*note: this will be lower than cruising speed*),
$L$ = width of street crossed (ft), and
$t_{sb}$ = bicycle start‐up time and end clearance time (s).


If pedestrian platooning is observed in the field, then the spatial distribution of pedestrians should be computed. If no platooning is observed, the spatial distribution of pedestrians is assumed to be 1.
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


#### ii. Estimate Probability of a Delayed Crossing


On the basis of calculation of the critical headway $t_G$, the probability that a bicycle will not incur any turning delay is equal to the likelihood that a bicycle will encounter a gap greater than or equal to the critical headway immediately upon arrival at the intersection. Assuming random arrivals of vehicles on the major street, and equal distribution of vehicles among all through lanes on the major street, the probability of encountering a headway exceeding the critical headway in any given lane can be estimated by using a Poisson distribution. The likelihood that a gap in a given lane does not exceed the critical headway is thus the complement as shown in the equation. Because traffic is assumed to be distributed independently in each through lane, the equation below shows the probability that a bicycle incurs nonzero delay at a TWSC crossing.
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


#### iii: Calculate Average Delay to Wait for Adequate Gap


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


#### iv. Estimate Delay Reduction due to Yielding Vehicles
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
2. Blocked—a vehicle is arriving within the critical headway window. The bicycle may cross only if vehicles in each blocked lane choose to yield. If not, the bicycle must wait an additional $h$ seconds for the next yielding event. On average, this process will be repeated until the wait exceeds the expected delay required for an adequate gap in traffic ($d_{bgd}$), at which point the average bicycle will receive an adequate gap in traffic and will be able to cross the street without having to depend on yielding motorists. Thus, average bicycle delay can be calculated with Equation $\ref{eq:dbtwostage}$, where the first term in the equation represents expected delay from crossings occurring when motorists yield, and the second term represents expected delay from crossings where pedestrians wait for an adequate gap.


$$
d_{bL1} = \sum\limits^n_{i=0}h(i-0.5) P(Y_i) + \left(P_d - \sum\limits^n_{i=0}P(Y_i) \right) d_{bgd}
$$


where
$d_{bL}$ = average bicycle delay when turning left (s),
$i$ = crossing event ($i = 1$ to $n$),
$h$ = average headway for each through lane,
$P(Y_i)$ = probability that motorists yield to pedestrian on crossing event i, and
$n=Int\left(\frac{1}{e^{-vt_{bc,G}}} \right)$, average number of crossing events before an adequate gap is
available.


Equation $\ref{eq:dbtwostage}$ requires the calculation of $P(Y_i)$. The probabilities $P(Y_i)$ that motorists will yield for a given crossing event are considered below for bicycle crossings of one, two, three, and four through lanes.


##### *One-Lane Crossing*


Under the scenario in which a bicycle crosses one lane, $P(Y_i)$ is found simply. When $i = 1$, $P(Y_i)$ is equal to the probability of a delayed crossing $P_d$ multiplied by the motorist yield rate, $M_y$. For $i = 2$, $P(Y_i)$ is equal to $M_y$ multiplied by the probability that the second yielding event occurs (i.e., that the bicycle did not cross on the first yielding event). For any $i$.
$$
P(Y_i) = P_d M_y (1-M_y)^{i-1}
$$
where
$M_y$ = motorist yield rate (decimal), and
$i$ = crossing event ($i$ = 1 to $n$).


##### *Two-Lane Crossing*


For a two‐lane (one in each direction) bicycle left turn crossing, $P(Y_i)$ requires either (a) motorists in both lanes to yield simultaneously if both lanes are blocked, or (b) a single motorist to yield if only one lane is blocked. Because these cases are mutually exclusive, where $i = 1$, $P(Y_i)$ is equal to:
$$
P(Y_i) = \left[ P_d - \sum\limits^{i-1}_{j=0}P(Y_j) \right]\left[ \frac{2P_b(1-P_b)M_y + P_b^2 M_y^2 }{P_d} \right]
$$
where $P(Y_0)=0$


##### *Three-Lane Crossing*


A three‐lane crossing follows the same principles as a two‐lane crossing. The calculation for P(Yi) with three lanes is:
$$
P(Y_i) = \left[ P_d - \sum\limits^{i-1}_{j=0}P(Y_j) \right]\left[ \frac{P_b^3 M_y^3 + 3P_b^2(1-P_b)M_y^2 + 3P_b(1-P_b)^2 M_y}{P_d} \right]
$$
where $P(Y_0)$ = 0.


##### *Four-Lane Crossing*


A four‐lane crossing follows the same principles as above. The calculation for $P(Y_i)$ with four-lanes is:
$$
P(Y_i) = \left[ P_d - \sum\limits^{i-1}_{j=0}P(Y_j) \right]\left[ \frac{P_b^4 M_y^4 + 4P_b^3(1-P_b)M_y^3 + 6P_b^2(1-P_b)^2 M_y^2 + 4P_b(1-P_b^3)M_y}{P_d} \right]
$$
where $P(Y_0)$ = 0.

### C: Determine two-stage Left Turn Delay

For a two-stage left turns, two situations can occur:
- a bicycle arrives at the first stage during a green phase, only stopping at the corner to wait the remaining  
for the phase to finish and the signal to turn green.
- a bicycle arrives at the first stage during a red phase and must wait for the first phase to finish, plus the second 
for the cycle to finish and turn green before proceeding to the second leg, to then wait a full 

Two-stage left turns may be computed as



$$
d_{bL2} = [ d_{1,R1}P_{R1} + d_{1,G1}(1-P_{R1}) ]_1 + [ d_{2,R1}P_{R1} + d_{2,G1}(1-P_{R1})]_2
$$

where
$d_{bL2}$ = bicycle delay for two-stage left turn (s/bike),
$d_{1,R1}$ = delay for stage 1, given arrival is during a red phase (s/bike),
$d_{1,G1}$ = delay for stage 1, given arrival is during a green phase (s/bike),
$d_{2,R1}$ = delay on median for stage 2, given arrival is during a Don’t Walk indication at corner (s/p),

$d_{2,G1}$ = delay on median for stage 2, given arrival is during the Walk indication at corner (s/p), and
$P_{R1}$ = proportion of arrivals during a Don’t Walk indication at corner (s/p).



## Step. 3: Calculate Average Bicycle Delay and Determine LOS

$$
d_b = d_{bS} + P_L [(1-P_{L2})d_{bL1} + P_{L2}d_{bL2})] \frac{g_c}{C}
$$
where
$d_b$ = overall average bicycle delay (s/bike),
$d_{bS}$ = bicycle delay from signal (s/bike),
$d_{bL1}$ = bicycle delay for one-stage left turns (s/bike),
$d_{bL2}$ = bicycle delay for two-stage left turns (s/bike),
$P_L$ = the proportion of left turning bicycles (decimal),
$P_{L2}$ = the proportion of left turning bicycles using two-stage maneuver (decimal),
$g_c$ = the effective green time of the bicycle lane (s),
$C$ = cycle time (s).




The delay experienced by a pedestrian is the service measure. Exhibit 19‐2 lists LOS criteria for pedestrians at TWSC intersections based on pedestrian delay. Pedestrian delay at TWSC intersections with two‐stage crossings is equal to the sum of the delay for each stage of the crossing.
<!--stackedit_data:
eyJoaXN0b3J5IjpbLTEyMjgzODQ4MzgsLTE3OTk3NTY3MDYsLT
Q1NzI1NDIxMCw0NjgxNjA5NSwtNTcxMzM1NTk3LC0xMzQ4OTQ0
NzM1LC01OTc1NDU2NzksMjAyNTEyNTYzMywyMTk0MjQxNjgsLT
UxNDEzOTA5NCwxODc4NTAxMDUsMjE1MDQ1ODE4LC0xMjk3MDEz
MzcsLTE3NTI1NjE5NjUsLTE0MTQxODM1MzgsMTM1OTY4NjQzMi
wtMTU2MzgwNzI4Ml19
-->