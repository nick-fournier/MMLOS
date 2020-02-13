# BICYCLE MODE 

> - The following bicycle delay calculation methodology is a modified from the HCM's two-way STOP-controlled intersection delay and from the Kittelson & Associates, Inc. NCHRP Project 17-876 Working Paper "Model for Predicting the Pedestrian Delay at Signalized Intersections". 
> - The reasoning being that a left turning bicyclist experiences similar delay due to gap acceptance that a pedestrian would when crossing the uncontrolled approaches of a TWSC intersection, and that a two-stage left turn for a bicycle is the same as a pedestrian's two-stage turn when trying to move  diagonally across an intersection. Thus, the delay calculations are analogous with modification.

Although bicycles may experience some amount of queuing in locations of particularly high bicycle usage, bicycle delay is largely unaffected by capacity at present bicycle usage levels in the United States. The primary source of bicycle delay, *in addition to signal delay*, is due to left-turn maneuvers at intersections, lane blockage by automobiles, and crossing two-way STOP-controlled (TWSC) intersections. Thus the calculation of intersection delay for bicycles will be performed by:

1. Calculate Signal Delay
2. Determine appropriate left-turn maneuver, or proportion of each.
3. Calculate left-turn delay.
4. Calculate average delay from signal delay and left-turn delay.
5. Determine Bicycle LOS Score for Intersection
6. Determine LOS.

If no control, calculate using TWSC intersection delay. If stop controlled, intersection delay is 0.

## Step 1: Determine Signal Delay

This step describes a procedure for evaluating the performance of one intersection approach. It is repeated for each approach of interest. Bicycle delay can be calculated only for intersection approaches that have an on‐street bicycle lane or a shoulder that can be used by bicyclists as a bicycle lane. Bicyclists who
share a lane with automobile traffic will incur the same delay as the automobiles.

### *A. Compute Bicycle Lane Capacity*

A wide range of capacities and saturation flow rates have been reported by many countries for bicycle lanes at intersections. Research indicates that the base saturation flow rate may be as high as 2,600 bicycles/h (31). However, few intersections provide base conditions for bicyclists, and current information is insufficient to calibrate a series of appropriate saturation flow adjustment factors. Until such factors are developed, it is recommended that a saturation flow rate of 2,000 bicycles/h be used as an average value achievable at most intersections. A saturation flow rate of 2,000 bicycles/h assumes that right‐turning motor vehicles yield the right‐of‐way to through bicyclists. Where aggressive right turning
traffic exists, 2,000 bicycles/h may not be achievable. Local observations to determine a saturation flow rate are recommended in such cases. The capacity of the bicycle lane at a signalized intersection may be computed with Equation $\ref{eq:cb}$.
$$
c_b = s_b \frac{g_b}{C} \label{eq:cb}
$$
where
$c_b$ = capacity of the bicycle lane (bicycles/h),
$s_b$ = saturation flow rate of the bicycle lane = 2,000 (bicycles/h),
$g_b$ = effective green time for the bicycle lane (s), and
$C$ = cycle length (s).

The effective green time for the bicycle lane can be assumed to equal that for
the adjacent motor‐vehicle traffic stream that is served concurrently with the
subject bicycle lane (i.e., $g_b = D_p – l_1 – l_2$).

### *B. Compute Bicycle Delay*

Bicycle delay is computed with Equation $\ref{eq:db}$.
$$
d_b = \frac{0.5(1-\frac{g_b}{C})^2}{1 - min\left[\frac{v_{bic}}{c_b},1.0\right]\frac{g_b}{C}} \label{eq:db}
$$
where $d_b$ is bicycle delay (s/bicycle), $v_{bic}$ is bicycle flow rate (bicycles/h), and other variables are as previously defined.

This delay equation is based on the assumption that there is no bicycle incremental delay or initial queue delay. Bicyclists will not normally tolerate an oversaturated condition and will select other routes or ignore traffic regulations to avoid the associated delays.

At most signalized intersections, the only delay to through bicycles is caused by the signal, because bicycles have the right‐of‐way over right‐turning vehicles during the green indication. Bicycle delay could be longer than that obtained from Equation $\ref{eq:db}$ when (a) bicycles are forced to weave with right‐turning traffic during the green indication, or (b) drivers do not acknowledge the bicycle right‐of‐way because of high flows of right‐turning vehicles. 

The delay obtained from Equation $\ref{eq:db}$ can be used to make some judgment about intersection performance. Bicyclists tend to have about the same tolerance for delay as pedestrians. They tend to become impatient when they experience a delay in excess of 30 s/bicycle. In contrast, they are very likely to comply with the signal indication if their expected delay is less than 10 s/bicycle.



## Step 2: Determine Intersection Delay

The main issue to determine the appropriate turning maneuver. While a small percentage of "strong and fearless" [cite] bicyclists may feel comfortable performing a single-phased left turn even at large intersections, this does not represent the majority of "concerned" bicyclists. Most bicyclists may perform a two-phased  turn at large or busy intersections, some may even transition to a pedestrian to make the crossing, but this transition will incur significant additional delay and inconvenience. 

When bicycles make a two-phased left, bicycle delay should be estimated separately for each stage of the crossing by using the procedures described in Steps 2 to 6. To determine bicycle LOS, the bicycle delay for each stage should be summed in addition to the signal delay to establish the average bicycle delay associated with the entire crossing. This service measure is used to determine bicycle LOS for a signalized intersection with two‐stage crossings.

### A: Identify Left Turn Maneuvers

At signalized intersections, bicycles typically perform a left turn using one of two maneuvers.

- **Single-phased permissive left using acceptable gaps in traffic.**
  Calculation of delay in this case is analogous to a pedestrian crossing at a TWSC intersection where delay is encountered when waiting for an acceptable gap in each traffic lane crossed. These maneuvers are typically performed at most intersections with small or moderate traffic volumes. Even upstream mixing lanes or centerline left turn lanes (e.g., Scott St. between Fell St. and Oak St. in San Francisco) still require a bicyclist to cross a lane of traffic before making a permissive left. Mitigation of this includes advanced bicycle/pedestrian signal phasing or two-phased left turn maneuvers.
- **Two-phased maneuver where the bicycle moves parallel with traffic in each signal phase.**
  Delay calculation for this maneuver is analogous to two-phased (diagonal) pedestrian crossing. These maneuvers are typically performed at larger intersections with high volume and/or multiple traffic lanes that makes permissive left turns difficult or impossible to perform safely. Infrastructure such as "left-turn queue boxes" and "protected intersections" attempt to encourage this maneuver. 

### B: Determine Single-Phase Left-Turn Delay



### C: Determine Two-Phase Left-Turn Delay

#### i. Determine Critical Headway

The procedure for estimating the critical headway is similar to that described Critical headway for pedestrians. for automobiles. The critical headway is the time in seconds below which a pedestrian will not attempt to begin crossing the street. Pedestrians use their judgment to determine whether the available headway between conflicting vehicles is long enough for a safe crossing. If the available headway is greater than the critical headway, it is assumed that the pedestrian will cross, but if the available headway is less than the critical headway, it is assumed that the pedestrian will not cross.

$$
t_{cb} = \frac{L}{S_b} + t_{sb}\label{eq:tc}
$$

where
$t_{lb}$ = critical headway for a single left-turning bicycle (s),
$S_p$ = average bicycle crossing speed (ft/s) (*note: this will be lower than cruising speed*),
$L$ = width of street crossed (ft), and
$t_{sb}$ = bicycle start‐up time and end clearance time (s).

If pedestrian platooning is observed in the field, then the spatial distribution of pedestrians should be computed with Equation $\ref{eq:Nlb}$. If no platooning is observed, the spatial distribution of pedestrians is assumed to be 1.
$$
N_{b} = Int\left[\frac{8.0(N_{cb}-1)}{W_{c}}\right] \label{eq:Nlb}
$$
where 
$N_{b}$ = spatial distribution of bicycles (bikes);
$N_{cb}$ = total number of bicycles in the crossing platoon, from Equation $\ref{eq:Nb}$ (bikes);
$W_c$ = width of intersection for bicycles (ft); and
4.0 = default clear effective width used by a single bicycle (ft).

To compute spatial distribution, the analyst must make field observations or estimate the platoon size by using Equation $\ref{eq:Nb}$:
$$
N_{cb} = \frac{v_b e^{v_b t_{cb}} + v e^{-vt_{cb}}}{(v_b + v) e^{v_b - v)t_{cb}}} \label{eq:Nb}
$$
where
$N_{cb}$ = total number of bicycles in the crossing platoon (bikes),
$v_b$ = bicycle flow rate (bikes/s),
$v$ = vehicular flow rate (veh/s), and
$t_{cb}$ = single bicycle critical headway (s).

Bicycle group critical headway is determined with Equation $\ref{eq:tcbG}$:
$$
t_{cb,G} = t_{cb}+2(N_b -1) \label{eq:tcbG}
$$
where
$t_{cb,G}$ = group critical headway (s),
$t_c$ = critical headway for a single bicycle (s), and
$N_b$ = spatial distribution of bicycles (bikes).

#### ii. Estimate Probability of a Delayed Crossing

On the basis of calculation of the critical headway $t_G$, the probability that a bicycle will not incur any turning delay is equal to the likelihood that a bicycle will encounter a gap greater than or equal to the critical headway immediately upon arrival at the intersection. Assuming random arrivals of vehicles on the major street, and equal distribution of vehicles among all through lanes on the major street, the probability of encountering a headway exceeding the critical headway in any given lane can be estimated by using a Poisson distribution. The likelihood that a gap in a given lane does not exceed the critical headway is thus the complement as shown in Equation $\ref{eq:Pb}$. Because traffic is assumed to be distributed independently in each through lane, Equation $\ref{eq:Pd}$ shows the probability that a bicycle incurs nonzero delay at a TWSC crossing.
$$
\begin{align}
P_b & = 1 - e^\frac{-t_{cb,G}v_v}{L}\label{eq:Pb}\\
P_d & = 1 - (1-P_b)^L\label{eq:Pd}
\end{align}
$$
where
$P_b$ = probability of a blocked lane,
$P_d$ = probability of a delayed crossing,
$L$ = number of through lanes crossed,
$t_{cb,G}$ = group critical headway (s), and
$v$ = vehicular flow rate (veh/s).

#### iii: Calculate Average Delay to Wait for Adequate Gap

Research indicates that average delay to pedestrians at unsignalized crossings, assuming that no motor vehicles yield and the pedestrian is forced to wait for an adequate gap, depends on the critical headway [cite], the vehicular flow rate of the subject crossing, and the mean vehicle headway. Thus, bicyclists making the same unsignalized crossing are subject to the same delay calculation as for pedestrians. The average delay per bicycle to wait for an adequate gap is given by Equation $\ref{eq:dbg}$.
$$
d_{bg} = \frac{1}{v_v} \left( e^{vt_{cb,G}} - v_v t_{cb,G} \right) \label{eq:dbg}
$$
where
$d_bg$ = average bicycle gap delay (s),
$t_{cb,G}$ = bicycle group critical headway (s), and
$v$ = vehicular flow rate (veh/s).

The average delay for any pedestrian who is unable to cross immediately upon reaching the intersection (e.g., any pedestrian experiencing nonzero delay) is thus a function of Pd and dg, as shown in Equation $\ref{eq:d_bgd}$:
$$
d_{bgd} = \frac{d_{b}}{P_d} \label{eq:d_bgd}
$$
where
$d_{bgd}$ = average gap delay for pedestrians who incur nonzero delay,
$d_{bg}$ = average pedestrian gap delay (s), and
$P_d$ = probability of a delayed crossing.

#### iv. Estimate Delay Reduction due to Yielding Vehicles

Although automobiles are generally ***not*** legally required to stop for bicycles, the delay calculation is dominated by Equation $\ref{eq:dbg}$, but vehicles do yield to bicycles nonetheless and should not only be included in the calculation, but encouraged. 

When a bicycle arrives at a crossing and finds an inadequate gap, that pedestrian is delayed until one of two situations occurs: (a) a gap greater than the critical headway is available, or (b) motor vehicles yield and allow the pedestrian to cross. Equation 19‐75 estimates pedestrian delay when motorists on the major approaches do not yield to pedestrians. Where motorist yield rates are significantly higher than zero,  pedestrians will experience considerably less delay than that estimated by Equation 19‐75.

In the United States, motorists are legally required to yield to pedestrians, under most circumstances, in both marked and unmarked crosswalks. However, actual motorist yielding behavior varies considerably. Motorist yield rates are influenced by a range of factors, including roadway geometry, travel speeds, pedestrian crossing treatments, local culture, and law enforcement practices.

Research (11, 12) provides information on motorist responses to typical pedestrian crossing treatments, as shown in Exhibit 19‐17. The exhibit shows results from two separate data collection methods. Staged data were collected with pedestrians trained by the research team to maintain consistent positioning, stance, and aggressiveness in crossing attempts. Unstaged data were collected through video recordings of the general population. The values shown in Exhibit 19‐17 are based on a limited number of sites and do not encompass the full range of available crossing treatments. As always, practitioners should supplement these values with local knowledge and engineering judgment.

It is possible for pedestrians to incur less actual delay than dg because of yielding vehicles. The likelihood of this situation occurring is a function of vehicle volumes, motorist yield rates, and number of through lanes on the major street. Consider a pedestrian waiting for a crossing opportunity at a TWSC intersection, with vehicles in each conflicting through lane arriving every h seconds. On average, a potential yielding event will occur every h seconds, where P(Y) represents the probability of motorists yielding for a given event. As vehicles are assumed to arrive randomly, each potential yielding event is considered to be independent. For any given yielding event, each through lane is in one of two states:

1. Clear—no vehicles are arriving within the critical headway window, or
2. Blocked—a vehicle is arriving within the critical headway window. The pedestrian may cross only if vehicles in each blocked lane choose to yield. If not, the pedestrian must wait an additional h seconds for the next yielding event. On average, this process will be repeated until the wait exceeds the expected delay required for an adequate gap in traffic (dgd), at which point the average pedestrian will receive an adequate gap in traffic and will be able to cross the street without having to depend on yielding motorists. Thus, average pedestrian delay can be calculated with Equation 19‐77, where the first term in the equation represents expected delay from crossings occurring when motorists yield, and the second term represents expected delay from crossings where pedestrians wait for an adequate gap.

$$
d_b = \sum\limits^n_{i=1}h(i-0.5) P(Y_i) + \left(P_d - \sum\limits^n_{i=1}P(Y_i) \right) d_{bgd}
$$

where
$d_b$ = average pedestrian delay (s),
$i$ = crossing event (i = 1 to n),
$h$ = average headway for each through lane,
$P(Y_i)$ = probability that motorists yield to pedestrian on crossing event i, and
$n$ = $Int(\frac{d_{bgd}}{h})$, average number of crossing events before an adequate gap is
available.

Equation 19‐77 requires the calculation of $P(Y_i)$. The probabilities $P(Y_i)$ that motorists will yield for a given crossing event are considered below for pedestrian crossings of one, two, three, and four through lanes.

##### *One-Lane Crossing*

Under the scenario in which a pedestrian crosses one through lane, P(Yi) is found simply. When i = 1, P(Yi) is equal to the probability of a delayed crossing Pd multiplied by the motorist yield rate, My. For i = 2, P(Yi) is equal to My multiplied by the probability that the second yielding event occurs (i.e., that the pedestrian did not cross on the first yielding event), Pd *(1 – My). Equation 19‐78 gives P(Yi) for any i.
$$
P(Y_i) = P_d M_y (1-M_y)^{i-1}
$$
where
$M_y$ = motorist yield rate (decimal), and
$i$ = crossing event ($i$ = 1 to $n$).

##### *Two-Lane Crossing*
<<<<<<< HEAD
=======

>>>>>>> 84e107513e912f186e71d21c7c1789c21f2a612f
For a two‐lane pedestrian crossing at a TWSC intersection, P(Yi) requires either (a) motorists in both lanes to yield simultaneously if both lanes are blocked, or (b) a single motorist to yield if only one lane is blocked. Because these cases are mutually exclusive, where i = 1, P(Yi) is equal to Equation 19‐79:
$$
P(Y_i) = \left[ P_d - \sum\limits^{i-1}_{j=0}P(Y_j) \right]\left[ \frac{2P_b(1-P_b)M_y + P_b^2 M_y^2 }{P_d} \right]
$$
where $P(Y_0)=0$

##### *Three-Lane Crossing*

A three‐lane crossing follows the same principles as a two‐lane crossing. Equation 19‐81 shows the calculation for P(Yi):
$$
P(Y_i) = \left[ P_d - \sum\limits^{i-1}_{j=0}P(Y_j) \right]\left[ \frac{P_b^3 M_y^3 + 3P_b^2(1-P_b)M_y^2 + 3P_b(1-P_b)^2 M_y}{P_d} \right]
$$
where $P(Y_0)$ = 0.

##### *Four-Lane Crossing*
<<<<<<< HEAD
=======

>>>>>>> 84e107513e912f186e71d21c7c1789c21f2a612f
A four‐lane crossing follows the same principles as above. Equation 19‐82 shows the calculation for $P(Y_i)$:
$$
P(Y_i) = \left[ P_d - \sum\limits^{i-1}_{j=0}P(Y_j) \right]\left[ \frac{P_b^4 M_y^4 + 4P_b^3(1-P_b)M_y^3 + 6P_b^2(1-P_b)^2 M_y^2 + 4P_b(1-P_b^3)M_y}{P_d} \right]
$$
where $P(Y_0)$ = 0.

#### v. Calculate Average Pedestrian Delay and Determine LOS

<<<<<<< HEAD
The delay experienced by a pedestrian is the service measure. Exhibit 19‐2 lists LOS criteria for pedestrians at TWSC intersections based on pedestrian delay. Pedestrian delay at TWSC intersections with two‐stage crossings is equal to the sum of the delay for each stage of the crossing.
=======
The delay experienced by a pedestrian is the service measure. Exhibit 19‐2 lists LOS criteria for pedestrians at TWSC intersections based on pedestrian delay. Pedestrian delay at TWSC intersections with two‐stage crossings is equal to the sum of the delay for each stage of the crossing.
>>>>>>> 84e107513e912f186e71d21c7c1789c21f2a612f
