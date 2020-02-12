<img src="https://render.githubusercontent.com/render/math?math=e^{i \pi} = -1">

# BICYCLE MODE 

> - The following bicycle delay calculation for non-STOP controlled intersections has been modified from the Highway Capacity Manual's pedestrian methodology for two-way stop-controlled intersections. 
> - The reasoning being that a left turning bicyclist experiences similar delay due to gap acceptance that a pedestrian would when crossing the uncontrolled approaches of a TWSC intersection.

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
N_{lb} = Int\left[\frac{8.0(N_{lb}-1)}{W_{lb}}\right] \label{eq:Nlb}
$$
where 
$N_{lb}$ = spatial distribution of pedestrians (ped);
$N_b$ = total number of pedestrians in the crossing platoon, from Equation $\ref{eq:Nb}$ (bikes);
$W_c$ = bike lane width (ft); and
8.0 = default clear effective width used by a single pedestrian to avoid interference when passing other pedestrians (ft).

To compute spatial distribution, the analyst must make field observations or estimate the platoon size by using Equation $\ref{eq:Nb}$:
$$
N_b = \frac{v_b e^{v_b t_{cb}} + v_v e^{-vt_{cb}}}{(v_b + v_v) e^{v_b - v_v)t_{cb}}} \label{eq:Nb}
$$
where
$N_c$ = total number of pedestrians in the crossing platoon (ped),
$v_b$ = bicycle flow rate (ped/s),
$v_v$ = vehicular flow rate (veh/s), and
$t_{cb}$ = single bicycle critical headway (s).

Group critical headway is determined with Equation $\ref{eq:tcbG}$:
$$
t_{cb,G} = t_{cb} + 2(N_b -1) \label{eq:tcbG}
$$
where
$t_{cb,G}$ = group critical headway (s),
$t_c$ = critical headway for a single pedestrian (s), and
$N_p$ = spatial distribution of pedestrians (ped).

#### ii. Estimate Probability of a Delayed Crossing

#### iii: Calculate Average Delay to Wait for Adequate Gap

#### iv. Estimate Delay Reduction due to Yielding Vehicles
