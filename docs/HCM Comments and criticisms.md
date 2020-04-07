## Highway Capacity Manual: Notes, Comments, and Criticisms

This document contains a list of notes, comments, and criticisms observed while using the latest version of the Highway Capacity Manual. The purpose is not to be contrarian, but to provide constructive criticism regarding possible mistakes, weaknesses, and area for improvement regarding bicycles and pedestrian level of service.



### 1. Bicycle LOS does not account control delay and vehicular traffic speed

Unlike pedestrian LOS, bicycle LOS in HCM does *NOT* directly account for motorized vehicle traffic speed and bicycle delay. This is problematic as it omits two fundamental aspects of bicyclist experience, the speed of traffic bicyclists are exposed to (safety) and the delay experienced waiting for gaps and signals (travel time). This essentially means that the safety and travel time of bicyclists is not important to LOS, which of course, is not realistic.

Below are the overall LOS score formulas for bicycles and pedestrians.

##### Pedestrian LOS score formula (current HCM):

Segment:

$$
I_{p,seg} = F_{cd} ( 0.318 I_{p,link}) + 0.220 I_{p,int} + 1.606
$$

Link:
$$
I_{p,link} = 6.0468 + F_w + F_v + F_s
$$

Intersection:
$$
I_{p,int} = 0.5997 + F_w + F_v + F_s + F_{delay}
$$

##### Bicycle LOS score formula (current HCM):

Segment:
$$
I_{b,seg} = 0.160 I_{b,link} + 0.011 F_{b,link}e^{I_{b,int}} + 0.035 \frac{N_{ap,s}}{L/5280} + 2.85
$$
Link:
$$
I_{b,link} = 0.760 + F_w + F_v + F_s + F_p
$$
Intersection:
$$
I_{b,int} = 4.1324 + F_w + F_v
$$
Where:
$I$ is the LOS score (0 = A and 5 = F),
$F_w$ is cross section adjustment factor,
$F_v$ is motorized vehicle adjustment factor,
$F_s$ is motorized vehicle speed adjustment factor,
$F_{delay}$ is delay adjustment factor, and
$F_p$ is pavement condition adjustment factor.

The key elements missing from the bicycle LOS formulas are $F_s$ and $F_{delay}$ at intersections. The Bicycle intersection LOS formula should be modified to include these factors. The problem however, are the calibration constants and factors. For example, for pedestrian LOS, the delay factor $F_{delay}$, and the speed factor $F_s$ are calculated as:
$$
F_{delay} = 0.0401 ln(d_{p,d})
$$

$$
F_s = 0.00013 N_{15,mj}S_{85,mj}
$$

In all likelihood, the calibration constants (i.e., 0.0401 and 0.00013) are not applicable to bicycles, but the basic function could be utilized.



### 2. Typo in equation 19-82 in Chapter 19 of the HCM appears to have a typo.

The equation has the cube-root on the last term in the wrong position. It should be outside the parenthesis, not inside:
$$
P(Y_i) = \left[ P_d - \sum\limits^{i-1}_{j=0}P(Y_j) \right]\times\left[ \frac{P_b^4 M_y^4 + 4P_b^3(1-P_b)M_y^3 + 6P_b^2(1-P_b)^2 M_y^2 + 4P_b(1-P_b)^3M_y}{P_d} \right]
$$

### 3. Pavement condition rating is subjective and does not include other important pavement quality considerations (such as striping, hazards, and cleanliness)



