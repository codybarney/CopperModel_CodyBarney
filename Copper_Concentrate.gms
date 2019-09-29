$title United States Copper Concentrate Transportation Problem

$onText
TEXT
$offText

Set
   i 'mine sites' / bagdad,  bingham,  chino,  continental,  mission,  morenci,  pinto,  ray,  robinson,  sierrita /
   j 'smelters'    / hayden,  kennecott,  miami /;

Parameter
   a(i) 'capacity of mine i in kilotonnes'
        / bagdad                270
          bingham               822
          chino                    194
          continental           181
          mission                 182
          morenci                428
          pinto                     202
          ray                        165
          robinson               220
          sierrita                  176 /

   b(j) 'demand at smelter j in kilotonnes'
        / hayden   720
          kennecott    950
          miami     730 /;

Table d(i,j) 'distance in miles'
                     hayden      kennecott      miami
   bagdad              222               641        210
   bingham            701                  0         673
   chino                 237               764        213
   continental       1119              433       1091
   mission                92               805        147
   morenci             158               721        134
   pinto                   49               697         0
   ray                      21               724        39
   robinson           637               232         625
   sierrita              105               818         145;

Scalar f 'freight in dollars per kilotonne per mile' / 1 /;

Parameter c(i,j) 'transport cost in miles per kilotonne';
c(i,j) = f*d(i,j);

Variable
   x(i,j) 'shipment quantities in kiltonnes'
   z      'total transportation costs in thousands of dollars';

Positive Variable x;

Equation
   cost      'define objective function'
   supply(i) 'observe supply limit at mine i'
   demand(j) 'satisfy demand at smelter j';

cost..      z =e= sum((i,j), c(i,j)*x(i,j));

supply(i).. sum(j, x(i,j)) =l= a(i);

demand(j).. sum(i, x(i,j)) =e= b(j);

Model transport / all /;

solve transport using lp minimizing z;

display x.l, x.m;
