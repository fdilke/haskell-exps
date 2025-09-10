In this report, we will explore the
various factors that influence fluid
dynamics in glaciers and how they
contribute to the formation and
behaviour of these natural structures.

+ The climate
  - Temperature
  - Precipitation
+ The topography
+ The geology

The equation $Q = rho A v + C$
defines the glacial flow rate

Total displaced soil by glacial flow:

$ 7.32 beta +
  sum_(i=0)^nabla Q_i / 2 $.

$ v := vec(x_1, x_2, x_3) $

$ frac(a^2, 2) $
$ vec(1, 2, delim: "[") $
$ mat(1, 2; 3, 4) $
$ mat(..#range(1, 5).chunks(2)) $
$ lim_x =
    op("lim", limits: #true)_x $

#set text(font: "monofur")

#let cdu(name) = ([CDU], table.cell(fill: black, text(fill: white, name)))
#let spd(name) = ([SPD], table.cell(fill: red, text(fill: white, name)))
#let fdp(name) = ([FDP], table.cell(fill: yellow, name))

#table(
  columns: (auto, auto, 1fr),
  stroke: (x: none),

  table.header[Tenure][Party][President],
  [1949-1959], ..fdp[Theodor Heuss],
  [1959-1969], ..cdu[Heinrich Lübke],
  [1969-1974], ..spd[Gustav Heinemann],
  [1974-1979], ..fdp[Walter Scheel],
  [1979-1984], ..cdu[Karl Carstens],
  [1984-1994], ..cdu[Richard von Weizsäcker],
  [1994-1999], ..cdu[Roman Herzog],
  [1999-2004], ..spd[Johannes Rau],
  [2004-2010], ..cdu[Horst Köhler],
  [2010-2012], ..cdu[Christian Wulff],
  [2012-2017], [n/a], [Joachim Gauck],
  [2017-],     ..spd[Frank-Walter-Steinmeier],
)

#import "@preview/commute:0.3.0": node, arr, commutative-diagram

#align(center)[#commutative-diagram(
  node((0, 0), $X$),
  node((0, 1), $Y$),
  node((1, 0), $X \/ "ker"(f)$, "quot"),
  arr($X$, $Y$, $f$),
  arr("quot", (0, 1), $tilde(f)$, label-pos: right, "dashed", "inj"),
  arr($X$, "quot", $pi$),
)]
