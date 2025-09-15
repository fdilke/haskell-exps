#let definition(name, body) = block(
  fill: luma(95%),
  inset: 1em,
  radius: 6pt,
  stroke: 1pt + gray,
  [
    *Definition#(if name != none { " (" + name + ")" })*:  

    #body
  ]
)

#import "@preview/fletcher:0.5.8" as fletcher: diagram, node, edge

= Ambigressive Diagrams in an Adequate Triple

#definition("ambigressive diagrams")[
  Given wide subcategories $(LL, RR)$ a commutative diamond is $#math.italic("ambigressive")$ if the left-pointing arrow is in $LL$ and the right-pointing arrows are in $RR$.
]

#diagram(spacing: 2.5cm, {
  let (X, Y, Z, W) = ((1, 0), (0, 1), (2, 1), (1, 2))
  node(X, $X$)
  node(Y, $Y$)
  node(Z, $Z$)
  node(W, $W$)
  edge(X, Y, $arrow(l)$, "->", shift:3pt, label-side:right)
  edge(X, Z, $arrow(r)$, "->", shift:3pt, label-side:left)
  edge(Y, W, $r$, "->", shift:3pt, label-side:right)
  edge(Z, W, $l$, "->", shift:3pt, label-side:left)
})


