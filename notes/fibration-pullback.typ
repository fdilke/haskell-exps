#let lemma(name, body) = block(
  fill: luma(95%),
  inset: 1em,
  radius: 6pt,
  stroke: 1pt + gray,
  [
    *Lemma#(if name != none { " (" + name + ")" })*:  

    #body
  ]
)

#import "@preview/fletcher:0.5.8" as fletcher: diagram, node, edge

= Pullbacks of Vertical-Cartesian Cospans in a Fibration

#lemma("pullbacks of vertical-cartesian cospans in a fibration")[
  Given a fibred category p: $EE$ -> $BB$ , any cospan $f: x -> y <- z: g$ in $EE$ where $g$ is vertical and $f$ cartesian, has a pullback in $EE$/$BB$.
]

#diagram(spacing: 2.5cm, {
  let (fE2, E2, E1, B1, B, E, B12, B_2) = ((2, 0), (3, 1), (0, 1), (2, 1), (3, 2), (1, 2), (0, 2), (1, 3))
  node(fE2,$f*E_2$)
  node(E2, $E_2$)
  node(E1, $E_1$)
  node(B1, $B_1$)
  node(B, $B$)
  node(E, $E$)
  node(B12, $B_1$)
  node(B_2, $B$)
  edge(fE2, E2, $diaer(f)$, "->", shift:3pt, label-side:left)
  edge(E2, E, $arrow(g)$, "->", shift:3pt, label-side:left)
  edge(E1, E, $arrow(f)$, "->", shift:3pt, label-side:left)
  edge(fE2, E1, $f^*arrow(g)$, "->", shift:3pt, label-side:right)

  edge(B1, B, $f$, "->", shift:3pt, label-side:left)  
  edge(B1, B12, $=$, "-", shift:3pt, label-side:right)  
  edge(B12, B_2, $f$, "->", shift:3pt, label-side:left) 
  edge(B, B_2, $=$, "-", shift:3pt, label-side:right)

  edge(fE2, B1, $P$, "|=>", shift:3pt, label-side:left)
  edge(E1, B12, $P$, "|=>", shift:3pt, label-side:left)
  edge(E2, B, $P$, "|=>", shift:3pt, label-side:left)
  edge(E, B_2, $P$, "|=>", shift:3pt, label-side:left)
})


