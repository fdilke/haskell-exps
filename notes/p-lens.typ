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

= Lenses in a Fibred Category

#definition("lenses in a fibred category")[
  Given a fibred category $p: EE -> BB$ a $#math.italic("p-lens")$ is a span with left leg vertical and right leg cartesian.
]

#diagram(spacing: 3cm, {
  let (fB, A_, Aplus2, Aplus, B_, Bplus) = ((1, 0), (0, 1), (1, 1), (0, 2), (2, 1), (2, 2))
  node(fB, $f^*B^-$)
  node(A_, $A^-$)
  node(Aplus2, $A^+$)
  node(Aplus, $A^+$)
  node(B_, $B^-$)
  node(Bplus, $B^+$)
  edge(fB, A_, $f^"#"$, "->", shift:3pt, label-side:right)
  edge(fB, Aplus2, $$, "|=>", shift:3pt, label-side:left)
  edge(A_, Aplus, $$, "|=>", shift:3pt, label-side:right)
  edge(Aplus2, Aplus, $=$, "-", shift:3pt, label-side:left)
  edge(fB, B_, $arrow(f)$, "->", shift:3pt, label-side:left)
  edge(B_, Bplus, $$, "|=>", shift:3pt, label-side:right)
  edge(Aplus2, Bplus, $f$, "->", shift:3pt, label-side:left)
})


