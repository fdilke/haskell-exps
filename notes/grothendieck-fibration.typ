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

= Grothendieck Fibration

#definition("Grothendieck fibration")[
  A functor $EE$ -> $BB$ is a $#math.italic("Grothendieck fibration")$ if for every object $y$ in $EE$ and every morphism $f_0: b -> P(y)$ in $BB$ there exists a $#math.italic("Cartesian lift")$: a morphism $f: x -> y$ in $EE$ 
  such that $f_0 = P(f)$, $b = P(x)$ and for every pair of morphisms $g: z -> y$ in $EE$ and $w: italic("Pz") -> italic("Px")$ in $BB$ such that $P(g) = P(f) compose w$, there exists a unique morphism $arrow(w): z -> x$ in $EE$ such that $g = f compose arrow(w)$ and $P(arrow(w)) = w$.
]

#diagram(spacing: 2.5cm, {
  let (z, y, x, Px, Pz, Py, E, B) = ((0, 0), (2, 0), (1, 1), (1, 2), (0, 3), (2, 3), (3, 0), (3, 3))
  node(z, $z$)
  node(y, $y$)
  node(x, $x$)
  node(Pz, $italic("Pz")$)
  node(Py, $italic("Py")$)
  node(Px, $italic("b = Px")$)
  node(E, $EE$)
  node(B, $BB$)
  edge(z, y, $g$, "->", shift:3pt)
  edge(z, x, $arrow(w)$, "-->", shift:3pt)
  edge(x, y, $f$, "->", label-side:left)
  edge(x, Px, $P$, "|=>", shift:3pt, label-side:left)
  edge(y, Py, $P$, "|=>", shift:3pt, label-side:left)
  edge(z, Pz, $P$, "|=>", shift:3pt, label-side:left)
  edge(E, B, $P$, "|=>", shift:3pt, label-side:left)
  edge(Pz, Px, $w$, "->", shift:3pt)
  edge(Pz, Py, $italic("Pg")$, "->", shift:3pt)
  edge(Px, Py, $f_0 = italic("Pf")$, "->", shift:3pt, label-side:left)
  // edge(X, X, $"Id"_X$, "->", bend:130deg, loop-angle:180deg)
  // edge(Y, Y, $"Id"_Y$, "->", bend:130deg, loop-angle:0deg)
})
