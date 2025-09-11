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

#let note(body) = block(
  fill: luma(95%),
  inset: 1em,
  radius: 6pt,
  stroke: 1pt + gray,
  [
    *Note*:

    #body
  ]
)

#import "@preview/fletcher:0.5.8" as fletcher: diagram, node, edge

= Isomorphism
#definition("isomorphism")[
  Given two objects $X, Y$ in a category.
  An isomorphism between them is a pair of _morphisms_ $f: X -> Y, g: Y -> X$ such that:
  $
    f compose g = "Id"_X \ g compose f = "Id"_Y
  $
  We say that both $f$ and $g$ are *isomorphisms* and $X$ and $Y$ are *isomorphic*
]

#diagram(spacing: 2cm, {
  let (X, Y) = ((0, 0), (1, 0))
  node(X, $X$)
  node(Y, $Y$)
  edge(X, Y, $f$, "->", shift:3pt)
  edge(Y, X, $g$, "->", shift:3pt, label-side:left)
  edge(X, X, $"Id"_X$, "->", bend:130deg, loop-angle:180deg)
  edge(Y, Y, $"Id"_Y$, "->", bend:130deg, loop-angle:0deg)
})

= Monomorphism
// Tues Sep 09 
#definition("monomorphism")[
  A _morphism_ $m : X -> Y $ is a *monomorphism*
  if for every _object_ $A$ and all _morphisms_ $f, g : A -> X$ we have:
  $
    m compose f = m compose g => f = g
  $
]

// Wed Sep 10
#diagram($
  A edge(f, ->, shift:#3pt) edge(g, ->, shift:#(-3pt), label-side:#right) &
  X edge(m, ->) & Y
$)

#note()[
  A _monomorphism_ is analagous to the concept of an _injective function_.
  $
    m ( f ( x ) ) = m ( g ( x ) ) => f ( x ) = g ( x )
  $
  Is equivalent to saying:
  $
    m compose f = m compose g => f = g
  $
]

= Epimorphism

#definition("epimorphism")[
  A _morphism_ $m : X -> Y$ is a *epimorphism*
  if for all objects $A$ and all _morphisms_ $f, g : Y -> A$ we have:
  $
    f compose m = g compose m => f = g
  $
]

#diagram($
  X edge(m, ->) &
  Y edge(f, ->, shift:#3pt) edge(g, ->, shift:#(-3pt), label-side:#right) & A
$)

#note()[
  A _epimorphism_ is analagous to the concept of a _surjective function_

  A surjective function is defined as a function $f: X -> Y $ that takes all values in $Y$.

  Imagine if $m$ didn't take all values in $Y$. Given a function $f$ We could come up with infintely many functions $g$ such that $f compose m = g compose m$.
]



