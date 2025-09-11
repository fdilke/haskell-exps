#import "@preview/diagraph:0.3.3": *


#set heading(numbering: (..nums) => [Graph #numbering("1", ..nums):])


#let render-example(dot, ..args) = context {
  let code = raw(dot.text, lang: "dot")
  let graph = render(dot.text, ..args)
  let side-by-side = measure(code).width + measure(graph).width < 20cm
  let columns = if side-by-side {
    (auto, auto)
  } else {
    (auto,)
  }
  grid(
    columns: columns,
    gutter: 1cm,
    // raw(dot.text, lang: "dot"),
    graph,
  )
}


= State Machine

#render-example(
  ```
  digraph finite_state_machine {
    rankdir=LR
    size="8,5"

    node [shape=doublecircle]
    LR_0
    LR_3
    LR_4
    LR_8

    node [shape=circle]
    LR_0 -> LR_2 [label="SS(B)"]
    LR_0 -> LR_1 [label="SS(S)"]
    LR_1 -> LR_3 [label="S($end)"]
    LR_2 -> LR_6 [label="SS(b)"]
    LR_2 -> LR_5 [label="SS(a)"]
    LR_2 -> LR_4 [label="S(A)"]
    LR_5 -> LR_7 [label="S(b)"]
    LR_5 -> LR_5 [label="S(a)"]
    LR_6 -> LR_6 [label="S(b)"]
    LR_6 -> LR_5 [label="S(a)"]
    LR_7 -> LR_8 [label="S(b)"]
    LR_7 -> LR_5 [label="S(a)"]
    LR_8 -> LR_6 [label="S(b)"]
    LR_8 -> LR_5 [label="S(a)"]
  }
  ```,
  labels: (
    "LR_0": $"LR"_0$,
    "LR_1": $"LR"_1$,
    "LR_2": $"LR"_2$,
    "LR_3": $"LR"_3$,
    "LR_4": $"LR"_4$,
    "LR_5": $"LR"_5$,
    "LR_6": $"LR"_6$,
    "LR_7": $"LR"_7$,
    "LR_8": $"LR"_8$,
  ),
)

= State Machine II

#render-example(
  ```
  digraph finite_state_machine {
    rankdir=LR
    size="8,5"

    node [shape=doublecircle]
    LR_0
    LR_1
    LR_2

    node [shape=circle]
    LR_0 -> LR_1 [label="SS(B)"]
    LR_0 -> LR_1 [label="SS(S)"]
    LR_1 -> LR_2 [label="S($end)"]
    LR_2 -> LR_1 [label="S($end)"]
  }
  ```,
  labels: (
    "LR_0": $"LR"_0$,
    "LR_1": $"LR"_1$,
    "LR_2": $"LR"_2$,
  ),
)

= State Machine III

context {
  let graph = render(
      ```
    digraph finite_state_machine {
      rankdir=LR
      size="8,5"

      node [shape=doublecircle]
      LR_0
      LR_3
      LR_4
      LR_8

      node [shape=circle]
      LR_0 -> LR_2 [label="SS(B)"]
      LR_0 -> LR_1 [label="SS(S)"]
      LR_1 -> LR_3 [label="S($end)"]
      LR_2 -> LR_6 [label="SS(b)"]
      LR_2 -> LR_5 [label="SS(a)"]
      LR_2 -> LR_4 [label="S(A)"]
      LR_5 -> LR_7 [label="S(b)"]
      LR_5 -> LR_5 [label="S(a)"]
      LR_6 -> LR_6 [label="S(b)"]
      LR_6 -> LR_5 [label="S(a)"]
      LR_7 -> LR_8 [label="S(b)"]
      LR_7 -> LR_5 [label="S(a)"]
      LR_8 -> LR_6 [label="S(b)"]
      LR_8 -> LR_5 [label="S(a)"]
    }
    ```.text,
    labels: (
      "LR_0": $"LR"_0$,
      "LR_1": $"LR"_1$,
      "LR_2": $"LR"_2$,
      "LR_3": $"LR"_3$,
      "LR_4": $"LR"_4$,
      "LR_5": $"LR"_5$,
      "LR_6": $"LR"_6$,
      "LR_7": $"LR"_7$,
      "LR_8": $"LR"_8$,
    ),
  )
  grid(
    columns: (auto,),
    gutter: 1cm,
    graph,
  )
}
