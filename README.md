# Elm Flow Layout!

This package is the flow layout library that we've developed at Vendr, which powers our Workflow visualizations.

It's made to visualize structures that are halfway between trees and DAGs.

Essentially anything that can fit into this type can be visualized:

```
type Flow node
    = Node node
    | Sequential (List (Flow node))
    | Parallel (List (Flow node))
    | Condition node
        { trueSeq : List (Flow node)
        , falseSeq : List (Flow node)
        }
```

![Example](https://github.com/vendrinc/elm-flow-layout/blob/main/examples/example.png?raw=true)

> [!NOTE]  
> This package likely won't receive significant updates!
> If you run into issues, we recommend simply copying this code into your project and modifying as needed.

## Contributors

Because this code was originally developed internally, we lost the git history for tracking for who developed this code.

The primary authors were:

- Agus Zubiaga - https://github.com/agu-z
- Martin Janiczek - https://github.com/Janiczek
- Macario Ortega - https://github.com/maca
