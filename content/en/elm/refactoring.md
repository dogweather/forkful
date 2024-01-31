---
title:                "Refactoring"
date:                  2024-01-25T02:12:15.719110-07:00
model:                 gpt-4-1106-preview
simple_title:         "Refactoring"

category:             "Elm"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/elm/refactoring.md"
---

{{< edit_this_page >}}

## What & Why?
Refactoring is essentially spring cleaning your code base—it's about restructuring existing code without changing its external behavior. Programmers do it to make the code more readable, reduce complexity, improve maintainability, and make it easier to extend.

## How to:
Consider you have an Elm function that's doing too much, like mixing up UI logic with state updates. It's a perfect candidate for refactoring. Originally:

```Elm
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    let
        updatedModel = { model | userInput = input }
    in
    if String.length input > 5 then
        ( updatedModel, Cmd.none )
    else
        ( model, Cmd.none )
```

After refactoring, we separate concerns by pulling out the logic into different functions:

```Elm
-- Update logic is separate
updateUserInput : String -> Model -> Model
updateUserInput input model = 
    { model | userInput = input }

-- The formatting (view) logic is also separate
formatUserInput : Model -> (Model, Cmd Msg)
formatUserInput model =
    if String.length model.userInput > 5 then
        ( model, Cmd.none )
    else
        ( { model | userInput = "" }, Cmd.none ) -- Clear input if it's too short, as an example rule.

-- Update function now uses helper functions
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    model
    |> updateUserInput input
    |> formatUserInput
```
With these changes, you have clear separation, and each function is easier to understand and test.

## Deep Dive
Refactoring as a formal practice can be traced back to the early days of programming when the cost of changing code was already being recognized as a critical aspect of the development process. Notably, Martin Fowler's book "Refactoring: Improving the Design of Existing Code," published in the late 1990s, really set the stage for refactoring with a structured approach and catalog of "code smells" to identify refactoring opportunities. 

In the context of Elm, refactoring taps into the language's strengths, like its strong type system, which promotes confidence during the process. Alternatives to manual refactoring can include automated code transformation tools, but Elm's tooling in this area is still maturing compared to some older languages. Implementation details often revolve around common refactorings like function extraction, renaming, and simplifying conditionals. Elm's compiler is a key ally in refactoring, as it won’t let you get away with much—it screams whenever something's amiss, ensuring your refactored code still works.

## See Also
- ["Refactoring: Improving the Design of Existing Code" by Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [Elm Discourse - Topics on Refactoring](https://discourse.elm-lang.org/search?q=refactoring)
