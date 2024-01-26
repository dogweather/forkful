---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:49:04.928632-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "Elm"
category:             "Elm"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elm/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Generating random numbers means creating unpredictable values. Programmers do it for games, simulations, and anytime they need a touch of chaos.

## How to (Як це зробити):
Here's the straightforward way to do it in Elm:

```Elm
import Random

type Msg = Roll | NewFace Int

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Roll ->
            (model, Random.generate NewFace (Random.int 1 6))

        NewFace newFace ->
            ({ model | face = newFace }, Cmd.none)
            
-- This will produce a random number between 1 and 6, emulating a dice roll.
```

## Deep Dive (Поглиблене занурення):
Elm uses a "pure" approach, keeping randomness explicit and managed, unlike older languages that use global states like `rand` in C. You'll need to thread random number through updates and commands, making randomness traceable and reliable. Alternatives like pseudo-random number generation have their uses, but Elm's built-in `Random` module is usually the way to go for simplicity and functionality.

## See Also (Дивіться також):
- Elm's Official Random Docs: [link](https://package.elm-lang.org/packages/elm/random/latest/)
- Community Discussions on `Random` Magic: [Elm Discourse](https://discourse.elm-lang.org/)
