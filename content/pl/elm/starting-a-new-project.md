---
title:                "Rozpoczęcie nowego projektu"
html_title:           "Elm: Rozpoczęcie nowego projektu"
simple_title:         "Rozpoczęcie nowego projektu"
programming_language: "Elm"
category:             "Elm"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Co i Po co?

Tworzenie nowego projektu to nic innego jak rozpoczęcie pracy nad nowym programem. Programiści robią to w celu stworzenia czegoś nowego, rozwiązania danego problemu, lub po prostu z ciekawości i chęci nauki. 

## Jak to zrobić:

Elm ma wbudowane narzędzia, które ułatwiają rozpoczęcie nowego projektu. Wystarczy użyć komendy `elm init`, a Elm automatycznie stworzy podstawową strukturę projektu w wybranej przez nas lokalizacji. Następnie, dodajemy nasz kod do pliku `Main.elm` i wywołujemy go w funkcji `main`, którą Elm automatycznie uruchamia przy starcie projektu.

```Elm
module Main exposing (main)

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
```

To wszystko! Teraz można zacząć pisać swój program.

## Głębszy zanurzenie:

Elm jest stworzonym przez Evana Czaplickiego językiem programowania funkcyjnego, który skupia się na prostocie, wydajności i skalowalności. Alternatywami dla Elm są takie języki jak JavaScript, Python czy Ruby, jednak Elm jest w stanie połączyć cechy tych języków, oferując jednocześnie silny system typów i narzędzia do tworzenia aplikacji internetowych. Implementacja nowego projektu w Elm jest prosta i intuicyjna, więc możesz zacząć pisać swój kod od razu.

## Zobacz także:

Jeśli chcesz dowiedzieć się więcej o języku Elm, możesz przeczytać oficjalną dokumentację na stronie [elm-lang.org](https://elm-lang.org/). Możesz też wypróbować swoje umiejętności na platformie [ellie-app.com](https://ellie-app.com/), gdzie możesz pisać i testować swój kod Elm online. Jeśli potrzebujesz wsparcia lub chcesz porozmawiać z innymi programistami, dołącz do społeczności Elm na [elmlang.slack.com](https://elmlang.slack.com/).