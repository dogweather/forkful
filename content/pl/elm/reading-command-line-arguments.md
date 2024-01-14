---
title:                "Elm: Odczytywanie argumentów wiersza poleceń"
simple_title:         "Odczytywanie argumentów wiersza poleceń"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Programowanie w języku Elm może być bardzo przyjemne i wygodne, ale czasami musimy mieć do czynienia z nieznajomymi zmiennymi i opcjami. Jednym z przykładów jest czytanie argumentów z wiersza poleceń. W tym artykule przedstawimy, dlaczego jest to ważna umiejętność i jak ją wykorzystać w języku Elm.

## Jak to zrobić

Aby czytać argumenty z wiersza poleceń w Elm, musimy skorzystać z pakietu `core` i jego modułu `Platform.Cmd`. Poniżej znajduje się przykładowy kod:

```Elm
import Platform.Cmd exposing (args)

main : Program Never
main =
    Program.platform
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : ( String, Cmd.Cmd msg )
init =
    ( "Hello, world!", Cmd.none )

update : msg -> String -> ( String, Cmd.Cmd msg )
update msg model =
    ( args! model, Cmd.none )

view : String -> Html.Html msg
view text =
    h1 [] [ text ]

subscriptions : String -> Sub.Sub msg
subscriptions _ =
    Sub.none
```

Kod ten używa funkcji `args`, która zwraca listę argumentów przekazanych do programu przez wiersz poleceń. Następnie funkcja `update` pobiera pierwszy argument z listy i wyświetla go w aplikacji. Prosta i wygodna metoda, prawda?

## Głębsze zanurzenie

W powyższym przykładzie użyliśmy tylko jednego argumentu, ale funkcja `args` zwraca całą listę argumentów. Możemy więc wykorzystać tę funkcjonalność do bardziej skomplikowanych zastosowań. Na przykład, możemy czytać argumenty z wiersza poleceń w celu ustawienia różnych ustawień lub opcji dla naszej aplikacji. Możemy również dostosowywać zachowanie naszej aplikacji w zależności od argumentów przekazanych do niej.

## Zobacz również

- Dokumentacja pakietu `core` dotycząca funkcji `args`: <link>
- Przykładowe aplikacje w Elm: <link>
- Wideo tutorial na temat czytania argumentów z wiersza poleceń w Elm: <link>