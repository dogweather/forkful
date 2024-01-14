---
title:    "Elm: Odczytywanie argumentów linii poleceń"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/pl/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Elm i chcesz rozszerzyć swoje umiejętności, czytanie argumentów wiersza poleceń jest ważnym narzędziem w twojej skrzynce narzędziowej. Dzięki temu możesz interaktywnie przekazywać dane do swojego programu podczas uruchamiania, co jest niezwykle przydatne przy debugowaniu i testowaniu.

## Jak to zrobić

W Elm istnieje wiele sposobów na odczytywanie argumentów wiersza poleceń, ale najprostszym i najbardziej powszechnym jest użycie modułu `Platform.Cmd`. Przykładowy kod wykorzystujący ten moduł wygląda następująco:

```Elm
import Json.Decode exposing (Decoder, string)
import Platform.Cmd exposing (Args, call)

type alias User =
    { name : String
    , age : Int
    }

program : Args -> Decoder User -> Cmd User
program args decoder =
    let
        userDecoder =
            string "name" User.name
                |> string "age" User.age

        readCmd =
            call decoder "user" args
                |> Cmd.map (Result.withDefault { name = "", age = 0 })
    in
    readCmd

-- użyj polecenia `program` w twoim glownym pliku lub w module `Main`:
main : Program Never Model Msg
main =
    program
        !Platform.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

```

Kod ten korzysta z dekodera, który definiuje żądane parametry, a następnie wywołuje polecenie `call` z nazwą i listą argumentów. Ostatecznie mapuje wynik na wartość domyślną w przypadku błędu.

## Głębszy wgląd

Ponadto, istnieje również możliwość korzystania z innego modułu `Platform.Cmd.Extra`, który oferuje dodatkowe funkcje do obsługi argumentów wiersza poleceń. Na przykład, można odczytać argumenty jako listę z wartościami typu `String`, a następnie przekonwertować je na dowolny inny typ danych.

Dodatkowo, można również użyć argumentów do sterowania uruchamianiem programu, np. wybierając różne konfiguracje lub tryby działania.

## Zobacz także

- Przykładowe użycie w oficjalnej dokumentacji Elm: [Working with Command Line Args](https://guide.elm-lang.org/interop/command_line.html)
- [Moduł Platform.Cmd](https://package.elm-lang.org/packages/elm-lang/core/latest/Platform-Cmd)
- [Moduł Platform.Cmd.Extra](https://package.elm-lang.org/packages/elm-lang/core/latest/Platform-Cmd-Extra)