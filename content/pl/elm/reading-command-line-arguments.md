---
title:                "Elm: Odczytywanie argumentów wiersza poleceń"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Dlaczego warto poznać czytanie argumentów wiersza poleceń w Elm

Poznanie, jak czytać argumenty wiersza poleceń w języku Elm, może znacznie ułatwić nam pracę z naszymi programami. Dzięki temu narzędziu będziemy mogli dostosowywać działanie naszych aplikacji w zależności od przekazanych argumentów, co jest szczególnie przydatne przy tworzeniu programów, które mają mieć różną funkcjonalność w zależności od kontekstu użycia.

# Jak to zrobić: Przykłady kodu i wyników

Przyjrzyjmy się przykładom kodu w języku Elm, które pozwolą nam na dokładne zrozumienie procesu czytania argumentów wiersza poleceń.

## Prosty przykład:
```Elm
main =
    let
        arguments = System.args
    in
        System.exit arguments
```

W powyższym przykładzie wykorzystujemy funkcję `System.args`, która zwraca nam listę przekazanych argumentów wiersza poleceń. Następnie wykorzystujemy funkcję `System.exit`, aby wyjść z programu i wyświetlić otrzymane argumenty.

Przykładowy wynik dla uruchomienia naszego programu z argumentami `--mode=test` wyglądałby następująco:

```
["--mode=test"]
```

## Lepsza obsługa argumentów:
```Elm
import Dict exposing (fromList)
import List exposing (cons)

main =
    let
        arguments = System.args
        params = fromList (List.filterMap extractParameter arguments)
    in
        System.exit params

extractParameter argument =
    case String.split "=" argument of
        [ key, value ] ->
            Just ( key, value )

        _ ->
            Nothing
```

W tym przykładzie wykorzystujemy funkcje z pakietu `Dict` oraz `List` w celu lepszej obsługi przekazanych argumentów. Najpierw tworzymy słownik `params`, który przechowuje przekazane argumenty jako klucze i wartości. Następnie wykorzystujemy funkcję `String.split` w celu rozdzielenia argumentu na parę `klucz=wartość` oraz funkcję `List.filterMap`, aby usunąć niepoprawne argumenty. Dzięki temu możemy łatwo dostosować nasz program do różnych argumentów i wykorzystywać je w wybrany przez nas sposób.

Przykładowy wynik dla uruchomienia naszego programu z argumentami `--mode=test` oraz `--date=2021-09-01` wyglądałby następująco:

```
{ "mode" = "test", "date" = "2021-09-01" }
```

## Zawansowane użycie:
```Elm
type alias User =
    { name : String
    , age : Int
    }

main =
    let
        arguments = System.args
        params = getArguments arguments
        user = getUser params
    in
        System.exit user

getArguments arguments =
    let
        params = getArgumentsDict arguments
    in
        { name = Dict.get "name" params
        , age = Dict.get "age" params
        }

getArgumentsDict arguments =
    fromList (List.filterMap extractParameter arguments)

extractParameter argument =
    case String.split "=" argument of
        [ key, value ] ->
            Just ( key, value )

        _ ->
            Nothing

getUser { name, age } =
    { name = Maybe.withDefault "John" name
    , age = Maybe.withDefault 25 (Maybe.map String.toInt age)
    }
```

W tym zaawansowanym przykładzie wykorzystujemy funkcję `String.toInt` oraz moduł `Maybe` w celu konwersji otrzymanych argumentów do odpowiedniego typu. Tworzymy również alias `User` oraz funkcję `getUser`, która zwraca nam obiekt typu `User` na podstawie przekazanych argumentów.

## Obsługa błędów:
```Elm
import String exposing (toInt)

main =
    let