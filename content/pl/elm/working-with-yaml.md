---
title:                "Elm: Praca z formatem yaml"
simple_title:         "Praca z formatem yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Dlaczego
Zostało udowodnione, że YAML jest skutecznym oraz czytelnym językiem do przechowywania danych. Pozwala na łatwe przechowywanie danych w plikach konfiguracyjnych oraz wykorzystanie ich w programowaniu. W tym artykule dowiecie się, dlaczego warto zacząć pracę z YAML w języku Elm.

## Jak
Aby rozpocząć pracę z YAML w Elm, należy skorzystać z biblioteki [elm-yaml](https://package.elm-lang.org/packages/gampleman/elm-yaml/latest/). Możemy w niej znaleźć funkcje, które pozwalają na odczytywanie danych z plików YAML oraz konwersję ich na formaty obiektów oraz wartości prostych.

```Elm
import Yaml

-- Odczytanie pliku YAML
fileContents : Task.Task Task.Error Yaml.Value
fileContents =
    Yaml.parseFile Yaml.defaultValue "config.yml"

-- Konwersja na obiekt
toObject : Yaml.Value -> Maybe Yaml.Object
toObject yaml =
    case yaml of
        Yaml.Object obj ->
            Just obj
        _ ->
            Nothing

-- Uzyskanie wartości z obiektu
getConfigValue : Maybe Yaml.Object -> String -> Maybe Yaml.Value
getConfigValue maybeObj key =
    case maybeObj of
        Just obj ->
            Dict.get key obj
        _ ->
            Nothing

-- Wykorzystanie funkcji
readConfig : String -> Task.Task Task.Error (Maybe String)
readConfig key =
    Task.perform (getConfigValue <|
        toObject
        |> andThen (maybe Just Yaml.toString)
    ) fileContents key
```

Powyższy przykład pokazuje, jak można odczytać wartość z pliku YAML i przekonwertować ją na format tekstowy. Więcej informacji na temat możliwych funkcji można znaleźć w dokumentacji biblioteki.

## Dogłębne zagłębienie
Praca z YAML w Elm może być bardziej skomplikowana, gdy zajdzie potrzeba przetwarzania złożonych struktur danych lub wykorzystania specjalnych funkcji, takich jak interpolacja ciągów tekstowych. Jednak dzięki bibliotece [elm-yaml](https://package.elm-lang.org/packages/gampleman/elm-yaml/latest/) możliwości tworzenia aplikacji z wykorzystaniem plików konfiguracyjnych w formacie YAML są praktycznie nieograniczone.

## Zobacz także
- [Dokumentacja biblioteki elm-yaml](https://package.elm-lang.org/packages/gampleman/elm-yaml/latest/)
- [Poradnik o przetwarzaniu danych z YAML w Elm](https://dev.to/manuarora/working-with-yaml-in-elm-1pa9)