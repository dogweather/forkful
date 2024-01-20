---
title:                "Praca z yaml"
html_title:           "Elm: Praca z yaml"
simple_title:         "Praca z yaml"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/working-with-yaml.md"
---

{{< edit_this_page >}}

## Czym jest i dlaczego?

YAML (Yet Another Markup Language) jest językiem znaczników używanym przez programistów do formatowania danych w sposób łatwy do czytania dla człowieka. Jest to popularna alternatywa dla innych formatów, takich jak JSON lub XML, ponieważ YAML jest uważany za bardziej przejrzysty i czytelny. Programiści używają YAML do przechowywania konfiguracji aplikacji lub danych w formie strukturyzowanej.

## Jak to zrobić:

```Elm
import Data.Yaml exposing (..)

-- przykładowy plik YAML
dreamTeam:
  - Jan
  - Kasia
  - Tomek
  - Ania

-- wczytanie danych z pliku
readFile "team.yaml"
    |> andThen decodeValue
    |> Result.withDefault []

-- wynik
["Jan", "Kasia", "Tomek", "Ania"]
```

## Głębsza analiza:

### Kontekst historyczny:
 YAML został opracowany w 2001 roku przez Clarka Evansa w celu stworzenia prostego i łatwego w użyciu języka znaczników. Od tego czasu stał się popularną opcją dla programistów ze względu na swoją czytelność i łatwość w rozwiązaniu różnych problemów związanych z formatowaniem danych.

### Alternatywy:
Istnieje wiele innych formatów, takich jak JSON lub XML, które są również popularne wśród programistów. Każdy z nich ma swoje własne zastosowanie, więc wybór zależy od konkretnej potrzeby i preferencji programisty.

### Szczegóły implementacji:
ELM udostępnia moduł Data.Yaml, który pozwala na łatwe wczytywanie i zapisywanie danych w formacie YAML. Moduł ten zawiera wiele przydatnych funkcji, takich jak decodeValue i encode, które ułatwiają pracę z tym językiem znaczników.

## Zobacz także:

- [Oficjalna strona YAML](https://yaml.org/)