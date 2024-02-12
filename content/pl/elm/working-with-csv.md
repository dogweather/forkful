---
title:                "Praca z plikami CSV"
aliases:
- pl/elm/working-with-csv.md
date:                  2024-02-03T19:19:25.982824-07:00
model:                 gpt-4-0125-preview
simple_title:         "Praca z plikami CSV"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?

Praca z CSV (ang. Comma Separated Values - wartości oddzielone przecinkami) obejmuje analizowanie i generowanie plików, które przechowują dane tabelaryczne w prostej, tekstowej formie. Jest to powszechna praktyka wśród programistów, umożliwiająca łatwą wymianę danych między różnymi aplikacjami lub efektywne przetwarzanie dużych zbiorów danych w bezpieczny sposób typów w Elm.

## Jak to zrobić:

Elm nie ma wbudowanego wsparcia dla analizowania ani generowania CSV; zamiast tego często wykorzystywane są pakiety stron trzecich, takie jak `panosoft/elm-csv`. Poniższe przykłady podkreślają podstawowe wykorzystanie tej biblioteki do analizowania i generowania plików CSV.

### Analizowanie CSV

Najpierw musisz dodać pakiet CSV do swojego projektu Elm:

```bash
elm install panosoft/elm-csv
```

Następnie możesz przekształcić łańcuch CSV na listę rekordów. Prosty przykład:

```elm
import Csv

csvData : String
csvData =
    "name,age\nJohn Doe,30\nJane Smith,25"

parseResult : Result String (List (List String))
parseResult =
    Csv.parse csvData

-- Przykładowe wyjście: Ok [["name","age"],["John Doe","30"],["Jane Smith","25"]]
```

### Generowanie CSV

Aby wygenerować łańcuch CSV z danych Elm, użyj funkcji `Csv.encode`:

```elm
import Csv

records : List (List String)
records =
    [ ["name", "age"]
    , ["John Doe", "30"]
    , ["Jane Smith", "25"]
    ]

csvOutput : String
csvOutput =
    Csv.encode records

-- Przykładowe wyjście: "name,age\nJohn Doe,30\nJane Smith,25\n"
```

To uproszczone podejście umożliwia Tobie integrację funkcji CSV w Twoich aplikacjach Elm, wykorzystując bezpieczne środowisko typów do manipulacji danymi i wymiany.
