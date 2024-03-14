---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:19:25.982824-07:00
description: "Praca z CSV (ang. Comma Separated Values - warto\u015Bci oddzielone\
  \ przecinkami) obejmuje analizowanie i generowanie plik\xF3w, kt\xF3re przechowuj\u0105\
  \ dane\u2026"
lastmod: '2024-03-13T22:44:35.344414-06:00'
model: gpt-4-0125-preview
summary: "Praca z CSV (ang. Comma Separated Values - warto\u015Bci oddzielone przecinkami)\
  \ obejmuje analizowanie i generowanie plik\xF3w, kt\xF3re przechowuj\u0105 dane\u2026"
title: Praca z plikami CSV
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
