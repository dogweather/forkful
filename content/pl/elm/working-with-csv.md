---
title:                "Praca z plikami CSV"
html_title:           "Bash: Praca z plikami CSV"
simple_title:         "Praca z plikami CSV"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?
Praca z CSV to manipulowanie danymi zapisanymi w formacie Comma-Separated Values. Programiści korzystają z niego, bo to łatwy do odczytu i uniwersalny format wymiany danych.

## How to:
Elm nie ma wbudowanych funkcji do pracy z CSV, ale możesz używać bibliotek lub pisać własne parsery.

```Elm
-- Załóżmy, że masz prosty CSV jako String:
csvData : String
csvData =
    "name,age\nAlice,30\nBob,25"

-- parsujemy dane, rozbijając na wiersze i kolumny
parseCsv : String -> List (List String)
parseCsv data =
    data
        |> String.split "\n"
        |> List.map (String.split ",")
        
-- Wykorzystaj `parseCsv` funkcję w swojej aplikacji
parsedData : List (List String)
parsedData = parseCsv csvData
```

Wynik: `[["name","age"],["Alice","30"],["Bob","25"]]`

## Deep Dive
CSV pojawił się w latach 70-tych. Jest prosty, ale ma ograniczenia, m.in. brak standardu dla znaków specjalnych. W Elm Parsing CSV zależy od zewnętrznych bibliotek jak `elm-csv` albo napisania własnego rozwiązania, które będzie dobrze współgrać z typowym dla Elm podejściem do niemutowalności danych i bezpieczeństwa typów.

Alternatywą dla CSV może być JSON, który jest lepiej wspierany w Elm dzięki wbudowanemu dekoderowi.

## See Also
- Elm CSV-parsing package: [elm-csv](https://package.elm-lang.org/packages/lovasoa/elm-csv/latest/)
- Elm guide on JSON decoding: [Elm JSON Decoding](https://guide.elm-lang.org/effects/json.html)
- Discussion on CSV handling in Elm: [Elm Discourse](https://discourse.elm-lang.org/)