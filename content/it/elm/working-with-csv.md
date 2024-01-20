---
title:                "Lavorare con i file CSV"
html_title:           "Bash: Lavorare con i file CSV"
simple_title:         "Lavorare con i file CSV"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why (Cosa e Perché)
Il CSV (Comma-Separated Values) è un formato per immagazzinare dati in modo semplice e standard. I programmatori lo usano per importare o esportare grandi quantità di dati dai/ai sistemi, facile da leggere sia per le persone che per le macchine.

## How to (Come fare)
```Elm
-- Supponiamo di avere un CSV come stringa
csvData : String
csvData =
    "name,age\nAlice,30\nBob,25"

-- Trasformare questi dati in una lista di record
type alias Person =
    { name : String
    , age : Int
    }

parseCsv : String -> List Person
parseCsv rawCsv =
    rawCsv
        |> String.split "\n"
        |> List.drop 1
        |> List.map (String.split ",")
        |> List.map (\personList -> Person (List.head personList |> Maybe.withDefault "") (String.toInt (List.head (List.drop 1 personList) |> Maybe.withDefault "0") |> Result.withDefault 0))

-- Usare parseCsv per ottenere una lista di Person
parsedPeople : List Person
parsedPeople =
    parseCsv csvData
```
**Output campione:**
```
[ Person { name = "Alice", age = 30 }, Person { name = "Bob", age = 25 } ]
```

## Deep Dive (Approfondimento)
Il CSV esiste dagli anni '70, semplice ma efficace. In Elm, non c'è una libreria standard per il parsing dei CSV, quindi lo facciamo manualmente o si può usare un pacchetto di terze parti. L'alternativa moderna al CSV è JSON, che gestisce dati strutturati più complessi. Tuttavia, CSV è ancora popolare per la sua leggibilità e facilità d'uso in fogli di calcolo.

## See Also (Vedi Anche)
- Documentazione Elm per `String`: https://package.elm-lang.org/packages/elm/core/latest/String
- Un pacchetto Elm CSV parser: https://package.elm-lang.org/packages/lovasoa/elm-csv/latest/
- Approfondimento sulle differenze tra CSV e JSON: https://www.datacamp.com/community/tutorials/csv-json
- Best practices per lavorare con CSV in Elm: (inserisci risorsa della community)