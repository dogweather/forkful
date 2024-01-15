---
title:                "Arbeide med csv"
html_title:           "Elm: Arbeide med csv"
simple_title:         "Arbeide med csv"
programming_language: "Elm"
category:             "Elm"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elm/working-with-csv.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du arbeider med store datamengder, så har du sannsynligvis støtt på CSV-filer. CSV (Comma Separated Values) er en vanlig filtype for å lagre data på en strukturert måte. Og med Elm, kan du enkelt håndtere og manipulere disse filene for å gjøre dine programmer mer effektive.

## Hvordan gjøre det
Det første du må gjøre er å importere "elm/csv" pakken i ditt prosjekt. Deretter kan du bruke funksjoner som "Decode.csv", "Decode.map" og "Decode.list" for å dekode CSV-filer til Elm-datastrukturer. La oss si at du har en CSV-fil med følgende innhold:

```Elm
Navn,Alder,Adresse
John,35,London
Sarah,28,New York
```

Nå kan du bruke følgende kode for å dekode filen til en liste med poster:

```Elm
Decode.csv
    |> Decode.map (\list -> List.map (\row -> Decode.list Decode.string row) list)
    |> Decode.list Decode.int
    |> Decode.decodeString "\,Alder\,[^,]+"
    |> List.map (\[name, age, address] -> { name = name, age = age, address = address })
```

Dette vil resultere i en liste med poster som ligner på dette:

```Elm
[{ name = "John", age = 35, address = "London" },
 { name = "Sarah", age = 28, address = "New York" }]
```

Du kan også bruke funksjoner som "Encode.encode" og "Encode.toFile" for å konvertere dine Elm-datastrukturer til CSV-format og eksportere dem til en fil.

## Dypdykk
Hvis du ønsker å bli mer avansert, kan du også bruke "elm/parser" pakken for å lage din egen tilpassede parser for CSV-filer. Dette gir deg mer kontroll over hvordan dataene skal håndteres og kan være nyttig hvis du for eksempel har spesielle krav til hvordan dataene skal organiseres.

Du kan også utforske andre CSV-relaterte pakker i Elm-økosystemet, som for eksempel "elm-csv-explorer" som lar deg visualisere og utforske CSV-data på en interaktiv måte.

## Se også
- [Offisiell Elm dokumentasjon for CSV](https://package.elm-lang.org/packages/elm/csv/latest/)
- [elm/parser pakken](https://package.elm-lang.org/packages/elm/parser/latest/)
- [elm-csv-explorer pakken](https://package.elm-lang.org/packages/ppyatetskyi/elm-csv-explorer/latest/)