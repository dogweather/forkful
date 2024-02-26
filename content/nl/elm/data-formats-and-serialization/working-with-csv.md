---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:56.281791-07:00
description: "Werken met CSV (Comma-Separated Values) betekent het lezen en schrijven\
  \ van gegevens in een tekstformaat waarbij elke regel waarden heeft die door komma's\u2026"
lastmod: '2024-02-25T18:49:48.087359-07:00'
model: gpt-4-0125-preview
summary: "Werken met CSV (Comma-Separated Values) betekent het lezen en schrijven\
  \ van gegevens in een tekstformaat waarbij elke regel waarden heeft die door komma's\u2026"
title: Werken met CSV
---

{{< edit_this_page >}}

## Wat & Waarom?

Werken met CSV (Comma-Separated Values) betekent het lezen en schrijven van gegevens in een tekstformaat waarbij elke regel waarden heeft die door komma's worden gescheiden. Programmeurs gebruiken CSV omdat het een eenvoudig formaat is dat door veel tools en systemen wordt ondersteund, wat het geweldig maakt voor gegevensuitwisseling.

## Hoe te:

Elm heeft geen ingebouwde CSV-parser, maar je kunt er gemakkelijk een toevoegen met een pakket zoals `elm-csv`. Hier is een snel voorbeeld van het parseren van CSV-gegevens:

```Elm
import Csv

csvData : String
csvData =
    "naam,leeftijd\nAlice,30\nBob,25"

parseCsv : String -> Result Csv.Error (List (List String))
parseCsv data =
    Csv.decode data

main =
    case parseCsv csvData of
        Ok rows ->
            -- doe iets met de rijen
            text (String.join "," (List.head rows |> Maybe.withDefault []))
            
        Err error ->
            -- verwerk de fout
            text (Csv.Error.toString error)
```

Voorbeelduitvoer voor het succesvolle geval, waarbij de koppen worden weergegeven:

```
naam,leeftijd
```

## Diepere Duik

CSV bestaat al sinds de vroege jaren 1970; het is zo eenvoudig dat het daadwerkelijke standaarden voorafgaat. Alternatieven zijn onder meer JSON en XML, maar CSV wordt nog steeds de voorkeur gegeven bij het omgaan met tabelgegevens die zwaar op cijfers leunen en kort zijn op structuur. In Elm, aangezien het een front-end taal is, zul je werken door ofwel CSV te ontvangen van een backend of een lokaal bestand te verwerken dat door de gebruiker is geüpload. Dit implementeren vereist kennis van Elm's ports voor JS-interop of file package voor uploads.

## Zie Ook

- Elm-gids over interop met JavaScript: [Elm Ports](https://guide.elm-lang.org/interop/ports.html)
