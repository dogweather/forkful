---
title:                "Å jobbe med json"
html_title:           "Gleam: Å jobbe med json"
simple_title:         "Å jobbe med json"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/working-with-json.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvis du jobber med webapplikasjoner eller backend-systemer, kommer du mest sannsynlig til å jobbe med dataformatet JSON. Det er en enkel og populær måte å lagre og utveksle data på, og å ha kunnskap om hvordan man håndterer JSON kan åpne opp for flere muligheter og utviklingsmuligheter.

## Hvordan gjøre det
```Gleam
let data = """
    {"navn": "Maria", "alder": 25, "jobb": "Programmerer"}
    ```

// Skriv ut navnet
io.println(data.navn) // Output: Maria

// Endre alder
let nyData = data |> Json.set("alder", 26)

// Konverter til JSON og skriv ut
let nyJson = nyData |> Json.encode_pretty(2)
io.println(nyJson)
```
Output: 
```
{
  "navn": "Maria",
  "alder": 26,
  "jobb": "Programmerer"
}
```

## Dykk dypere
Å arbeide med JSON i Gleam er enkelt takket være Json-modulen. Den lar deg enkelt konvertere datastrukturer til JSON og vice versa ved hjelp av funksjoner som `encode` og `decode`. Du kan også bruke `encode_pretty` for å få en finere formatering på JSON-utdataen. Hvis du trenger å jobbe med større datastrukturer, kan du også bruke `save_to_file` og `load_from_file` for å lagre og hente JSON-filer.

## Se også
- [Gleam dokumentasjon om Json-modulen](https://gleam.run/documentation/json/)
- [Offisiell JSON-nettside](https://www.json.org/json-en.html)
- [En oversikt over Gleam funksjoner for å manipulere data](https://5t3ph.github.io/over-json-with-gleam.html)