---
title:                "Gleam: Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Det finns många olika situationer där det kan vara användbart att räkna ut en datum i framtiden eller tidigare. Till exempel kan det vara för att planera ett evenemang, boka en resa eller hålla koll på en viktig deadline.

## Hur man gör

För att beräkna en datum i framtiden eller tidigare i Gleam, kan du använda funktionen `add_days`. Du behöver bara ange det datum som du vill utgå ifrån och antalet dagar som du vill lägga till eller subtrahera.

```Gleam
let datum = Date.from_y_m_d(2020, 7, 20)
let nytt_datum = Date.add_days(datum, 10)

IO.println(nytt_datum)
```
Output: 2020-07-30

För att subtrahera dagar istället, använd ett negativt tal för antalet dagar.

```Gleam
let datum = Date.from_y_m_d(2020, 7, 20)
let nytt_datum = Date.add_days(datum, -5)

IO.println(nytt_datum)
```
Output: 2020-07-15

## Djupdykning

För att få ut mer exakta datum kan du använda funktionen `add_time` istället för `add_days`. Denna funktion tar också hänsyn till tid på dagen, vilket kan vara användbart i vissa situationer.

Det är också möjligt att räkna ut en datum i framtiden eller tidigare baserat på en veckodag istället för antalet dagar. Till exempel kan du räkna ut nästa torsdag eller förra måndagen.

## Se också

- [Gleam dokumentation om Datatyper](https://gleam.run/dokumentation/standardbibliotek/datatyper#date)
- [En introduktion till Gleam](https://medium.com/@alfredwesterveld/en-introduktion-till-gleam-f81d6e79c9c9)
- [Gleam Bytecode-översikt](https://gleam.run/dokumentation/bytemodule_format#datum-typen)