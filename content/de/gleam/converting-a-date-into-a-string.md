---
title:                "Datum in einen String umwandeln"
date:                  2024-01-20T17:36:32.511218-07:00
model:                 gpt-4-1106-preview
simple_title:         "Datum in einen String umwandeln"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Ein Datum in einen String umzuwandeln bedeutet, es in eine lesbare Textform zu bringen – praktisch für Nutzeranzeige und Speicherung. Programmierer machen das, um Daten handhabbar und menschenlesbar zu machen.

## So geht's:
```gleam
import gleam/calendar.{Date}
import gleam/erlang

// Datum erstellen
let mein_date = Date(from_iso_year_month_day: #(2023, 3, 14))
assert Ok(mein_date) = mein_date

// Datum zu String konvertieren
let datum_string = erlang.date_to_string(mein_date)
assert Ok("2023-03-14") = datum_string
```

Ausgabe:
```
"2023-03-14"
```

## Tiefgang
Historisch gesehen folgen Datumsstrings oft dem ISO 8601-Standard, da dieser international klar und vergleichbar ist. Alternativen zur Umwandlung können Bibliotheken wie `calendar` in anderen Sprachen sein oder Funktionen wie `format_date` zur individuellen Anpassung des Formats. In Gleam wird die Konvertierung effizient mit erlang-Funktionen gehandhabt, die zuverlässige Pattern-Matching und Typsicherheit bieten.

## Weiterführendes
- Gleam-Dokumentation: [https://gleam.run/](https://gleam.run/)
- ISO 8601-Standard: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)
- Erlang's calendar module: [http://erlang.org/doc/man/calendar.html](http://erlang.org/doc/man/calendar.html)
