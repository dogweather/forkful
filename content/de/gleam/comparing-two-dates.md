---
title:                "Vergleich von zwei Daten"
date:                  2024-01-20T17:33:01.164188-07:00
model:                 gpt-4-1106-preview
simple_title:         "Vergleich von zwei Daten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Gleam-Datumsvergleiche unter der Lupe

## Was & Wozu?
Ein Datumsvergleich checkt, ob ein Datum vor, nach oder am gleichen Tag wie ein anderes ist. Programmierer nutzen das, um Zeitabläufe zu steuern, Gültigkeiten zu prüfen oder Events zu planen.

## So geht’s:
In Gleam gibt es (zum jetzigen Zeitpunkt Anfang 2023) keine eingebaute Datumsbibliothek. Ihr müsst also eine externe Bibliothek wie `chronotope` oder ein natives Erlang-Paket verwenden. Hier ist ein Beispiel, wie man zwei Daten mit `chronotope` vergleicht:

```gleam
import chronotope.{Date}

fn compare_dates(date1: Date, date2: Date) {
  case date1 < date2 {
    True -> "Date1 ist früher als Date2"
    False -> "Date1 ist nicht früher als Date2"
  }
}

fn main() {
  let date1 = Date.from_iso8601!("2023-03-01")
  let date2 = Date.from_iso8601!("2023-04-01")
  compare_dates(date1, date2)
}
```

Erwartete Ausgabe:

```
"Date1 ist früher als Date2"
```

## Tiefgang:
Datumsvergleiche sind elementar in der Programmierung und haben eine lange Geschichte. Früher erfolgten diese oft als einfache String-Vergleiche, was fehleranfällig war. Heutzutage nutzen wir dafür spezialisierte Bibliotheken, die auf Kalendersystemen und Zeitzonen basieren.

Erlang und Elixir bieten robuste Möglichkeiten für die Datumsverarbeitung, die auch in Gleam verwendet werden können. Allerdings erfordert das Verständnis dafür oft ein tieferes Eintauchen in die funktionale Programmierung und die Erlang-Welt. Mit Gleam profitieren wir von der Typsicherheit und den modernen Features der Sprache, während wir auf erprobtes Backend zugreifen.

## Siehe auch:

- Erlang-Datumsfunktionen: [erlang.org/doc/man/calendar.html](http://erlang.org/doc/man/calendar.html)
- Zeit und Datumsbehandlung in Elixir: [hexdocs.pm/elixir/1.12/Date.html](https://hexdocs.pm/elixir/1.12/Date.html)
