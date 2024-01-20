---
title:                "Datum aus einem String parsen"
date:                  2024-01-20T15:36:03.815596-07:00
html_title:           "Arduino: Datum aus einem String parsen"
simple_title:         "Datum aus einem String parsen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Parsen eines Datums aus einem String bedeutet, eine textbasierte Datumsangabe in ein Datumstyp umzuwandeln. Programmierer machen das, um Datumsangaben zu verarbeiten und zu manipulieren, die zunächst als Text vorliegen, wie z.B. Benutzereingaben oder Dateidaten.

## So geht's:
```gleam
// Angenommen, wir haben die Gleam-Standardbibliothek.
import gleam/erlang/time.{StringFormat, parse_from_string}

fn main() {
  let date_string = "2023-03-15"
  let format = "{year}-{month}-{day}"
  let result = parse_from_string(date_string, format)

  case result {
    Ok(date_time) -> io.println("Datum erfolgreich geparst: " ++ date_time.to_string())
    Error(e) -> io.println("Fehler beim Parsen: " ++ e)
  }
}
```

Beispiel Ausgabe:
```
Datum erfolgreich geparst: 2023-03-15T00:00:00Z
```

## Tieftauchgang
Historisch gesehen gab es viele Ansätze für das Datumsparsing, von simplen String-Split-Methoden bis zu komplexen Libraries wie Java's SimpleDateFormat. In Gleam können wir die kräftigen Funktionen der `erlang/time`-Bibliothek nutzen, die robuste Parsing-Optionen bietet. Alternativen wären das Nutzen von kundenspezifischen Lösungen oder anderen Libraries, falls spezielle Formatierungen oder Zeitberechnungen erforderlich sind. In der Implementierung muss besonders auf Fehlerbehandlung und Validierung der Eingabe geachtet werden, um Probleme wie ungültige Daten oder falsche Formatspezifikationen zu vermeiden.

## Ähnliche Quellen
- Gleam's `erlang/time` Moduldokumentation: https://hexdocs.pm/gleam/gleam_erlang_time.html
- Erlang's Zeitfunktionen für tiefergehende Formate und Parsing-Optionen: http://erlang.org/doc/man/calendar.html
- Rust's `chrono` Library, eine Inspiration für Zeitlibraries in anderen Sprachen: https://docs.rs/chrono/0.4.19/chrono/