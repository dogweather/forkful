---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parse von Datum mit Elixir

## Was & Warum?

Ein Datum aus einem String zu parsen bedeutet, einen Text-String in ein brauchbares Datum umzuwandeln. Programmierer machen das, weil sie oft Daten aus Textdateien, Datensätzen und Benutzereingaben verwenden müssen.

## Wie zu:

In Elixir wird das Parsen eines Datums aus einem String normalerweise mit dem Modul DateTime oder Date durchgeführt.

```elixir
datum_string = "2023-11-22 14:38:43Z" # String mit Datum und Uhrzeit
{:ok, datum} = DateTime.from_iso8601(datum_string)
IO.inspect(datum)
```
Ausführung des obigen Skripts gibt:
```elixir
%DateTime{year: 2023, month: 11, day: 22, hour: 14, minute: 38, second: 43, utc_offset: 0, time_zone: "Etc/UTC"}
```
## Deep Dive

Historisch gesehen erforderte das Parsen von Datumsstrings im Allgemeinen entweder benutzerdefinierten Code oder spezialisierte Bibliotheken. Elixir hat jedoch ab Version 1.3 Standardmodule für Datum und Zeit eingeführt. Es lohnt sich immer, auf dem neuesten Stand zu bleiben.

Als alternative Methode könnten Sie auch die Library `timex` nutzen, die mächtige und flexible Tools für die Arbeit mit Datum und Zeit bietet. Dennoch bevorzugen viele Entwickler die eingebauten Methoden, um Abhängigkeiten zu minimieren.

Die `DateTime.from_iso8601/1`-Funktion in Elixir überprüft das Format streng. Wenn der String nicht die richtige ISO 8601-Syntax hat, schlägt die Funktion fehl.

## Siehe auch

- Offizielle Elixir-Dokumentation zum Umgang mit Datums- und Zeitangaben: [Elixir DateTime official documentation](https://hexdocs.pm/elixir/DateTime.html#content)