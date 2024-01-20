---
title:                "Ein Datum in einen String umwandeln"
html_title:           "Java: Ein Datum in einen String umwandeln"
simple_title:         "Ein Datum in einen String umwandeln"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Datum in String konvertieren bezeichnet den Vorgang, bei dem ein Datumsobjekt in einen String umgewandelt wird. Programmierer machen das, um Daten leichter lesbar zu machen oder sie in Umgebungen zu verwenden, die nur Text verarbeiten.

## So geht's:
In Elixir verwenden wir die native Funktion Date.to_string/1, um ein Datum in einen String umzuwandeln. Hier ist ein einfacher Code:
```Elixir
iex > datum = Date.new!(2022, 1, 1)
iex > Date.to_string(datum)
"2022-01-01"
```
Der obige Code erzeugt ein Datum und wandelt es dann in einen String um.

## Tiefere Informationen
Historisch gesehen, in älteren Versionen von Elixir, mussten wir die Erlang-Funktion :calendar.universal_time_to_string verwenden, um Datum in String zu konvertieren. Aber jetzt, mit der Weiterentwicklung von Elixir, bleibt die Date.to_string-Funktion vorzuziehen.

Alternativen zum Konvertieren von Datum in String können die Verwendung von Drittlibrary, wie Timex, sein. Es bietet mehr Einstellungsmöglichkeiten bei der Formatierung des Strings.

Die Implementierung von Date.to_string in Elixir basiert auf dem ISO 8601-Datumsformat, was bedeutet, dass das erzeugte Stringformat immer "YYYY-MM-DD" ist.

## Siehe Auch
Weitere Informationen zum Arbeiten mit Daten in Elixir sind in den offiziellen Elixir-Dokumenten zu findern:

- Date — Elixir v1.12.3 : [Link](https://hexdocs.pm/elixir/Date.html)
- Timex Library : [Link](https://hexdocs.pm/timex/readme.html)

Examining the Date.to_string Source Code : [Link](https://github.com/elixir-lang/elixir/blob/v1.12/lib/elixir/lib/calendar/date.ex)