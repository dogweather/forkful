---
title:                "Einen Datum aus einem String parsen"
html_title:           "Elixir: Einen Datum aus einem String parsen"
simple_title:         "Einen Datum aus einem String parsen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Das Parsen eines Datums aus einem String dient dazu, Text in ein tatsächliches Datum zu konvertieren. Das wird oft in Kommunikation mit APIs oder Datenbanken benötigt, da diese oft Zeichenketten verwenden.

## Wie geht das?

Das Parsen eines Datums ist in Gleam eine ziemlich einfache Aufgabe. Es verwendet das Modul `gleam/calendar`, welches Funktionen zur Date-Konversion bietet. Ein Beispiel ist unten aufgeführt.

```gleam
import gleam/calendar.{date, from_iso_year_week_day}

let datum = "2022-01-01"
let parsed_datum = date.from_iso_string(datum)

assert Ok(#date(2022, 1, 1)) = parsed_datum
```

Die Funktion `date.from_iso_string` wandelt einen String in ein Datum um. Bei erfolgreichem Parsen gibt die Funktion ein `Ok` zurück, ansonsten ein `Error`. 

## Tief einsteigen

Historisch wurde das Parsen von Datum eben seit jeher für die Kommunikation mit APIs und Datenbanken verwendet. Es gibt einige Alternativen wie Bibliotheken in anderen Programmiersprachen oder die Verwendung von integrierten Built-in-Möglichkeiten in Datenbanken. Die genaue Implementierung in Gleam hängt stark von der genauen gleam Version und dem Modul ab, das Sie verwenden.

Gleam verwendet das ISO 8601-Datum- und Zeitformatstandard. Datum-Strings, die dieses Format nicht einhalten, können nicht korrekt geparst werden.

Eine wichtige Sache zu beachten ist, dass parsen immer fehlerbehaftet ist. Das heißt, es kann immer Fehler geben, die Sie prüfen und behandeln müssen!

## Siehe auch

Schauen Sie sich weitere Ressourcen an, um mehr über das Parsen von Datum zu lernen:
2. [ISO 8601 Standard](https://www.iso.org/iso-8601-date-and-time-format.html)