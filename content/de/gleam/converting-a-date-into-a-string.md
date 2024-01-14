---
title:    "Gleam: Umwandlung eines Datums in einen String"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von Datum in einen String ist eine grundlegende Programmieraufgabe, die in vielen Anwendungen erforderlich ist. Zum Beispiel kann es verwendet werden, um ein Datum in einem bestimmten Format auf einer Website anzuzeigen oder um ein Datum in einer Datenbank zu speichern. Die Verwendung von Gleam ermöglicht es Entwicklern, diese Aufgabe auf einfache und effiziente Weise zu erledigen.

## Wie geht das?

```Gleam
// Erstellen eines Datums
let date = Date.create(2021, 3, 20)

// Konvertieren in einen String
let string = date |> Date.to_string

// Ausgabe: "2021-03-20T00:00:00Z"
```

Die `Date`-Bibliothek in Gleam enthält die Funktion `to_string`, die ein Datum in das ISO-8601-Format konvertiert. Dies ist ein Standardformat, das von vielen Systemen unterstützt wird.

Um ein Datum in einem bestimmten Format anzuzeigen, kann der `to_string`-Funktion ein Argument übergeben werden, z.B. `Date.to_string(date, "%d.%m.%Y")` würde das Datum im Format "20.03.2021" ausgeben.

Weitere nützliche Funktionen der `Date`-Bibliothek sind z.B. `now`, um das aktuelle Datum und die aktuelle Uhrzeit zu erhalten, oder `parse`, um einen String in ein Datum zu konvertieren.

## Tiefergehende Informationen

Die `Date`-Bibliothek basiert auf dem `Calendar`-Modul, das wiederum auf der bekannten `calendar`-Bibliothek in Erlang aufbaut. Dies ermöglicht es Gleam, komplexe Zeitberechnungen durchzuführen und Zeitstempel in verschiedenen Zeitzonen korrekt zu verarbeiten.

Es ist auch möglich, benutzerdefinierte Zeit- und Datumsformate mithilfe der `strf`-Funktion zu erstellen, die die Erlang-Funktion `strftime` verwendet. Mit dieser Funktion können Entwickler ihre Datums- und Zeitangaben auf ihre spezifischen Anforderungen und Präferenzen zuschneiden.

## Siehe auch

- [Offizielle Date-Bibliothek in Gleam](https://gleam.run/lib/date)
- [Erlang calendar library](https://erlang.org/doc/man/calendar.html)
- [ISO-8601 Date and Time format](https://www.iso.org/iso-8601-date-and-time-format.html)