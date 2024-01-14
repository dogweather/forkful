---
title:                "Gleam: Das aktuelle Datum erhalten"
simple_title:         "Das aktuelle Datum erhalten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums kann eine nützliche Funktion in der Programmierung sein, um zeitabhängige Aktionen auszuführen, z. B. das Erstellen von täglichen Backups oder das Anzeigen des aktuellen Datums in einer Anwendung.

## How To

Die aktuelle Datums- und Uhrzeitfunktion ist in Gleam durch das `Calendar` Paket verfügbar. Sie kann einfach importiert werden, indem man `use Calendar` am Anfang des Codes hinzufügt. Dann kann das aktuelle Datum mit der `today` Funktion aufgerufen werden, die einen `Calendar.Date`-Datentyp zurückgibt.

```Gleam
use Calendar

let current_date = Calendar.today()
```

Um das aktuelle Datum in einem bestimmten Format zu erhalten, kann die `format` Funktion verwendet werden, die das Datum als Zeichenkette zurückgibt. Zum Beispiel:

```Gleam
let current_date_str = Calendar.format(current_date, "%Y-%m-%d")
```

Der obige Code würde das Datum im Format "Jahr-Monat-Tag" ausgeben, z. B. 2021-09-01.

Um auch die aktuelle Uhrzeit abzurufen, kann die `now` Funktion verwendet werden, die einen `Calendar.DateTime`-Datentyp zurückgibt. Dieser Datentyp kann dann ebenfalls mit der `format` Funktion in das gewünschte Format gebracht werden.

## Deep Dive

Die `Calendar` Bibliothek basiert auf dem `Erlang Calendar` Modul, was bedeutet, dass die Datums- und Uhrzeitmanipulation in Gleam auch die gleichen Funktionen bereitstellt wie in Erlang. Dies beinhaltet Funktionen wie zum Beispiel das Hinzufügen von Tagen zu einem Datum, das Vergleichen von Datumswerten und vieles mehr.

Eine Besonderheit von Gleam ist, dass Datumsangaben nicht veränderbar sind, d.h. die Funktionen, die sie manipulieren sollen, geben immer ein neues Datum zurück, anstatt das ursprüngliche zu ändern. Dies kann dazu beitragen, unerwartete Seiteneffekte zu vermeiden.

## Siehe auch

- [Gleam Dokumentation](https://gleam.run/)
- [Erlang Calendar Modul](https://erlang.org/doc/man/calendar.html)
- [Einführung in die Zeit- und Datumsmanipulation in Gleam](https://simplabs.com/blog/2021/03/16/exploring-time-date-manipulation-in-gleam)