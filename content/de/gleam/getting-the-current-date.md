---
title:                "Die aktuelle Datum erhalten"
html_title:           "Gleam: Die aktuelle Datum erhalten"
simple_title:         "Die aktuelle Datum erhalten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Warum

Das Abrufen des aktuellen Datums ist oft ein wichtiger Teil der Programmierung. Es ermöglicht es uns, zeitabhängige Aktionen auszuführen, Daten in einer bestimmten Reihenfolge zu organisieren und vieles mehr. In diesem Artikel werden wir uns ansehen, wie man das aktuelle Datum in Gleam bekommt.

## Wie man das aktuelle Datum in Gleam bekommt

Die einfachste Möglichkeit, das aktuelle Datum in Gleam zu bekommen, ist die `Time.now()` Funktion zu verwenden. Dies gibt uns ein `Date` Objekt zurück, das das aktuelle Datum und die Uhrzeit enthält.

```Gleam
let current_date = Time.now()
```

Wenn wir nur das Datum ohne Zeit benötigen, können wir die `Time.date()` Funktion verwenden.

```Gleam
let today = Time.date()
```

Um das Datum in einem bestimmten Format zu erhalten, können wir die `Time.format()` Funktion verwenden und dem Format eine Schnur übergeben.

```Gleam
let today_str = Time.format(today, "{YYYY}/{MM}/{D}")
```

Dies würde ein Datum im Format "YYYY/MM/D" zurückgeben, z.B. "2020/07/12". Für eine vollständige Liste der unterstützten Formate, schau dir die Dokumentation an.

## Tiefere Einblicke

Das `Date` Objekt, das von `Time.now()` zurückgegeben wird, besteht aus verschiedenen Komponenten wie Jahr, Monat, Tag, Stunde, Minute und Sekunde. Wir können auf diese Komponenten zugreifen, indem wir die entsprechenden Funktionen aufrufen, z.B. `Time.year()`, `Time.month()`, `Time.day()`, usw.

```Gleam
let year = Time.year(Time.now())
let month = Time.month(Time.now())
let day = Time.day(Time.now())
```

Außerdem können wir mithilfe der `Time.adjust()` Funktion ein bestimmtes Datum anpassen, indem wir die entsprechenden Parameter wie "year", "month" und "day" angeben.

```Gleam
let tomorrow = Time.adjust(Time.date(), [year => 2020, month => 7, day => 13])
```

Dies würde das Datum auf den 13. Juli 2020 festlegen. Für weitere Informationen zu den verfügbaren Funktionen und deren Verwendung, lies dir die offizielle Dokumentation durch.

## Siehe auch

- [Offizielle Dokumentation zu Gleam](https://gleam.run/documentation)
- [Beispielcode für die Verwendung von `Time`](https://github.com/gleam-lang/gleam/blob/master/examples/time.gleam)