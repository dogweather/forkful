---
title:                "Zwei Daten vergleichen"
html_title:           "Gleam: Zwei Daten vergleichen"
simple_title:         "Zwei Daten vergleichen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Was & Warum?

Vergleichen von zwei Daten ist eine gebräuchliche Programmieraufgabe, bei der zwei Datumsangaben miteinander verglichen werden, um herauszufinden, welches Datum früher oder später liegt. Diese Aufgabe ist wichtig, um das Verhalten von Programmen zu steuern, die von Zeitangaben abhängig sind.

# Wie gehts?

Man kann zwei Datumsangaben verwenden, um sie miteinander zu vergleichen. Mit der eingebauten Funktion `Date.compare` in Gleam können wir zwei Zeiten direkt vergleichen. Die Funktion gibt `Order` zurück, das angibt, ob das erste Datum früher (`LT`), später (`GT`) oder gleich (`EQ`) dem zweiten Datum ist.

```Gleam
let now = Date.now()
let lastWeek = Date.from_calendar(2020, 7, 21)
let order = Date.compare(now, lastWeek)
```

Das obige Beispiel zeigt, wie wir `Date.compare` verwenden, um das aktuelle Datum `now` mit dem Datum der vergangenen Woche `lastWeek` zu vergleichen. In diesem Fall würden wir `GT` erhalten, da `lastWeek` früher als `now` ist.

# Tiefere Einblicke

Das Vergleichen von Daten ist eine gängige Programmieraufgabe, die jedoch in der Vergangenheit zu einigen Problemen führte. In älteren Programmiersprachen wie C und Java mussten wir komplexe Berechnungen durchführen, um zwei Daten zu vergleichen. Dank moderner Sprachen wie Gleam wird dieser Prozess viel einfacher und fehlerfreier.

Alternativ zur `Date.compare` Funktion gibt es auch die `Date.less_than` und `Date.greater_than` Funktionen, die ebenfalls zur Vergleich von Daten verwendet werden können. Diese Funktionen geben einfach einen booleschen Wert zurück, anstatt sich auf die Bestimmung der Reihenfolge zu konzentrieren.

In der Implementierung wird `Date.compare` letztendlich auf die `compare` Funktion des `Date.OrderedType` Protokolls zurückgeführt. Dies ermöglicht es, die Funktion für benutzerdefinierte Datumsformate zu implementieren.

# Siehe auch

- [Gleam Dokumentation über die Date Library](https://gleam.run/articles/dates)
- [Gleam Protocols](https://gleam.run/articles/protocols)
- [Gleam Standard Library](https://gleam.run/lib/standard)