---
title:    "Gleam: Vergleich von zwei Datumsangaben"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Warum

Das Vergleichen von zwei Daten ist ein wichtiger Schritt in der Programmierung, um zu überprüfen, ob ein Datum vor oder nach einem bestimmten Zeitpunkt liegt. Es ist auch nützlich, um zu berechnen, wie viel Zeit zwischen den beiden Daten vergangen ist.

## Wie man

In Gleam gibt es eine Funktion namens `Date.compare`, die es uns ermöglicht, zwei Daten zu vergleichen. Sie nimmt zwei Argumente, die beide vom Typ `Date` sind, und gibt entweder `Order.Less`, `Order.Equal` oder `Order.Greater` zurück, je nachdem, ob das erste Datum vor, gleich oder nach dem zweiten Datum liegt.

```Gleam
let date1 = Date.new(2021, 10, 1)
let date2 = Date.new(2021, 9, 1)

let result = Date.compare(date1, date2)

io.println(result) // Gibt Order.Greater aus
```

In diesem Beispiel vergleichen wir zwei Daten, `date1` und `date2`, die im Abstand von einem Monat liegen. Da `date1` später als `date2` liegt, wird `Order.Greater` zurückgegeben.

## Tiefer Einblick

Wenn wir uns tiefer mit dem Vergleichen von Daten beschäftigen, gibt es einige wichtige Dinge zu beachten. Zum Beispiel ist es wichtig zu wissen, dass `Date.compare` die Zeitzone nicht berücksichtigt. Das heißt, wenn wir zwei Daten vergleichen, die in verschiedenen Zeitzonen liegen, wird dies nicht beachtet und könnte zu unerwarteten Ergebnissen führen.

Es ist auch wichtig zu beachten, dass `Date.compare` nicht nur auf Daten beschränkt ist, sondern auch auf andere Datentypen wie `DateTime` und `Time` angewendet werden kann. Dabei gilt die gleiche Logik wie bei der Vergleich von Daten.

## Siehe auch

- [Gleam Dokumentation zu Datum und Zeit](https://gleam.run/docs/stdlib/datetime)
- [Ein Leitfaden zum Umgang mit Datum und Zeit in Gleam](https://medium.com/@gleamlang/taming-dates-and-times-in-gleam-2a4907c06f87)
- [Date and Time Libraries im Vergleich für Gleam](https://github.com/gleam-lang/date-time-libraries-comparison)