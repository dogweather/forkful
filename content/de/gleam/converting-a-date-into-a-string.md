---
title:                "Gleam: Umwandlung eines Datums in eine Zeichenfolge"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Konvertieren von einem Datum in eine Zeichenfolge ist oft notwendig, um die Daten in einem verständlichen Format anzuzeigen oder zu speichern. Zum Beispiel kann es hilfreich sein, ein Datum als Teil eines Dateinamens zu formatieren oder es in einer E-Mail zu verwenden.

## Wie man es macht

Die Gleam-Programmiersprache bietet einfache Möglichkeiten, ein Datum in eine lesbare Zeichenfolge umzuwandeln. Das folgende Beispiel zeigt, wie man das aktuelle Datum in das Format "TT.MM.JJJJ" umwandeln kann:

```Gleam
// Importiere das Date-Modul
import date.{Format, today}

// Konvertiere das aktuelle Datum in ein String-Format
let date_string =
  today()
  |> Format.to_string("%d.%m.%Y")

// Gib das Ergebnis aus
IO.print(date_string) // => 02.03.2021
```

Natürlich können auch andere Datumsformate verwendet werden, wie im [Gleam Date Modul] (https://gleam.run/modules/date.html) beschrieben wird.

## Tieferer Einblick

Bei der Konvertierung eines Datums in eine Zeichenfolge gibt es einige wichtige Dinge zu beachten. Zum Beispiel sollte man immer die Zeitzone des Datums angeben, um Verwirrungen zu vermeiden. Außerdem können auch verschiedene Datums- und Zeitformate verwendet werden, je nach Anforderungen und Standards.

Ein weiterer wichtiger Faktor ist die Performance. Wenn man viele Datums-Konvertierungen in seinem Code verwendet, sollte man auf effiziente Methoden zurückgreifen, um die Ausführungsgeschwindigkeit zu optimieren. Gleam bietet dafür viele hilfreiche Tools und Funktionen.

## Siehe auch

- [Gleam Date Modul] (https://gleam.run/modules/date.html)
- [Gleam String Format Modul] (https://gleam.run/modules/string.html#string-format)

Vielen Dank fürs Lesen! Wir hoffen, dass dieser Artikel hilfreich für Sie war und Ihnen dabei geholfen hat, Dates in Strings in Gleam zu konvertieren. Vergessen Sie nicht, die oben verlinkten Ressourcen zu konsultieren, um noch mehr über die Möglichkeiten von Gleam zu erfahren. Happy coding!