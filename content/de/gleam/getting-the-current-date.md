---
title:    "Gleam: Die aktuelle Datum erhalten"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Warum

Das Abrufen des aktuellen Datums kann in vielen Programmiersprachen sehr nützlich sein. Es ermöglicht uns, dynamische Daten in unsere Anwendungen einzubinden und sie immer auf dem neuesten Stand zu halten. In Gleam ist dies dank einiger eingebauter Funktionen besonders einfach.

# Wie Geht Es

In Gleam können wir das aktuelle Datum mithilfe der Funktion `DateTime.now()` erhalten. Diese Funktion gibt eine Struktur zurück, die das aktuelle Datum, die Uhrzeit und die Zeitzone enthält. Wir können dann diese Struktur verwenden, um die einzelnen Komponenten des Datums abzurufen und in unserer Anwendung zu nutzen. Schauen wir uns dazu ein Beispiel an:

```Gleam
let date = DateTime.now()
IO.debug("Das aktuelle Datum ist:")
IO.debug("Jahr: {}", date.year)
IO.debug("Monat: {}", date.month)
IO.debug("Tag: {}", date.day)
```

Dieses Beispiel gibt das aktuelle Datum in die Konsole aus und gibt uns die Möglichkeit, mit den einzelnen Komponenten des Datums zu arbeiten. Hier ist eine mögliche Ausgabe:

```
Das aktuelle Datum ist:
Jahr: 2021
Monat: 4
Tag: 10
```

# Tiefer Eintauchen

In Gleam gibt es noch viele weitere Funktionen, die uns bei der Arbeit mit Datum und Uhrzeit unterstützen. Zum Beispiel können wir mit `DateTime.add()` ein Datum um eine bestimmte Anzahl an Tagen, Monaten oder Jahren erweitern. Oder mit `DateTime.diff()` können wir die Differenz zwischen zwei Datumswerten berechnen. Es lohnt sich, diese Funktionen genauer zu untersuchen, um das volle Potenzial von Gleam bei der Arbeit mit Datum und Uhrzeit auszuschöpfen.

# Siehe Auch

- [Gleam-Dokumentation zu Datum und Uhrzeit](https://gleam.run/documentation/standard-library/date-time)
- [Gleam-Zeitpaket auf GitHub](https://github.com/gleam-lang/time)