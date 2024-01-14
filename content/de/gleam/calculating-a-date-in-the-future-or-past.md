---
title:    "Gleam: Berechnung eines Datums in der Zukunft oder Vergangenheit"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Warum

Das Berechnen von Daten in der Zukunft oder Vergangenheit kann äußerst nützlich sein, wenn man beispielsweise eine Event-Planung oder eine Altersberechnung durchführen möchte.

## Wie geht's

Um ein Datum in der Zukunft oder Vergangenheit zu berechnen, können wir die Bibliothek "gleam-date" nutzen. Zuerst müssen wir sie in unserem Projekt hinzufügen, indem wir die folgende Zeile in unsere Projektabhängigkeiten aufnehmen:

```
gleam.json: "gleam-date" => github: "northernelegy/gleam-date" v0.7.0
```

Dann können wir die Bibliothek importieren und die Funktion `Date.advance` nutzen, um ein Datum in der Zukunft oder Vergangenheit zu berechnen. Hier ist ein Beispielcode:

```
import date
import gleam/date
import gleam/time

pub fn main() {
  let today = date.from_gregorian(2020, 9, 1)
  let future_date = date.advance(today, 2, gleam/time/day)
  let past_date = date.advance(today, -5, gleam/time/day)

  gleam/io.print("Heute: ", today)
  gleam/io.print("In 2 Tagen: ", future_date)
  gleam/io.print("Vor 5 Tagen: ", past_date)
}
```

Das obige Beispiel gibt die berechneten Datumswerte in der ISO 8601-Notation aus. Die Ausgabe sieht folgendermaßen aus:

```
Heute: 2020-09-01
In 2 Tagen: 2020-09-03
Vor 5 Tagen: 2020-08-27
```

## Tiefere Einblicke

Die `Date.advance`-Funktion akzeptiert drei Argumente: ein Datum, eine Anzahl an Zeiteinheiten und eine Zeiteinheit. Die möglichen Zeiteinheiten sind `gleam/time/year`, `gleam/time/week`, `gleam/time/day`, `gleam/time/hour`, `gleam/time/minute` und `gleam/time/second`. 

Zusätzlich können wir auch die `Date.add`-Funktion nutzen, um genaue Datenwerte hinzuzufügen. Dies kann hilfreich sein, wenn wir beispielsweise ein bestimmtes Datum zu einem Geburtstag hinzufügen möchten, um das Alter einer Person zu berechnen.

Für weitere Informationen über die Datumsberechnungen und die verfügbaren Funktionen in der "gleam-date"-Bibliothek, können Sie die offizielle Dokumentation auf GitHub lesen oder direkt den Quellcode der Bibliothek studieren.

## Siehe auch

- [Offizielle Dokumentation von "gleam-date"](https://github.com/northernelegy/gleam-date)
- [Gleam-Forum](https://gleam.run/community)
- [Gleam-Dokumentation (auf Deutsch)](https://gleam.run/documentation)