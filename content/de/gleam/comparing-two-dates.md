---
title:                "Vergleich von zwei Daten"
html_title:           "C#: Vergleich von zwei Daten"
simple_title:         "Vergleich von zwei Daten"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Vergleichen von zwei Daten ermöglicht es uns, festzustellen, welches Datum früher oder später ist. Dies ist in der Programmierung nützlich, um zeitbasierte Funktionalitäten wie Terminerinnerungen, Zeitpläne oder Fristberechnungen zu implementieren.

## So geht's:
In Gleam können wir das `time` Modul verwenden, um zwei Daten zu vergleichen. Im folgenden Beispiel werden wir zwei Daten miteinander vergleichen:

```gleam
import gleam/time.{..}

fn main() {
  let datum1 = time.now()
  let datum2 = time.next_second(datum1)

  let ist_früher = time.is_before(datum1, datum2)

  case ist_früher {
    True -> io.println("Datum1 ist früher als Datum2!")
    False -> io.println("Datum1 ist nicht früher als Datum2!")
  }
}
```
Die Ausgabe könnte so aussehen:

```
Datum1 ist früher als Datum2!
```

## Tiefer Tauchen:
Die Fähigkeit, Daten zu vergleichen, wurde erstmals in der frühen Ära der Computersysteme entwickelt und ist seitdem ein fundamentaler Aspekt der Programmierung. 

Es gibt verschiedene Alternativansätze zum Vergleich von Daten - einige Sprachen bieten eingebaute Datenvergleichsoperatoren (wie "<", ">" und "==" in Python), während andere, wie Gleam, spezifische Funktionen für den Zweck.

Die Implementierung des Vergleichs von Daten in Gleam ist relativ einfach und zugänglich. Gleam nutzt seine robuste Typsicherheit und Funktionen, um eine klare und konsistente Syntax zu bieten, was das Verständnis und die Verfolgung von Code auf lange Sicht erleichtert.

## Siehe auch:
Für weitere Lektüre und Ressourcen, lesen Sie bitte:
1. [Zeit- und datumsbezogene Funktionen in Gleam](https://docs.gleam.run/stdlib/time/)
2. [Gleam Github Repository](https://github.com/gleam-lang/gleam)
3. [Ein Vergleich von Datumsvergleichsfunktionen in verschiedenen Programmiersprachen](http://rosettacode.org/wiki/Date_comparison)