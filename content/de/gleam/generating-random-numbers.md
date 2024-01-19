---
title:                "Zufallszahlen generieren"
html_title:           "Arduino: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?

Zufallszahlen sind numerische Werte, die durch einen Prozess generiert werden, der so gestaltet ist, dass jedes mögliche Ergebnis genauso wahrscheinlich ist. Programmierer verwenden Zufallszahlen in einer Vielzahl von Anwendungen, wie zum Beispiel in Spielen, in Verschlüsselungsalgorithmen oder bei Simulationen.

## So geht's:

Der Gleam-Code zum Erzeugen von Zufallszahlen in der aktuellen Version ist recht einfach. Hier ist ein Beispiel:

```Gleam
import gleam/random.{Generator, new}

fn main() -> Nil {
  let generator = new(12345)
  let (number, generator) = Generator.int(0, 100, generator)
  io.println(number)
}
```

Dieser Code erzeugt eine Zufallszahl zwischen 0 und 100. Die Ausgabe könnte so aussehen:

```Gleam
57
```

## Vertiefung

Historisch gesehen haben Programmiersprachen Zufallszahlen durch Algorithmen wie die lineare Kongruenzmethode generiert. Heute gibt es jedoch weitere, teilweise komplexere Methoden, wie Mersenne Twister oder Xorshift.

Alternative Ansätze in Gleam umfassen die Verwendung spezieller Funktionen oder Bibliotheken. Beachten Sie jedoch, dass die Verwendung von Zufallszahlen auf bestimmte Weise nicht völlig zufällig sein kann, da sie auf Algorithmen basieren und daher vorhersehbar sind.

In Gleam wird der Zufallszahlengenerator so implementiert, dass er einen Initialwert benötigt (in diesem Fall die Zahl 12345) und eine Funktion zum Generieren einer neuen Zufallszahl in einem angegebenen Bereich.

## Siehe auch

- Gleam's [random module](https://hexdocs.pm/gleam_stdlib/gleam/random/index.html) Dokumentation bietet detailliertere Informationen und Beispiele.
- Für einen technischen Überblick über Zufallszahlengeneratoren, lesen Sie die [Wikipedia-Seite](https://de.wikipedia.org/wiki/Pseudozufallszahl).
- Für eine ausführlichere Diskussion über die Nutzung von Zufallszahlen in der Programmierung, empfehle ich dieses [Artikel](https://www.johndcook.com/blog/2016/01/30/9999963619/).