---
title:                "Zufällige Zahlen generieren"
html_title:           "Gleam: Zufällige Zahlen generieren"
simple_title:         "Zufällige Zahlen generieren"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Zufallszahlen gehören zu den grundlegenden Elementen in der Welt der Programmierung. Sie können verwendet werden, um Spiele zu erstellen, komplexe Berechnungen durchzuführen oder einfach nur um Spaß zu haben. Das Generieren von Zufallszahlen kann ein nützliches Werkzeug sein, das jeder Programmierer in seinem Repertoire haben sollte.

## So geht's

```Gleam
import gleam/random
random.int(1, 10)  // Gibt eine Zufallszahl zwischen 1 und 10 zurück
random.float(0.0, 1.0)  // Gibt eine Zufallszahl zwischen 0 und 1 zurück
```

Das Gleam-Standardmodul `random` bietet verschiedene Funktionen für das Generieren von Zufallszahlen. Mit `int(x, y)` können ganzzahlige Zufallszahlen zwischen `x` und `y` erzeugt werden, während `float(x, y)` für die Generierung von Fließkommazahlen verwendet werden kann.

Das Generieren von Zufallszahlen ist hilfreich, wenn Sie zufällige Daten für Tests oder Simulationen benötigen. Sie können auch Zufallszahlen verwenden, um eine zufällige Reihenfolge von Elementen in einer Liste zu generieren oder um Zufallsereignisse in einem Spiel zu simulieren.

## Deep Dive

Das Generieren von Zufallszahlen ist keine leichte Aufgabe. Es erfordert eine sorgfältige Balance zwischen Zufälligkeit und Wiederholbarkeit. Wenn die Zufallszahlen zu vorhersehbar sind, können sie die Integrität Ihrer Anwendungen beeinträchtigen. Glücklicherweise bietet Gleam integrierte Funktionen zur Generierung von qualitativ hochwertigen Zufallszahlen, die diesen Aspekt berücksichtigen.

Eine Möglichkeit, die Qualität der erzeugten Zufallszahlen zu verbessern, ist die Verwendung eines Seed-Werts. Dieser Wert dient als Ausgangspunkt für die Generierung der Zufallszahlen und kann angepasst werden, um unterschiedliche Ergebnisse zu erzielen. Gleam bietet eine `seed`-Funktion, die es Ihnen ermöglicht, einen beliebigen Wert als Seed zu verwenden und somit Zufallszahlen mit einer höheren Qualität zu erzeugen.

## Siehe auch

- [Gleam Dokumentation: Random Modul](https://gleam.run/modules/random/)
- [10 nützliche Anwendungen für Zufallszahlen in der Programmierung](https://code.tutsplus.com/de/tutorials/ten-usefulapplications-for-programming-with-random-numbers--net-2934)
- [Das Gesetz der großen Zahlen erklärt](https://www.statisticshowto.com/law-of-large-numbers/)