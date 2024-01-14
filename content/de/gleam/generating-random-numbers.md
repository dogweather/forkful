---
title:                "Gleam: Zufallszahlen erzeugen"
programming_language: "Gleam"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/gleam/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Generieren von Zufallszahlen ist ein wichtiger Bestandteil vieler Programmiertätigkeiten. Egal ob für Spiele, Simulationen oder Sicherheitsmaßnahmen, häufig ist eine zufällige Komponente notwendig. In Gleam gibt es einfache und effiziente Methoden, um zufällige Zahlen zu erzeugen.

## Wie geht es

Um Zufallszahlen in Gleam zu generieren, verwenden wir die eingebaute "rand" Bibliothek. Zuerst müssen wir sie importieren und dann können wir die Funktion "generate" aufrufen, um eine zufällige Ganzzahl zu erhalten.

```Gleam
import rand

let my_random_number = rand.generate()
```

Durch Angabe von zwei ganzen Zahlen als Argumente können wir den Bereich einschränken, in dem die zufällige Zahl generiert wird.

```Gleam
let my_random_number = rand.generate(1, 10)    // gibt eine zufällige Zahl zwischen 1 und 10 zurück
```

Für Gleitkommazahlen können wir die Funktion "uniform" verwenden, die eine zufällige Fließkommazahl zwischen 0 und 1 zurückgibt. Auch hier können wir mit zwei Argumenten den Bereich einschränken und somit eine zufällige Gleitkommanummer im gewünschten Bereich erhalten.

```Gleam
let my_random_float = rand.uniform()           // gibt eine zufällige Gleitkommazahl zwischen 0 und 1 zurück
let my_random_float = rand.uniform(10.0, 20.0) // gibt eine zufällige Gleitkommazahl zwischen 10 und 20 zurück
```

## Tiefere Einblicke

Gleam verwendet den Mersenne-Twister-Algorithmus für die Generierung von Zufallszahlen. Dieser Algorithmus ist bekannt für seine hohe Qualität und Periodizität, was bedeutet, dass die generierten Zahlen gleichmäßig verteilt sind und sich nicht wiederholen werden, es sei denn, der gleiche Seed-Wert wird verwendet. Dieser Seed-Wert kann auch im "generate" -Befehl angegeben werden, um eine vorgegebene Zahlenfolge zu erhalten.

In Gleam gibt es auch die Möglichkeit, benutzerdefinierte Generatoren zu erstellen, indem man die "Generator" -Struktur verwendet und benutzerdefinierte Funktionen für die Generierung von Zufallszahlen implementiert. Dies ermöglicht eine größere Flexibilität und Kontrolle über die generierten Zahlen.

## Siehe auch

- [Gleam-Dokumentation über Zufallszahlen](https://gleam.run/documentation/stdlib/rand)
- [Weitere Informationen zu Mersenne-Twister-Algorithmus] (https://de.wikipedia.org/wiki/Mersenne-Twister)
- [Tutorial zur Erstellung benutzerdefinierter Generatoren in Gleam](https://gleam.run/how-to/generators)