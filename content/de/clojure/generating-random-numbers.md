---
title:    "Clojure: Zufallszahlen erzeugen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Warum: Generierung von Zufallszahlen in Clojure

Die Generierung von Zufallszahlen ist ein wichtiger Bestandteil vieler Programmieraufgaben. Oftmals werden Zufallszahlen benötigt, um bestimmte Abläufe oder Entscheidungen in Programmen zu simulieren oder zufällige Daten zu generieren. In Clojure gibt es verschiedene Möglichkeiten, Zufallszahlen zu generieren und in diesem Blog-Artikel möchten wir uns damit beschäftigen, wie das funktioniert.

## Wie es geht

Um in Clojure Zufallszahlen zu generieren, können wir die Funktion "rand" aus der Standartbibliothek `clojure.core` verwenden. Diese Funktion akzeptiert eine optionalen Parameter `n`, der die maximale Zahl angibt, die generiert werden soll. Wenn kein Parameter angegeben wird, werden Zufallszahlen im Bereich von 0 (inklusive) bis 1 (exklusive) generiert.

```Clojure
(rand) ;; generiert eine Zufallszahl zwischen 0 und 1
;; Beispiel-Ausgabe: 0.452098327

(rand 10) ;; generiert eine Zufallszahl zwischen 0 und 10
;; Beispiel-Ausgabe: 5.34267589
```

Alternativ können wir die Funktion "rand-int" verwenden, um ganze Zufallszahlen zu generieren. Diese Funktion erwartet ebenfalls einen optionalen Parameter `n`, der als obere Grenze für die generierten Zufallszahlen dient.

```Clojure
(rand-int 100) ;; generiert eine ganze Zufallszahl zwischen 0 und 100
;; Beispiel-Ausgabe: 73
```

Natürlich können wir auch eigene Grenzen für die generierten Zufallszahlen definieren, indem wir die "range" Funktion verwenden. Diese Funktion generiert eine Sequenz von Zahlen von einem Startwert (inklusive) bis zu einem Endwert (exklusive) mit einem optionalen Schritt, der standardmäßig 1 beträgt.

```Clojure
(range 10) ;; generiert eine Sequenz von Zahlen von 0 (inklusive) bis 10 (exklusive) mit einem Schritt von 1
;; Beispiel-Ausgabe: (0 1 2 3 4 5 6 7 8 9)

(range 20 30 2) ;; generiert eine Sequenz von Zahlen von 20 (inklusive) bis 30 (exklusive) mit einem Schritt von 2
;; Beispiel-Ausgabe: (20 22 24 26 28)
```

Um nun eine zufällige Zahl aus dieser Sequenz zu wählen, können wir die "rand-nth" Funktion verwenden, die eine Sequenz akzeptiert und eine zufällige Element aus dieser zurückgibt.

```Clojure
(rand-nth (range 10)) ;; wählt eine zufällige Zahl aus der Sequenz (0 1 2 3 4 5 6 7 8 9)
;; Beispiel-Ausgabe: 7
```

## Tiefergehende Informationen

Die "rand" und "rand-int" Funktionen verwenden den Systemzeitpunkt als Seed für die Zufallszahlengenerierung. Das bedeutet, dass jedes Mal, wenn die Funktion aufgerufen wird, eine neue Zufallszahl generiert wird. Wenn wir jedoch eine bestimmte Zufallszahl mehrmals verwenden möchten, z.B. in einer Schleife, sollten wir den Seed manuell setzen, indem wir die Funktion "set-seed!" verwenden.

```Clojure
(set-seed! 2020) ;; setzt den Seed auf die Zahl 2020
(rand) ;; generiert nun immer dieselbe Zufallszahl
;; Beispiel-Ausgabe: 0.5603120015123885
```

Es ist auch möglich, benutzerdefinierte Seeds zu erstellen, indem wir die "seed-random" Funktion verwenden und einen Integer-Wert übergeben. Dadurch können wir sicherstellen, dass bei jedem Aufruf der Funktion dieselbe Zufallszahl generiert wird.

```Clojure
(seed-random 1234)
(rand)
;; Beispiel-Ausgabe: 0.11474264970968126

(seed-random 1234)
(rand)
;; Beispiel-Ausgabe: 0.11474264970968126
```

## Siehe auch

Weitere Informationen und Beispiele zur Zufallszahlen-Generierung in Clojure finden Sie in der offiziellen Dokumentation: 
- https://clojure.org/api