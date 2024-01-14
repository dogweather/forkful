---
title:    "Clojure: Zufallszahlen generieren"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Warum

Das Generieren von Zufallszahlen ist ein wichtiger Bestandteil der Programmierung und kann in verschiedenen Anwendungen nützlich sein, wie beispielsweise bei der Erstellung von Spielen, Simulationen oder Verschlüsselungstechniken.

# Wie man es macht

Um in Clojure Zufallszahlen zu generieren, verwenden wir die Funktion `rand`. Diese Funktion akzeptiert eine Zahl als Parameter und gibt eine Zufallszahl zwischen 0 (inklusive) und der angegebenen Zahl (exklusive) zurück.

```Clojure
(rand 10) ; Gibt eine Zufallszahl zwischen 0 und 10 zurück
(rand 100) ; Gibt eine Zufallszahl zwischen 0 und 100 zurück
```

Um eine ganze Zufallszahl zu erhalten, können wir die Funktion `int` verwenden, die die Nachkommastellen entfernt.

```Clojure
(int (rand 100)) ; Gibt eine ganze Zufallszahl zwischen 0 und 100 zurück
```

Wenn wir eine genauere Kontrolle über den Bereich der generierten Zufallszahlen haben wollen, können wir die Funktion `rand-range` verwenden. Diese Funktion akzeptiert zwei Parameter und generiert eine Zufallszahl zwischen dem ersten Parameter (inklusive) und dem zweiten Parameter (exklusive).

```Clojure
(rand-range 10 20) ; Gibt eine Zufallszahl zwischen 10 und 20 zurück
(rand-range 1 100) ; Gibt eine Zufallszahl zwischen 1 und 100 zurück
```

# Tiefergehende Erläuterung

Clojure verwendet den Mersenne Twister-Algorithmus zur Generierung von Zufallszahlen. Dieser Algorithmus ist im Allgemeinen schneller und auch gleichmäßiger als andere Algorithmen, was bedeutet, dass die generierten Zufallszahlen wahrhaft zufällig sind.

Es ist auch wichtig zu beachten, dass die Funktion `rand` nicht wirklich Zufallszahlen produziert, sondern eine deterministische Berechnung durchführt, die auf eine vorherige Zufallszahl als "Seed" zurückgreift. Dies bedeutet, dass, wenn wir dieselbe Seed-Zahl übergeben, wir immer dieselben Zufallszahlen erhalten werden.

# Siehe auch

- Dokumentation zur Funktion `rand`: https://clojuredocs.org/clojure.core/rand
- Dokumentation zur Funktion `int`: https://clojuredocs.org/clojure.core/int
- Dokumentation zur Funktion `rand-range`: https://clojuredocs.org/clojure.core/rand-range