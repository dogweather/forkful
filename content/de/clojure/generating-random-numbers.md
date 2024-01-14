---
title:                "Clojure: Generierung von Zufallszahlen"
programming_language: "Clojure"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Warum

Warum sollte man sich überhaupt mit der Generierung von Zufallszahlen beschäftigen? Nun, in der Programmierung kann diese Fähigkeit sehr nützlich sein, z.B. für Simulationen, Spiele, kryptographische Anwendungen oder auch nur zum Testen von Funktionen.

# Wie geht das?

Um in Clojure Zufallszahlen zu generieren, gibt es mehrere Möglichkeiten. Eine einfache Methode ist die Verwendung der `rand`-Funktion, die eine Zufallszahl zwischen 0 und 1 zurückgibt. Beispiel:

```Clojure
(rand) ; Output: 0.8477345227
```

Möchte man eine Zufallszahl innerhalb eines bestimmten Bereichs erhalten, kann man die `rand-int`-Funktion verwenden. Diese nimmt zwei Argumente entgegen, den unteren und den oberen Grenzwert. Beispiel:

```Clojure
(rand-int 1 10) ; Output: 7
```

Man kann auch eine Liste von Zufallszahlen mit der `repeatedly`-Funktion generieren, indem man ihr als Argument die Anzahl der gewünschten Werte sowie eine Funktion übergibt, die Zufallszahlen erzeugt. Beispiel:

```Clojure
(repeatedly 5 rand) ; Output: (0.627081839 0.3325115686 0.9662961649 0.47983 0.9021066034)
```

# Tiefer eintauchen

Die `rand`-Funktion verwendet einen Pseudo-Zufallszahlengenerator, der auf dem Mersenne-Twister-Algorithmus basiert. Das bedeutet, dass die generierten Zahlen nicht wirklich zufällig sind, sondern auf einer vorhersehbaren Berechnung basieren. Wenn man absolute Zufälligkeit benötigt, sollte man daher einen externen Zufallszahlengenerator verwenden.

Es ist auch möglich, den Zufallszahlengenerator zu initialisieren, um immer die gleiche Sequenz von Zahlen zu erhalten. Dafür kann man die `seed`-Funktion verwenden und ihr einen beliebigen Wert übergeben.

# Siehe auch

- Clojure Dokumentation zu Zufallszahlen: https://clojuredocs.org/clojure.core/rand
- Island of Clojure Artikel über Zufallszahlen: http://www.islandofclojure.com/clojure-random-numbers/