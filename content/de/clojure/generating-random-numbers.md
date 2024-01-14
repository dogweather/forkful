---
title:                "Clojure: Zufällige Zahlen generieren"
simple_title:         "Zufällige Zahlen generieren"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum

Das Generieren von zufälligen Zahlen ist in der Programmierung oft nützlich, zum Beispiel für die Erstellung von Tests oder Simulationen.

## Wie geht das?

Verwenden Sie die eingebaute Funktion `rand` in Clojure, um eine zufällige Gleitkommazahl zwischen 0 und 1 zu generieren.

```Clojure
(rand) ; Output: 0.487736238491231
```

Sie können auch `rand-int` verwenden, um eine zufällige Ganzzahl innerhalb eines bestimmten Bereichs zu generieren.

```Clojure
(rand-int 10) ; Output: 4
(rand-int 1 10) ; Output: 9
```

Wenn Sie mehr Kontrolle über die zufällige Sequenz benötigen, können Sie mit `set!` und `seed` eine benutzerdefinierte Seed-Zahl festlegen.

```Clojure
(set! *random-seed* 12345)
(rand) ; Output: 0.0735208995776269
(rand-int 10) ; Output: 5
```

## Tief eintauchen

Hinter den Kulissen verwendet Clojure die `java.util.Random` Klasse, um zufällige Zahlen zu generieren. Dadurch können Sie auch Methoden dieser Klasse verwenden, z.B. `nextInt` oder `nextDouble`.

```Clojure
(-> (java.util.Random.)
    (.nextInt 100)) ; Output: 42 
```

Wenn Sie wirklich tiefer in die Welt der Pseudozufallszahlengeneratoren eintauchen möchten, empfehle ich Ihnen, sich mit der mathematischen Theorie hinter ihnen zu beschäftigen. Clojure bietet auch das `core.memoize` Paket, das Funktionen speichert und dazu verwendet, bereits berechnete zufällige Zahlen erneut zu verwenden.

## Siehe auch

- [Dokumentation zu `rand` und `rand-int`](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/rand)
- [Clojure Seeds - Eine ausführliche Erklärung](https://clojuredocs.org/clojure.core/seed)
- [Details zur `java.util.Random` Klasse](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [`core.memoize` Dokumentation](https://clojure.github.io/core.memoize/)