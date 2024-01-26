---
title:                "Generierung von Zufallszahlen"
date:                  2024-01-20T17:48:42.176312-07:00
model:                 gpt-4-1106-preview
simple_title:         "Generierung von Zufallszahlen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Generieren von Zufallszahlen ist der Prozess, bei dem Zahlen erzeugt werden, die nicht vorhersehbar sind. Programmierer brauchen sie für alles mögliche, von der Spielentwicklung über Simulationen bis zur Datenverschlüsselung.

## So geht's:
Hier wird gezeigt, wie du in Clojure Zufallszahlen erstellen kannst. Die `rand`-Funktion gibt dir eine Gleitkommazahl zwischen 0 und 1. Um eine Ganzzahl zu bekommen, benutze `rand-int`.

```Clojure
;; Eine Zufalls-Gleitkommazahl bekommen
(rand)

;; Eine Zufalls-Ganzzahl zwischen 0 (inklusive) und 10 (exklusive) bekommen
(rand-int 10)
```

Beispiel Output:
```Clojure
0.7092038062959585  ; Ergebnis von (rand)
7                   ; Ergebnis von (rand-int 10)
```

## Deep Dive:
Das Konzept der Zufallszahlen ist nicht neu. Schon in der Antike warfen Menschen Würfel. In der Informatik werden häufig Pseudozufallszahlen verwendet, die mit Algorithmen erstellt werden und nur scheinbar zufällig sind, wie es `rand` in Clojure tut.

Alternative Methoden umfassen die Nutzung spezialisierter Bibliotheken oder Hardwaregeräte, die echte Entropie aus physischen Phänomenen nutzen. In Clojure kannst du auch die `java.util.Random`-Klasse aus Java nutzen für verschiedene Arten von Zufallszahlen.

Die Implementierung in Clojure hängt von der JVM (Java Virtual Machine) ab und wurde so entwickelt, dass sie relativ einfach, aber effektiv ist. Die Funktion `rand` basiert auf `java.util.Random` und daher vererbt sie viele Charakteristika von Javas Implementierung der Zufallszahlen.

## Siehe Auch:
- Clojure-Dokumentation für Zufallszahlen: https://clojuredocs.org/clojure.core/rand
- Oracle Java-Dokumentation für `java.util.Random`: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html
- Eine ausführliche Diskussion über Zufallszahlen und deren Nutzung in der Softwareentwicklung: https://www.random.org/randomness/
