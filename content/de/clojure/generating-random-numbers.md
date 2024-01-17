---
title:                "Zufallszahlen generieren"
html_title:           "Clojure: Zufallszahlen generieren"
simple_title:         "Zufallszahlen generieren"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Generieren von Zufallszahlen ist eine häufige Aufgabe, mit der Programmierer konfrontiert werden. Es beinhaltet die Erstellung von zufälligen Zahlen für verschiedene Zwecke, wie z.B. Simulationen, Spiele oder Sicherheitsmechanismen. Durch die Verwendung von Zufallszahlen können Programme auf eine Vielzahl von Eingaben reagieren und somit flexibler und robuster werden.

## Wie man es macht:
Das Generieren von Zufallszahlen in Clojure kann mithilfe der Funktion "rand" erfolgen, die eine zufällige Gleitkommazahl zwischen 0 und 1 zurückgibt. Dies kann durch Multiplikation mit einer gewünschten Größe und anschließender Umwandlung in eine ganze Zahl angepasst werden. Zum Beispiel: 
```
Clojure (rand) ; => 0.536764
Clojure (int (* 100 (rand))) ; => 58
```

## Tiefere Einblicke:
Das Generieren von Zufallszahlen kann auf verschiedene Arten und mit verschiedenen Algorithmen erfolgen. Die häufigste Methode ist jedoch die Verwendung eines Pseudozufallsgenerators, der eine scheinbar zufällige Sequenz von Zahlen erzeugt. In Clojure wird dies durch die Funktion "rand" implementiert, die den Mersenne-Twister-Algorithmus verwendet. Alternativ können auch echte Zufallszahlen mithilfe von externen Bibliotheken wie "java.util.Random" generiert werden.

## Siehe auch:
- Clojure-Dokumentation für rand: https://clojuredocs.org/clojure.core/rand
- Wikipedia-Artikel über Zufallszahlen: https://de.wikipedia.org/wiki/Zufallszahl
- Java-Dokumentation für java.util.Random: https://docs.oracle.com/javase/8/docs/api/java/util/Random.html