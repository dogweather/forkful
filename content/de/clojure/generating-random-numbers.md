---
title:                "Erstellen von Zufallszahlen"
html_title:           "Clojure: Erstellen von Zufallszahlen"
simple_title:         "Erstellen von Zufallszahlen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Warum
Jeder Programmierer weiß, dass es oft notwendig ist, Zufallszahlen zu generieren. Ob es nun für Spiele, Simulationen oder Sicherheitsfunktionen ist, wenn Sie mit Clojure arbeiten, gibt es einige einfache Möglichkeiten, um Zufallszahlen zu erstellen.

## Wie geht's?
Um eine Zufallszahl in Clojure zu generieren, verwenden Sie einfach die Funktion `rand`. Hier ist ein Beispiel:

```
Clojure (rand)
; => 0.3675405103616147
```
Dieses Beispiel erzeugt eine Zufallszahl zwischen 0 und 1. Um eine Zufallszahl innerhalb eines bestimmten Bereichs zu erhalten, können Sie die Hilfsfunktion `rand-int` verwenden. Hier ist ein Beispiel, um eine Zufallszahl zwischen 1 und 10 zu erstellen:

```
Clojure (rand-int 10)
; => 8
```

## Tieferer Einblick
Die Zufallszahlengenerierung in Clojure basiert auf dem Java-Random-Generator. Dies bedeutet, dass Sie dieselben Funktionen wie in Java verwenden können, z.B. `nextInt` für Zufallszahlen innerhalb eines bestimmten Intervalls oder `nextDouble` für Zufallszahlen mit Dezimalzahlen. Um auf den zugrundeliegenden Java-Random-Generator zuzugreifen, können Sie die Funktion `ThreadLocalRandom/current` verwenden.

## Siehe auch
- [Clojure Dokumentation über Zufallszahlen](https://clojuredocs.org/clojure.core/rand)
- [Java Random Generator Dokumentation](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html)
- [Clojure Cheat Sheet für Zufallszahlen](https://clojure.org/guides/cheatsheet#rand)