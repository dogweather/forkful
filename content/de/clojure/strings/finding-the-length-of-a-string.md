---
date: 2024-01-20 17:47:10.832223-07:00
description: "Die L\xE4nge eines Strings zu ermitteln, bedeutet, die Anzahl der Zeichen\
  \ in diesem String zu z\xE4hlen. Programmierer tun das, um Textverarbeitungsaufgaben\u2026"
lastmod: '2024-03-13T22:44:53.411120-06:00'
model: gpt-4-1106-preview
summary: "Die L\xE4nge eines Strings zu ermitteln, bedeutet, die Anzahl der Zeichen\
  \ in diesem String zu z\xE4hlen."
title: "Ermittlung der Zeichenkettenl\xE4nge"
weight: 7
---

## So geht's:
Clojure bietet eine einfache Funktion, `count`, um die Länge eines Strings zu bekommen:

```Clojure
(defn string-length [s]
  (count s))

(println (string-length "Hallo Welt")) ; Ausgabe: 10
```
Die `string-length` Funktion nimmt einen String `s` und gibt seine Länge zurück.

## Tiefer Tauchen:
Historisch gesehen benutzten viele Sprachen eine Funktion ähnlich `strlen` in C, um Strings zu zählen. Clojure, erbaut auf der JVM, profitiert von Java's robusten String-Fähigkeiten, setzt aber auf Simplizität mit `count`.

Es gibt Alternativen, wie das Benutzen von `.length()` Java-Interoperabilität:

```Clojure
(println (.length "Hallo Welt")) ; Ausgabe: 10
```
Aber `count` ist idiomatischer in Clojure.

Implementationstechnisch sollte man beachten, dass `count` in Clojure constant-time performance für Strings und andere sequentielle Typen liefert, im Unterschied zu anderen Operationen, die möglicherweise durch die Sequenz iterieren müssen.

## Siehe Auch:
- Clojure Docs für `count`: [ClojureDocs - count](https://clojuredocs.org/clojure.core/count)
- Einführung in Clojure's Sequenzen: [Clojure - Sequences](https://clojure.org/reference/sequences)
- Java Interoperability Guide: [Clojure Java Interop](https://clojure.org/reference/java_interop)
