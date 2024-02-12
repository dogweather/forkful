---
title:                "Ermittlung der Zeichenkettenlänge"
aliases: - /de/clojure/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:10.832223-07:00
model:                 gpt-4-1106-preview
simple_title:         "Ermittlung der Zeichenkettenlänge"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge eines Strings zu ermitteln, bedeutet, die Anzahl der Zeichen in diesem String zu zählen. Programmierer tun das, um Textverarbeitungsaufgaben durchzuführen, wie Datenvalidierung oder um Speicheranforderungen abzuschätzen.

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
