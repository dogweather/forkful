---
title:                "Ausgabe von Debugging-Daten"
html_title:           "Clojure: Ausgabe von Debugging-Daten"
simple_title:         "Ausgabe von Debugging-Daten"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Warum

Wenn du an einem Clojure Programm arbeitest und Fehlerbeseitigung betreibst, kann das Drucken von Debug Ausgaben sehr hilfreich sein. Es ermöglicht dir, den Programmfluss zu verfolgen und zu verstehen, was genau im Code passiert.

## Wie geht man vor?

Um Debug Ausgaben in Clojure zu drucken, kann man die Funktion `println` verwenden. Diese Funktion gibt die übergebenen Argumente in der Konsole aus. Hier ist ein Beispiel:

```Clojure
(defn foo [x]
  (println "Der Wert von x ist" x)
  (+ x 2))

(foo 5)
```

Die Ausgabe dieses Codes ist:

```
Der Wert von x ist 5
=> 8
```

Man kann auch Variablen innerhalb von Debug Ausgaben ausgeben, um ihren aktuellen Wert zu sehen:

```Clojure
(defn bar []
  (let [x 10]
    (println "Der Wert von x ist" x)
    (+ x 5)))

(bar)
```

Die Ausgabe ist:

```
Der Wert von x ist 10
=> 15
```

## Tiefes Eintauchen

Es gibt einige Dinge, die man beachten sollte, wenn man Debug Ausgaben in Clojure verwendet. Zuerst ist es wichtig, die `println` Funktion nur für Debug Zwecke zu verwenden, da sie die Lesbarkeit des Codes beeinträchtigen kann.

Eine alternative Möglichkeit, Debug Ausgaben zu drucken, ist die Verwendung von `clojure.pprint/pprint`, welche eine schön formatierte Ausgabe erstellt. Hier ist ein Beispiel:

```Clojure
(require '[clojure.pprint :refer [pprint]])

(defn baz []
  (let [x 5
        y 10]
    (pprint {:x x :y y})))

(baz)
```

Die Ausgabe ist:

```
{:x 5,
 :y 10}
=> nil
```

Eine weitere Sache, die man beachten sollte, ist, dass man Debug Ausgaben in Funktionen verwenden sollte, die keine Seiteneffekte haben. Wenn die Funktion Seiteneffekte hat, wie zum Beispiel das Ändern von globalen Variablen, kann dies zu unerwarteten Ergebnissen führen, wenn man Debug Ausgaben verwendet.

## Siehe auch

- [Clojure Dokumentation über Debugging](https://clojure.org/guides/debugging)
- [Artikel über Debugging in Clojure](https://www.baeldung.com/clojure-debugging)