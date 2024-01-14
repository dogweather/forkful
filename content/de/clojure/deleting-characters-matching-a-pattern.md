---
title:                "Clojure: Entfernen von Zeichen, die einem Muster entsprechen"
programming_language: "Clojure"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist eine nützliche Aufgabe in der Programmierung. Es kann helfen, unerwünschte Zeichen aus Benutzereingaben zu entfernen oder Daten zu bereinigen, bevor sie in eine Datenbank eingefügt werden.

## Wie man es macht

Es gibt mehrere Möglichkeiten, um Zeichen in Clojure zu löschen, die einem bestimmten Muster entsprechen. Eine Möglichkeit ist die Verwendung der Funktion `clojure.string/replace`. Hier ist ein Beispiel, das alle Leerzeichen in einem String entfernt:

```Clojure
(require '[clojure.string :as str])

(str/replace "Hallo Welt" #" " "")
;; => "HalloWelt"
```

Eine weitere Möglichkeit ist die Verwendung von regulären Ausdrücken mit der Funktion `re-seq`. Hier ist ein Beispiel, das alle Zeichen entfernt, die keine Buchstaben oder Zahlen sind:

```Clojure
(re-seq #"[^\w]+" "1, 2, 3... Go!" )
;; => ("1" "2" "3" "Go")
```

## Tiefer eintauchen

Beim Löschen von Zeichen, die einem bestimmten Muster entsprechen, ist es wichtig, zu verstehen, wie reguläre Ausdrücke funktionieren. Reguläre Ausdrücke sind eine Sequenz von Zeichen, die ein Muster definieren, das in einem String gefunden werden soll. Sie können eine Vielzahl von Symbolen und Operatoren verwenden, um komplexere Muster zu erstellen.

Um tiefer in reguläre Ausdrücke einzutauchen, empfehle ich die Lektüre des offiziellen Clojure-Dokuments zu den Funktionen `re-pattern` und `re-seq`. Sie können auch verschiedene Online-Tutorials und Beispiele zu regulären Ausdrücken finden, um Ihr Verständnis zu vertiefen.

## Siehe auch

- [Offizielles Clojure-Dokument zu `re-pattern` und `re-seq`](https://clojure.github.io/clojure/clojure.string-api.html)
- [Reguläre Ausdrücke in Clojure](https://dzone.com/articles/regular-expressions-in-clojure)
- [Clojure String Funktionen](https://www.tutorialspoint.com/clojure/clojure_string_functions.htm)