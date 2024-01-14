---
title:    "Clojure: Die Länge eines Strings finden"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

# Warum

Das Finden der Länge eines Strings ist eine grundlegende Aufgabe in der Programmierung. Egal ob du gerade erst anfängst oder ein erfahrener Entwickler bist, es ist wichtig zu verstehen, wie dies in Clojure erreicht werden kann.

# Wie Es geht

Um die Länge eines Strings in Clojure zu finden, können wir die Funktion `(count)` verwenden. Diese Funktion nimmt eine Sammlung als Argument und gibt die Anzahl der Elemente in dieser Sammlung zurück.

```Clojure
(count "Hallo Welt")
;; Ausgabe: 11
```

Wenn wir jedoch versuchen, `count` direkt auf einen String anzuwenden, erhalten wir einen Fehler, da Strings in Clojure keine Sammlungen sind.

```Clojure
(count "Hello World")
;; Ausgabe: java.lang.IllegalArgumentException: Don't know how to create ISeq from: java.lang.String
```

Um dieses Problem zu lösen, können wir den String in eine Sequenz von Zeichen umwandeln und dann die Funktion `count` anwenden.

```Clojure
(count (seq "Hello World"))
;; Ausgabe: 11
```

In diesem Beispiel verwandeln wir den String "Hello World" zuerst in die Sequenz `(\H \e \l \l \o \  \W \o \r \l \d)`, auf der `count` angewendet werden kann.

## Tiefes Eintauchen

Das Finden der Länge eines Strings scheint eine einfache Aufgabe zu sein, aber es gibt einige interessante Konzepte, die beim Einsatz von `count` zu beachten sind.

Beispielsweise gibt `count` für eine leere Sequenz `()`, die keine Elemente enthält, 0 zurück. Dies ist sinnvoll, da die Länge einer leeren Sammlung 0 ist.

```Clojure
(count ())
;; Ausgabe: 0
```

Außerdem kann `count` auf andere Datentypen wie Listen, Vektoren und Maps angewendet werden, um die Anzahl der Elemente in diesen Datentypen zu finden.

```Clojure
(count [1 2 3 4])
;; Ausgabe: 4

(count {"a" 1, "b" 2, "c" 3})
;; Ausgabe: 3
```

Zuletzt ist es wichtig zu beachten, dass `count` für Strings die Anzahl der Zeichen zurückgibt, nicht die Anzahl der Bytes. Dies liegt daran, dass in Clojure Strings als Unicode-Sequenzen behandelt werden, wodurch Zeichen mit mehreren Bytes gezählt werden können.

# Siehe auch

- [Clojure Dokumentation zu count](https://clojuredocs.org/clojure.core/count)
- [Clojure for the Brave and True von Daniel Higginbotham](https://www.braveclojure.com/sequences/)
- [Einführung in Clojure von Lee Gaines](https://medium.com/coding-blocks/introduction-to-clojure-part-1-the-router-ea862e6cf062)