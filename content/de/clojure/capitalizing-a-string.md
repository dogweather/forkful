---
title:    "Clojure: Eine Zeichenkette in Großbuchstaben umwandeln."
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Warum

Es gibt viele Gründe, warum man eine Zeichenkette in Großbuchstaben umwandeln möchte. Zum Beispiel kann es nützlich sein, wenn man mit Benutzereingaben arbeitet und sicherstellen möchte, dass bestimmte Worte oder Sätze immer in Großbuchstaben erscheinen. Es kann auch hilfreich sein, um Strings in Sortieralgorithmen zu vergleichen, da Groß- und Kleinschreibung normalerweise anders behandelt werden.

##Wie geht das?

In Clojure gibt es eine praktische Funktion namens `clojure.string/upper-case`, die eine Zeichenkette in Großbuchstaben konvertiert. Schauen wir uns ein Beispiel an:

```Clojure
(clojure.string/upper-case "hallo welt")
```
Dies würde die Ausgabe "HALLO WELT" erzeugen. Einfach, oder?

##Tiefergehende Informationen

Es gibt einige Dinge zu beachten, wenn man Strings in Großbuchstaben umwandelt. Zum Beispiel wird die Funktion `clojure.string/upper-case` Umlaute und Sonderzeichen nicht korrekt umwandeln. In solchen Fällen ist es ratsam, die Funktion `clojure.string/replace` zu verwenden und individuelle Ersetzungsregeln für diese Zeichen zu definieren.

##Siehe auch

- [Die offizielle Dokumentation zu `clojure.string`](https://clojuredocs.org/clojure.string)
- [Eine praktische Anleitung zur Arbeit mit Strings in Clojure](https://www.braveclojure.com/strings/)
- [Clojure Cheat Sheet mit nützlichen String-Funktionen](https://clojure.org/api/cheatsheet)