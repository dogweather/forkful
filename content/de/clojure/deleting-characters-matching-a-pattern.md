---
title:    "Clojure: Entfernen von Zeichen, die einem Muster entsprechen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/clojure/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Das Löschen von Zeichen, die einem bestimmten Muster entsprechen, kann in der Programmierung nützlich sein, um unerwünschte Zeichen in einer Zeichenkette zu entfernen oder Daten zu bereinigen. In Clojure gibt es mehrere Möglichkeiten, dies zu erreichen.

## Wie es geht

Es gibt verschiedene Funktionen in Clojure, die zum Löschen von Zeichen verwendet werden können, die einem bestimmten Muster entsprechen. Eine davon ist die `replace` Funktion, die verwendet wird, um ein Zeichen in einer Zeichenkette durch ein anderes zu ersetzen. Zum Beispiel:

```Clojure
(def my-string "H3110 W0r1d")
(replace my-string #"1" "i")
```

Die Ausgabe dieser Funktion wird "Hello World" sein, da das Zeichen "1" durch "i" ersetzt wurde. Eine weitere nützliche Funktion ist `clojure.string/replace-first`, mit der nur das erste aufeinanderfolgende Vorkommen eines Musters ersetzt wird. Eine Anwendung davon könnte sein, eine Telefonnummer in einem bestimmten Format zu normalisieren. Zum Beispiel:

```Clojure
(def phone-number "+123-555-4567")
(clojure.string/replace-first phone-number #"-", "")
```

Die Ausgabe dieser Funktion wird "+123555-4567" sein, da das erste "-" Zeichen entfernt wurde. Andere nützliche Funktionen zum Löschen von Zeichen sind `clojure.string/replace-last` und `clojure.string/replace-re` (regular expression), die auf ähnliche Weise funktionieren.

## Tiefergehende Informationen

Um tiefer in das Thema des Löschens von Zeichen einzutauchen, ist es wichtig zu verstehen, wie reguläre Ausdrücke in Clojure funktionieren. Reguläre Ausdrücke werden verwendet, um Muster in Zeichenketten zu finden und können in Funktionen wie `replace` und `match?` verwendet werden. Eine hilfreiche Ressource, um mehr über reguläre Ausdrücke in Clojure zu erfahren, ist die offizielle Dokumentation unter https://clojuredocs.org/clojure.string/replace.

Eine andere interessante Möglichkeit, Zeichen in Clojure zu löschen, ist die Verwendung der `clojure.string/trim` Funktion. Diese Funktion entfernt alle Leerzeichen am Anfang und Ende einer Zeichenkette. Eine Anwendung davon könnte sein, Benutzereingaben vor der Verarbeitung zu bereinigen. Zum Beispiel:

```Clojure
(def user-input "  Hello  ")
(clojure.string/trim user-input)
```

Die Ausgabe dieser Funktion wird "Hello" sein, da die Leerzeichen am Anfang und Ende entfernt wurden.

## Siehe auch

* https://Clojure.org
* https://Clojuredocs.org/clojure.string/replace
* https://Clojuredocs.org/clojure.string/trim