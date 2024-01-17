---
title:                "Die Länge eines Texts finden"
html_title:           "Clojure: Die Länge eines Texts finden"
simple_title:         "Die Länge eines Texts finden"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?
Wenn wir als Programmierer mit Text arbeiten, ist es oft wichtig zu wissen, wie viele Zeichen oder Buchstaben in einem String enthalten sind. Das Bestimmen der Länge eines Strings kann uns helfen, bestimmte Bedingungen in unserem Code zu überprüfen oder die Ausgabe eines Programms zu formatieren.

## Wie geht's?
Wir können die Funktion `count` verwenden, um die Länge eines Strings in Clojure zu bestimmen. Sehen wir uns ein Beispiel an:
```Clojure
(count "Hallo Welt!")
```
Dieses Code-Snippet gibt die Ausgabe `11` zurück, da der String `Hallo Welt!` 11 Zeichen enthält. Für eine weitere Veranschaulichung hier ein Beispiel mit einer Variablen:
```Clojure
(def text "Dies ist ein Beispiel")
(count text)
```
Die Ausgabe ist hier `19`.

## Tiefentauchgang
Die Funktion `count` wurde in Clojure bereits seit der Einführung der Sprache verwendet und ist somit Teil der Kernbibliothek. Eine andere Möglichkeit, die Länge eines Strings zu bestimmen, wäre die Verwendung der Methode `length` auf der Klasse `java.lang.String`. Jedoch wird die `count` Funktion empfohlen, da sie effizienter arbeitet und auch Zeichen wie Leerzeichen und Sonderzeichen zählt.

## Siehe auch
- Offizielle Clojure Dokumentation zu `count`: https://clojuredocs.org/clojure.core/count
- Alternative Methode `length` auf der Klasse `java.lang.String`: https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#length()