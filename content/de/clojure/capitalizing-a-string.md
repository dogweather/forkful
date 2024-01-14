---
title:    "Clojure: Eine Zeichenkette großschreiben."
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich mit der Großschreibung von Zeichenketten beschäftigen? Ganz einfach: Es ist eine grundlegende Funktion, die in vielen Programmen und Anwendungen verwendet wird. Durch das Verständnis dieser Funktion können Sie Ihre Programmierfähigkeiten erweitern und effizientere Code schreiben.

## Anleitung

Die Funktion "capitalize" in Clojure wird verwendet, um den ersten Buchstaben einer Zeichenkette in einen Großbuchstaben zu ändern. Hier ist ein Beispiel:

```Clojure
(capitalize "hallo welt")
```
Die Ausgabe davon wäre "Hallo Welt". Auch Sonderzeichen und Zahlen werden dabei berücksichtigt:

```Clojure
(capitalize "123abc!@#")
```
Die Ausgabe wäre "123abc!@#".

## Tiefergehende Informationen

In Clojure ist die Funktion "capitalize" eher einfach und unkompliziert. Sie arbeitet jedoch nicht nur mit einzelnen Wörtern, sondern auch mit ganzen Sätzen:

```Clojure
(capitalize "ich liebe es, Clojure zu programmieren.")
```
Die Ausgabe davon wäre "Ich liebe es, Clojure zu programmieren." Es ist wichtig zu beachten, dass die Funktion "capitalize" nur den ersten Buchstaben des Satzanfangs ändert und alle anderen Buchstaben bleiben unverändert.

## Siehe auch

Hier sind einige nützliche Links, um mehr über das Großschreiben von Zeichenketten in Clojure zu erfahren:

- Dokumentation für die capitalize-Funktion von Clojure: https://clojuredocs.org/clojure.core/capitalize
- Ein Tutorial für grundlegende Clojure-Funktionen, einschließlich capitalize: https://www.tutorialspoint.com/clojure/clojure_basic_functions.htm
- Eine Einführung in die Programmierung mit Clojure: http://www.learn-clojure.com/