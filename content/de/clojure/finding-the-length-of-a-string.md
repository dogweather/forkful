---
title:                "Die Länge eines Strings finden."
html_title:           "Clojure: Die Länge eines Strings finden."
simple_title:         "Die Länge eines Strings finden."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Bevor wir uns in die Details stürzen, warum sollte man überhaupt die Länge eines Strings in Clojure finden wollen? Nun, die Länge eines Strings ist eine grundlegende Operation, die in vielen Anwendungsfällen benötigt wird, egal ob es um die Verarbeitung von Benutzereingaben, das Formatieren von Ausgaben oder das Überprüfen von Bedingungen geht.

## Wie geht das?

Um die Länge eines Strings in Clojure zu finden, gibt es mehrere Möglichkeiten. Eine einfache Methode ist die Verwendung der Funktion `count`, die die Anzahl der Elemente in einer Sequenz zurückgibt. Da Strings in Clojure als Sequenzen behandelt werden, kann `count` verwendet werden, um die Anzahl der Zeichen im String zu ermitteln. Ein Beispiel:

```Clojure
(count "Hallo Welt") ; gibt 10 zurück
```

Eine andere Möglichkeit ist die Verwendung der Funktion `str` in Kombination mit `count`. `str` wandelt jedes übergebene Argument in einen String um und gibt diesen zurück. Durch die Verwendung von `count` auf dem Ergebnis von `str` erhält man die Länge des übergebenen Strings. Ein Beispiel:

```Clojure
(count (str "Hallo " "Welt")) ; gibt 10 zurück
```

## Tiefentauchen

Jetzt, da wir eine einfache und eine alternative Methode kennen, um die Länge eines Strings zu finden, lassen Sie uns einen Blick unter die Haube werfen. Wie bereits erwähnt, werden Strings in Clojure als Sequenzen behandelt. Dies bedeutet, dass sie als eine Liste von Elementen betrachtet werden können. Jedes Zeichen in einem String wird als einzelnes Element in der Sequenz betrachtet. Daher gibt `count` einfach die Anzahl der Elemente in dieser Liste zurück.

Die Funktion `str` nutzt dieses Konzept und kombiniert alle übergebenen Argumente zu einer einzigen Sequenz von Zeichen. Anschließend wird `count` auf diese Sequenz angewendet, um ihre Länge zu ermitteln.

## Siehe auch

- [Clojure-Dokumentation zu count](https://clojuredocs.org/clojure.core/count)
- [Clojure-Dokumentation zu str](https://clojuredocs.org/clojure.core/str)