---
title:    "Clojure: Verknüpfung von Zeichenfolgen"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Warum

Das Zusammenführen von Strings ist eine häufig verwendete Aufgabe in der Programmierung. Es ermöglicht uns, verschiedene Textabschnitte zu einer zusammenhängenden Zeichenkette zu verbinden. In diesem Artikel werden wir uns ansehen, wie man dies in Clojure erreichen kann.

## Wie geht das

Das Zusammenführen von Strings in Clojure ist sehr einfach und intuitiv. Wir können die Funktion `str` verwenden, um zwei oder mehr Strings aneinander zu hängen. Schauen wir uns ein einfaches Beispiel an:

```Clojure
(str "Hallo" "Welt") ; gibt "HalloWelt" aus
```

Beachten Sie, dass die einzelnen Strings durch Leerzeichen getrennt werden müssen, wenn Sie einen Lesbarkeitstreuer erstellen möchten. Aber was ist, wenn wir eine ganze Liste von Wörtern haben, die wir zusammenführen möchten? Dafür haben wir die Funktion `clojure.string/join`, die eine Liste von Strings mit einem Trennzeichen als Argument akzeptiert. Schauen wir uns ein Beispiel an:

```Clojure
(clojure.string/join ", " ["Dies" "ist" "ein" "Beispiel"]) ;gibt "Dies, ist, ein, Beispiel" aus
```

## Tiefer Einblick

Wir können auch die Funktion `format` verwenden, um Strings zusammenzuführen und dabei gleichzeitig bestimmte Platzhalter innerhalb der Zeichenkette zu ersetzen. Diese Platzhalter werden dann durch die angegebenen Werte ersetzt. Schauen wir uns ein Beispiel an:

```Clojure
(format "Mein Lieblingsgewürz ist %s" "Paprika") ; gibt "Mein Lieblingsgewürz ist Paprika" aus
```

Es ist auch wichtig zu beachten, dass das Zusammenführen von Strings in Clojure nicht die gleiche Performance hat wie in anderen Sprachen wie Java oder C++. Clojure verwendet Immutable Datentypen, was bedeutet, dass bei jedem Hinzufügen eines neuen Strings ein neuer String erstellt werden muss, was zu einer höheren Speicherauslastung führen kann. Falls Sie mit größeren Datenmengen arbeiten, empfiehlt es sich, die `StringBuilder` Klasse zu verwenden.

## Siehe auch

https://clojure.org/reference/strings
https://www.braveclojure.com/basic-data-structures/
https://clojuredocs.org/clojure.core/item