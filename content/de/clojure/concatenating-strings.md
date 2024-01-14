---
title:                "Clojure: Verkettung von Zeichenfolgen"
simple_title:         "Verkettung von Zeichenfolgen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Warum

Das Verketten von Strings ist in der Programmierung eine sehr gängige und nützliche Aufgabe. Es ermöglicht uns, mehrere String-Werte zu einer Zeichenfolge zusammenzufügen und so komplexe Textausgaben zu erstellen. In diesem Blog-Beitrag werden wir uns genauer damit befassen, wie man Strings in Clojure verbinden kann und warum es eine wichtige Fähigkeit ist, die jeder Programmierer beherrschen sollte.

## Wie geht das?

Um Strings in Clojure zu verknüpfen, gibt es die Funktion "str". Sie nimmt beliebig viele Argumente entgegen und gibt den resultierenden String zurück. Schauen wir uns ein simples Beispiel an:

```Clojure
(str "Hallo" "," "welt") => "Hallo, welt"
```

Wie man sieht, kann man nicht nur einzelne Worte, sondern auch Zeichensymbole und Zahlen miteinander verknüpfen. Die Reihenfolge der Argumente bestimmt dabei auch die Reihenfolge im resultierenden String.

Zudem gibt es noch die Funktion "join", die ähnlich wie "str" funktioniert, aber auch ein Trennzeichen zwischen den Strings einfügen kann. Schauen wir uns dazu ein Beispiel an:

```Clojure
(join "-" ["Das" "ist" "ein" "Beispiel"]) => "Das-ist-ein-Beispiel"
```

In diesem Beispiel haben wir die Funktion "join" verwendet, um aus einem Vektor von Strings einen zusammenhängenden String zu erstellen. Dabei haben wir als Trennzeichen das Minuszeichen verwendet.

## Tiefer Einblick

Bei der Verkettung von Strings müssen wir darauf achten, dass wir auch bestimmte Typen in String-Werte umwandeln. So kann es zum Beispiel vorkommen, dass wir eine Zahl als Teil eines Strings haben und diese in einen String-Wert umwandeln müssen, um sie mit anderen Strings zu verbinden. Dafür gibt es in Clojure die Funktion "str" und auch die Funktion "format", die Formatierungen wie in der Programmiersprache C ermöglicht.

Ein weiterer wichtiger Aspekt ist die Effizienz beim Verketten von Strings. In Clojure werden Strings als unveränderliche Datentypen angesehen, was bedeutet, dass jeder String generell eine neue Kopie des Originals erstellen muss, um die Verkettung durchzuführen. Um dies zu vermeiden, ist es ratsam, größere Strings als Puffer zu verwenden und diese dann mit den gewünschten Werten zu füllen.

## Siehe auch

- [Clojure Dokumentation zu String Manipulation](https://clojure.org/reference/java_interop#compatibility)
- [Vergleich von "str" und "join" Funktionen in Clojure](https://stuartsierra.com/2008/05/29/stringbuilder-for-clojure)
- [Weitere Tipps zur Optimierung von String-Verkettungen in Clojure](https://martintrojer.github.io/clojure/2016/04/09/shifting-to-clojure-part-03-string-concatenation.html)