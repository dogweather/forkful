---
title:                "Clojure: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/clojure/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum
Jeder, der schon einmal mit Programmierung zu tun hatte, weiß wie wichtig es ist, die Länge einer Zeichenfolge zu kennen. Egal ob es darum geht, Benutzereingaben zu überprüfen oder bestimmte Operationen auf einer Zeichenfolge auszuführen, die Länge ist eine entscheidende Information. In diesem Blogbeitrag werden wir uns genau damit beschäftigen - wie man in Clojure die Länge einer Zeichenfolge findet.

## Wie man die Länge einer Zeichenfolge in Clojure findet
Um die Länge einer Zeichenfolge in Clojure zu finden, haben wir mehrere Möglichkeiten. Eine davon ist die Verwendung der Funktion `count`, die die Anzahl der Elemente in einer Sequenz zurückgibt. Da eine Zeichenfolge in Clojure als Sequenz von einzelnen Zeichen behandelt wird, können wir diese Funktion nutzen, um die Länge zu bestimmen. Schauen wir uns dazu ein Beispiel an:

```Clojure
(let [text "Hallo Welt"]
  (count text))
```

Das Ergebnis dieses Codeschnipsels wird 10 sein, da der String "Hallo Welt" aus genau 10 Zeichen besteht. Wir können auch mehrere Zeichenfolgen kombinieren, um die Gesamtlänge zu berechnen:

```Clojure
(let [text1 "Hallo "
      text2 "Welt"]
  (count (str text1 text2)))
```

Hier wird das Schlüsselwort `str` verwendet, um die beiden Zeichenfolgen miteinander zu verbinden. Das Ergebnis wird wieder 10 sein, da die einzelnen Zeichenfolgen zusammen 10 Zeichen ergeben.

## Tiefentauchen
Um besser zu verstehen, wie Clojure die Länge einer Zeichenfolge bestimmt, müssen wir etwas tiefer in die Funktionsweise von `count` schauen. Tatsächlich wird `count` nicht nur für Sequenzen verwendet, sondern für alle Datentypen, die eine Anzahl von Elementen besitzen. Hierfür verwendet es das Protokoll `Counted`. Ein Protokoll ist eine abstrakte Schnittstelle, die es Datentypen ermöglicht, auf dieselbe Art und Weise behandelt zu werden. In unserem Fall müssen also alle Datentypen, die die Länge bestimmen sollen, das Protokoll `Counted` implementieren. Dies ermöglicht es uns, `count` in einer Vielzahl von Situationen zu nutzen.

## Siehe auch
* Offizielle Dokumentation zu `count`: https://clojuredocs.org/clojure.core/count
* Informationen zu Protokollen in Clojure: https://clojure.org/reference/protocols