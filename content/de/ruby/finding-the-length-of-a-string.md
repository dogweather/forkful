---
title:    "Ruby: Die Länge einer Zeichenkette finden"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Finden der Länge eines Strings ist eine häufige Aufgabe in der Ruby-Programmierung. Es ermöglicht uns, die Anzahl der Zeichen in einem String zu bestimmen und somit verschiedene Manipulationen durchzuführen.

## Wie

Um die Länge eines Strings in Ruby zu finden, können wir die `.length` oder `.size` Methode verwenden. Diese Methoden geben uns die Anzahl der Zeichen in einem String zurück.

```Ruby
text = "Hallo Welt"

text.length #=> 10
text.size #=> 10
```

In diesem Beispiel haben wir die Länge des Strings "Hallo Welt" mit beiden Methoden ermittelt. Beachte, dass auch Leerzeichen als Zeichen gezählt werden.

## Deep Dive

Die `.length` und `.size` Methoden werden oft synonym verwendet, aber es gibt einen kleinen Unterschied zwischen den beiden. Während `.length` sich auf die Anzahl der Zeichen bezieht, zählt `.size` die Anzahl der Bytes im String. Dies kann einen Unterschied machen, wenn der String spezielle Zeichen enthält, die aus mehreren Bytes bestehen.

Es ist auch wichtig zu beachten, dass sowohl `.length` als auch `.size` nur für Strings funktionieren. Wenn wir versuchen, sie auf einer anderen Datenstruktur anzuwenden, können wir einen Fehler erhalten.

## Siehe auch

- [Offizielle Ruby-Dokumentation zu den String-Methoden](https://ruby-doc.org/core-2.7.0/String.html)
- [Ein weiteres Blog-Beitrag zum Thema "Länge eines Strings finden"](https://blog.alexseifert.com/2019/10/19/finding-the-length-of-a-string-in-ruby/)
- [In deutscher Sprache übersetzte Ruby-Dokumentation](https://ruby-doc.org/de/)