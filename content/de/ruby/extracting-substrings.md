---
title:    "Ruby: Unterstrings extrahieren"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/de/ruby/extracting-substrings.md"
---

{{< edit_this_page >}}

## Warum

Das Herausziehen von Teilbereichen aus Strings ist eine nützliche Funktion in der Ruby-Programmierung. Es ermöglicht uns, Teile eines Strings zu isolieren und spezifische Informationen zu extrahieren, die wir in unserem Code benötigen. In diesem Blogpost werde ich erklären, wie man dies in Ruby macht und warum es eine wichtige Fähigkeit ist, die jeder Ruby-Entwickler beherrschen sollte.

## Wie man es macht

Um Teilbereiche aus Strings zu extrahieren, verwenden wir die `[]`-Notation in Kombination mit dem Indexing-Operator `..`. Zum Beispiel könnten wir einen Teilbereich eines Strings mit dem folgenden Code isolieren:

```Ruby
str = "Hallo, Welt!"
puts str[0..4]
```

Dies würde den Teilbereich "Hallo" des Strings ausgeben. Wir geben den Start- und Endindex des Teilbereichs an, den wir extrahieren möchten, und der Operator `..` zeigt an, dass wir ein offenes Ende haben. Dies bedeutet, dass der Endindex eingeschlossen ist. Beachten Sie, dass der erste Index eines Strings bei 0 beginnt.

Es gibt auch andere Möglichkeiten, Teile eines Strings zu extrahieren, wie zum Beispiel mit dem `slice`-Befehl oder dem `scan`-Befehl. Es gibt jedoch keine Notation wie `str[start, length]`, um einen Teilbereich von einer bestimmten Länge zu extrahieren.

## Tiefere Einblicke

Es gibt einige wichtige Dinge zu beachten, wenn wir Teilbereiche aus Strings extrahieren. Zum Beispiel ist es wichtig zu wissen, dass der Indexierungsvorgang von links nach rechts startet. Dies bedeutet, dass der Endindex immer größer oder gleich dem Startindex sein muss. Wenn wir versuchen, ein Bereich mit einem Endindex zu extrahieren, der kleiner als der Startindex ist, wird ein leerer String zurückgegeben.

Ein weiteres wichtiges Konzept ist das von sogenannten "negative indices". Wir können Teilbereiche eines Strings auch von hinten aus extrahieren, indem wir negative Indices verwenden. Zum Beispiel würde `str[-3..-1]` den Teilbereich "elt" ausgeben, da der letzte Buchstabe des Strings dem Index -1 entspricht.

## Siehe auch

- [Ruby String-Dokumentation] (https://ruby-doc.org/core-2.7.1/String.html)
- [Ruby `[]`-Notation] (https://medium.com/launch-school/ruby-numbered-reference-and-indexing-196ab7113a8d)
- [Ruby `slice`-Befehl] (https://medium.com/launch-school/ruby-slicing-strings-3b3a390fbd7b)

Ich hoffe, dass dieser Blogpost Ihnen geholfen hat, das Konzept der Unterstring-Extraktion in Ruby besser zu verstehen. Es ist ein sehr nützliches Werkzeug, das Ihnen helfen wird, effizientere und kontrolliertere Codes zu schreiben. Viel Spaß beim Programmieren!