---
title:                "Ruby: Die Länge eines Strings finden"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Die Länge einer Zeichenkette zu finden, ist eine wichtige Fähigkeit für jeden, der mit Ruby programmiert. Es ermöglicht uns, die Größe einer Zeichenkette zu ermitteln und darauf basierend unsere Programme anzupassen. In dieser Blog-Post werden wir die verschiedenen Methoden zur Ermittlung der String-Länge in Ruby untersuchen.

## Wie funktioniert es

Um die Länge einer Zeichenkette in Ruby zu finden, gibt es mehrere Möglichkeiten. Die einfachste Methode ist die Verwendung der `length`-Methode. Diese Methode gibt die Anzahl der Zeichen in einer Zeichenkette zurück.

Ein Beispielcode könnte so aussehen:

```Ruby
name = "Max Mustermann"
puts name.length

# Output: 15
```

Wie wir sehen können, gibt die `length`-Methode die Anzahl der Zeichen in der `name`-Variablen zurück, die in diesem Fall 15 beträgt.

Eine weitere Möglichkeit ist die Verwendung der `size`-Methode. Diese funktioniert genauso wie die `length`-Methode und gibt ebenfalls die Anzahl der Zeichen zurück. Der einzige Unterschied ist, dass `size` auch auf anderen Datentypen wie Arrays und Hashes verwendet werden kann.

```Ruby
text = "Dies ist ein Beispieltext."
puts text.size

# Output: 27
```

## Tiefere Einblicke

Wusstest du, dass die `length`- und `size`-Methoden in Ruby tatsächlich auf der `size`-Methode des `Enumerable`-Moduls basieren? Dies bedeutet, dass sie auf jeder Klasse verfügbar sind, die das `Enumerable`-Modul verwendet, wie zum Beispiel Arrays oder Hashes.

Außerdem gibt es auch die Möglichkeit, die `bytesize`-Methode zu verwenden, um die Anzahl der Bytes einer Zeichenkette zurückzugeben. Diese Methode ist besonders nützlich, wenn du mit multibyte-Zeichen arbeitest, da sie die richtige Anzahl an Bytes zurückgibt, anstatt nur die Anzahl der Zeichen zu zählen.

```Ruby
text = "äöü"
puts text.bytesize

# Output: 6
```

## Siehe auch

- [Offizielle Ruby Dokumentation zur String-Klasse](https://ruby-doc.org/core-2.6/String.html)
- [Tutorial zur String-Manipulation in Ruby](https://www.tutorialspoint.com/ruby/ruby_strings.htm)
- [Praktische Anwendungen von Strings in Ruby](https://medium.com/rubycademy/string-methods-in-ruby-a811dc7c8c92)