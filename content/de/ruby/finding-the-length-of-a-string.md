---
title:                "Die Länge eines Strings finden"
html_title:           "Ruby: Die Länge eines Strings finden"
simple_title:         "Die Länge eines Strings finden"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Warum

Wenn Sie Ruby lernen oder bereits verwenden, ist es wichtig zu wissen, wie Sie die Länge einer Zeichenkette finden können. Dies kann hilfreich sein, um Texte zu analysieren, Verarbeitung in einer Schleife durchzuführen oder einfach nur, um zu wissen, wie lang ein bestimmter Satz oder ein Wort ist.

## Wie geht es?

Um die Länge einer Zeichenkette in Ruby zu finden, können Sie die `.length` Methode verwenden. Diese Methode gibt die Anzahl der Zeichen in der Zeichenkette zurück.

```Ruby
puts "Hallo Welt".length
```
Dieser Code gibt die Ausgabe `10` zurück, da die Zeichenkette "Hallo Welt" aus 10 Zeichen besteht (einschließlich Leerzeichen). Sie können auch Variablen verwenden, um die Länge von Benutzereingaben oder anderen Zeichenketten zu finden.

```Ruby
eingabe = gets.chomp
puts "Die eingegebene Zeichenkette hat #{eingabe.length} Zeichen."
```

Wenn der Benutzer zum Beispiel "Hallo" eingibt, wird die Ausgabe `Die eingegebene Zeichenkette hat 5 Zeichen.` ausgegeben. Sie können auch die `.size` Methode verwenden, die das gleiche Ergebnis liefert wie die `.length` Methode. Es ist jedoch zu beachten, dass die Verwendung von `.length` für Zeichenketten und `.size` für Arrays und Hashes allgemein üblich ist.

## Tiefer Einblick

Die `.length` Methode zählt nicht nur die tatsächlichen Buchstaben in einer Zeichenkette, sondern auch Leerzeichen, Satzzeichen und Sonderzeichen. Wenn Sie also eine Zeichenkette mit Leerzeichen oder Satzzeichen haben, wird die Anzahl der Zeichen in der Ausgabe größer sein als die Anzahl der tatsächlichen Buchstaben. Wenn Sie nur die Anzahl der Buchstaben zählen möchten, können Sie die `.count()` Methode verwenden und bestimmte Zeichen angeben, die gezählt werden sollen.

```Ruby
puts "Hallo welt".count("Ll") # zählt nur L und l
```

Die Ausgabe für diesen Code wäre `3`, da es in der Zeichenkette "Hallo welt" insgesamt drei Buchstaben "L" und "l" gibt.

## Siehe auch

- [Ruby Dokumentation für .length und .size](https://ruby-doc.org/core-3.0.0/String.html#method-i-length)
- [Ruby Dokumentation für .count](https://ruby-doc.org/core-3.0.0/String.html#method-i-count)
- [Codecademy Tutorial für Strings in Ruby](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-introduction-to-strings)