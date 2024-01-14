---
title:                "Ruby: Großschreibung einer Zeichenkette"
simple_title:         "Großschreibung einer Zeichenkette"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Kapitalisieren von Strings ist eine häufige Aufgabe, die in Ruby Programmen durchgeführt werden muss. Durch die Großschreibung eines Strings kann die Lesbarkeit und Übersichtlichkeit des Codes erhöht werden. Dies ist besonders hilfreich, wenn es um die Anzeige von Benutzerinformationen oder die Erstellung von Textausgaben geht.

## Wie geht man vor?

Es gibt mehrere Methoden in Ruby, um einen String zu kapitalisieren. Die einfachste Methode ist die Verwendung der `.capitalize` Methode. Diese Methode wandelt den ersten Buchstaben eines Strings automatisch in einen Großbuchstaben um und den Rest der Buchstaben in Kleinbuchstaben. Hier ist ein Beispiel:

```Ruby
name = "max muster"
puts name.capitalize
# Output: Max muster
```

Eine weitere Methode ist die Verwendung der `.upcase` Methode. Diese Methode wandelt alle Buchstaben eines Strings in Großbuchstaben um. Beispiel:

```Ruby
text = "hello ruby"
puts text.upcase
# Output: HELLO RUBY
```

Es gibt auch die Möglichkeit, einen String manuell zu kapitalisieren, indem man den Zugriff auf den ersten Buchstaben über den Index `[0]` verwaltet und diesen in einen Großbuchstaben umwandelt. Beispiel:

```Ruby
string = "hallo"
string[0] = string[0].capitalize
puts string
# Output: Hallo
```

## Tiefergehende Informationen

Es gibt auch die Möglichkeit, nur den ersten Buchstaben jedes Wortes in einem String zu kapitalisieren. Dies wird als Titelkapselung bezeichnet und kann mit der `.titleize` Methode erreicht werden. Diese Methode wandelt den ersten Buchstaben jedes Wortes in einen Großbuchstaben um und konvertiert alle anderen Buchstaben in Kleinbuchstaben. Beispiel:

```Ruby
text = "hallo ruby welt"
puts text.titleize
# Output: Hallo Ruby Welt
```

Es ist auch möglich, die verschiedenen Methoden zu kombinieren, um spezifische Kapitalisierungsvorgänge durchzuführen. Zum Beispiel kann man die `.capitalize` Methode verwenden, um nur den ersten Buchstaben eines Strings zu ändern, und dann die `.gsub` Methode verwenden, um alle übrigen Buchstaben in Kleinbuchstaben umzuwandeln. Beispiel:

```Ruby
text = "hallo RUBY welt"
puts text.capitalize.gsub(/[^A-Z]/, ' ')
# Output: Hallo RUBY Welt
```

## Siehe auch

- https://ruby-doc.org/core-2.6.3/String.html#method-i-capitalize
- https://ruby-doc.org/core-2.6.3/String.html#method-i-titleize
- https://ruby-doc.org/core-2.6.3/String.html#method-i-upcase