---
title:                "Ruby: Großschreibung einer Zeichenkette"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Warum

Das Verändern von Strings ist eine grundlegende Funktion in der Ruby Programmierung. Eine übliche Aufgabe ist es, Strings in Großbuchstaben umzuwandeln und in diesem Blogpost werde ich zeigen, wie man das mit Ruby erreichen kann.

## Wie geht's

Die einfachste Möglichkeit, Strings in Großbuchstaben umzuwandeln, ist mit der `.upcase` Methode. Zum Beispiel:

```Ruby
puts "hallo".upcase
```

Das Ergebnis dieser Code-Zeile wäre "HALLO".

Man kann auch wählen, ob man den gesamten String in Großbuchstaben umwandeln möchte oder nur den Anfangsbuchstaben. Hier ein Beispiel, um den ersten Buchstaben eines Strings in einen Großbuchstaben zu ändern:

```Ruby
puts "ruby".capitalize
```

Das Ergebnis wäre "Ruby".

Die `.capitalize` Methode kann besonders nützlich sein, wenn man Nutzerinput erwartet und sicherstellen möchte, dass der erste Buchstabe jedes Wortes groß geschrieben wird, unabhängig davon, wie der Nutzer es eingibt.

Zusätzlich gibt es auch noch die Möglichkeit, einen String in Kleinbuchstaben umzuwandeln, indem man die `.downcase` Methode nutzt.

```Ruby
puts "WE <3 RUBY".downcase
```

Das Ergebnis wäre "we <3 ruby".

## Tiefer Einblick

Wenn man sich etwas näher mit dem Ruby Code beschäftigt, gibt es eine Methode, die unter der Haube verwendet wird, um Strings in Großbuchstaben umzuwandeln: `.swapcase`.

```Ruby
puts "Hallo Ruby!".swapcase
```

Das Ergebnis wäre "hALLO rUBY!".

Dies ist besonders nützlich, wenn man zum Beispiel einen String hat, in dem einige Buchstaben bereits groß geschrieben sind, aber man möchte, dass alle Buchstaben, die zuvor großgeschrieben waren, nun klein geschrieben werden.

## Siehe auch

Weitere nützliche Methoden für die String-Manipulation in Ruby:

- `gsub` ersetzt alle Vorkommen von einem bestimmten Zeichen oder einer Zeichenfolge in einem String. Beispiele und Erklärungen findest du auf [ruby-doc.org](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub).
- `length` gibt die Länge eines Strings zurück. Mehr dazu auf [ruby-doc.org](https://ruby-doc.org/core-2.7.1/String.html#method-i-length).
- `split` teilt einen String an einem bestimmten Zeichen oder einer Zeichenfolge und gibt ein Array mit den einzelnen Teilen zurück. Eine ausführliche Erklärung und Beispiele findest du auf [ruby-doc.org](https://ruby-doc.org/core-2.7.1/String.html#method-i-split).

Für eine Gesamtübersicht über alle String-Methoden in Ruby, schau dir die offizielle Dokumentation auf [ruby-doc.org](https://ruby-doc.org/core-2.7.1/String.html) an.