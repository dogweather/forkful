---
title:                "String in Großbuchstaben umwandeln"
html_title:           "C: String in Großbuchstaben umwandeln"
simple_title:         "String in Großbuchstaben umwandeln"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Was & Warum?

Eine Zeichenkette zu kapitalisieren bedeutet, den ersten Buchstaben im Wort groß zu machen, oft für bessere Lesbarkeit oder Einhaltung bestimmter Schreibstandards. Programmierer nutzen dies, um Nutzereingaben zu formatieren oder Textausgaben zu normieren.

## Anleitung:

Um in Ruby einen String zu kapitalisieren, verwenden wir einfach die Methode `.capitalize`. Hier ist ein Codebeispiel:

```ruby
mein_string = "ruby ist fantastisch"
puts mein_string.capitalize
```

Ausgabe:

```
Ruby ist fantastisch
```

Falls du jeden Wortanfang groß haben möchtest, gibt es `titleize` in Rails oder Du kannst `.split.map(&:capitalize).join(' ')` in plain Ruby nutzen:

```ruby
mein_string = "ruby ist wirklich fantastisch"
puts mein_string.split.map(&:capitalize).join(' ')
```

Ausgabe:

```
Ruby Ist Wirklich Fantastisch
```

## Vertiefung:

Ein kurzer historischer Kontext: In frühen Textverarbeitungssystemen war das Kapitalisieren eine nützliche Funktion, um Schlüsselwörter oder Titel hervorzuheben. In Ruby wurde `.capitalize` dazu entworfen, diese Aufgabe einfach zu machen.

Als alternative Methoden stehen `.upcase` für die Umwandlung aller Buchstaben in Großbuchstaben oder `.downcase` für Kleinschreibung zur Verfügung. Für aufwändigere Formatierungen könnt ihr auf Regular Expressions oder externe Bibliotheken wie ActiveSupport aus Rails zurückgreifen, die die Methode `titleize` anbietet.

Die interne Implementierung von `.capitalize` betrachtet nur den ersten Buchstaben des Strings und ändert dessen Case, während der Rest des Strings zu Kleinbuchstaben wird. Es handelt sich also nicht um eine vollständige Titel-Kapitalisierung im Sinne von Buchtiteln oder Überschriften.

## Siehe Auch:

- Ruby-Dokumentation zur `.capitalize` Methode: [ruby-doc.org/core-2.7.0/String.html#method-i-capitalize](https://ruby-doc.org/core-2.7.0/String.html#method-i-capitalize)
- Rails ActiveSupport `titleize`: [api.rubyonrails.org/classes/String.html#method-i-titleize](https://api.rubyonrails.org/classes/String.html#method-i-titleize)
- Ein tieferer Blick in Ruby's String Methoden: [ruby-doc.org/core-2.7.0/String.html](https://ruby-doc.org/core-2.7.0/String.html)