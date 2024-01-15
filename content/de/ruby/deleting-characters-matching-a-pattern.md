---
title:                "Löschen von Zeichen mit einem Muster"
html_title:           "Ruby: Löschen von Zeichen mit einem Muster"
simple_title:         "Löschen von Zeichen mit einem Muster"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Warum

Es gibt verschiedene Gründe, warum man sich mit dem Löschen von Zeichen in einem String beschäftigen würde. Einer dieser Gründe könnte sein, dass man bestimmte Zeichen aus einem Text entfernen möchte, um eine bestimmte Struktur oder ein bestimmtes Format einzuhalten. Auch bei der Bearbeitung von Benutzereingaben kann es notwendig sein, Zeichen zu löschen, um sicherzustellen, dass die Daten richtig verarbeitet werden können.

## Wie geht man vor

Um Zeichen in einem String zu löschen, gibt es in Ruby verschiedene Methoden zur Verfügung. Eine Möglichkeit ist die Verwendung der Methode `gsub`, die jedes Vorkommen eines bestimmten Zeichens oder einer Zeichenfolge ersetzen kann. Zum Beispiel:

```ruby
text = "This sentence contains some unnecessary characters."
new_text = text.gsub(" ", "") # entfernt alle Leerzeichen im String
puts new_text # Ausgabe: "Thissentencecontainssomeunnecessarycharacters."
```

Eine weitere Möglichkeit ist die Verwendung der Methode `delete`, die es ermöglicht, spezifische Zeichen oder Zeichensätze zu entfernen. Zum Beispiel:

```ruby
text = "Delete these vowels"
new_text = text.delete("aeiou") # entfernt alle Vokale im String
puts new_text # Ausgabe: "Dlt ths vwls"
```

Zusätzlich können Reguläre Ausdrücke verwendet werden, um gezielt nach einem bestimmten Muster von Zeichen zu suchen und diese zu löschen. Zum Beispiel:

```ruby
text = "My email is john@example.com"
new_text = text.gsub(/[@.]/, "") # sucht nach allen "@" und "." im String und löscht sie
puts new_text # Ausgabe: "My email is johnexamplecom"
```

## Tiefensuche

Das Löschen von Zeichen in einem String kann mithilfe von Regulären Ausdrücken sehr mächtig sein. Durch die Verwendung von Metazeichen wie dem Punkt oder dem Stern kann man bestimmte Muster von Zeichen sehr genau definieren und löschen. Zum Beispiel:

```ruby
text = "There are many cats in the city."
new_text = text.gsub(/ca.*?s/, "") # sucht nach allen Wörtern, die mit "ca" beginnen und mit "s" enden und löscht sie
puts new_text # Ausgabe: "There are many in the city."
```

Man sollte jedoch bei der Verwendung von regulären Ausdrücken Vorsicht walten lassen, um unerwünschte Ergebnisse zu vermeiden. Es empfiehlt sich, die Dokumentation von Ruby zu Rate zu ziehen und mehrere Tests durchzuführen, bevor man Reguläre Ausdrücke in produktivem Code verwendet.

## Siehe auch

- [Ruby Dokumentation zu String Manipulation](https://ruby-doc.org/core-2.7.1/String.html)
- [Tutorial zu regulären Ausdrücken in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)
- [Video-Tutorial zu String Manipulation in Ruby](https://www.youtube.com/watch?v=d1tXPoMr348)