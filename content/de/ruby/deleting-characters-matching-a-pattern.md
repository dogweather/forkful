---
title:                "Löschen von Zeichen, die einem Muster entsprechen"
html_title:           "Ruby: Löschen von Zeichen, die einem Muster entsprechen"
simple_title:         "Löschen von Zeichen, die einem Muster entsprechen"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Was & Warum?

Wenn man Charaktere löschen möchte, die einem bestimmten Muster entsprechen, spricht man in der Welt der Programmierung von "Deleting characters matching a pattern". Programmierer nutzen diese Funktion, um schnell und effizient unerwünschte Zeichen aus einem Text zu entfernen.

## Wie geht's?

Hier sind ein paar Beispiele, wie man in Ruby Charaktere löschen kann, die einem bestimmten Muster entsprechen.

```
# Beispiel 1: Löschen von Zahlen aus einem String
str = "abc123def456"
str.gsub!(/\d/, "")
puts str # Output: abcdef

# Beispiel 2: Löschen von Sonderzeichen aus einem String
str = "Hello! It's #Monday!"
str.gsub!(/[!#]/, "")
puts str # Output: Hello It's Monday

# Beispiel 3: Löschen von Leerzeichen aus einem String
str = "This is a string with spaces"
str.gsub!(/\s+/, "")
puts str # Output: Thisisastringwithspaces
```

## Tiefere Einblicke

Das Löschen von Charakteren basierend auf einem Muster ist nichts Neues in der Programmierung. Schon in frühen Programmiersprachen, wie zum Beispiel C, konnte man ähnliche Funktionen nutzen. Alternativ kann man auch reguläre Ausdrücke nutzen, um gezielt nach Mustern in einem String zu suchen.

Die Implementierung einer Funktion zum Löschen von Charakteren ist relativ einfach. Man vergleicht jeden einzelnen Charakter im String mit dem angegebenen Muster und entfernt ihn, falls es übereinstimmt. Dadurch können unnötige Schleifen vermieden werden, was die Effizienz erhöht.

## Siehe auch

- [Ruby-Dokumentation zum Befehl `gsub`](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Einführung in reguläre Ausdrücke in Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)