---
title:                "Einen String in Kleinbuchstaben umwandeln"
html_title:           "Elm: Einen String in Kleinbuchstaben umwandeln"
simple_title:         "Einen String in Kleinbuchstaben umwandeln"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# Zeichenketten in Kleinbuchstaben umwandeln in Ruby 

## Was und Warum?
Die Umwandlung einer Zeichenkette in Kleinbuchstaben (oder auch 'lower case') ist ein häufiger Bedarf in der Programmierung. Sie hilft, die Datenkonsistenz zu bewahren und Vergleiche zwischen Zeichenketten, unabhängig von der ursprünglichen Eingabe, zu erleichtern.

## Wie?
Ruby macht es uns leicht. Sie nutzt die Methode `.downcase` für diese Aufgabe. Sie wird direkt auf eine Zeichenkette angewendet. Probier es aus:

```Ruby
text = "Hallo Welt"
lower_text = text.downcase
puts lower_text
# Ausgabe: "hallo welt"
```

## Tiefere Information

1. Historischer Kontext: Ruby hat diese Funktion immer angeboten, sie wurde nicht in späteren Versionen hinzugefügt.
2. Alternativen: Es gibt keine native Alternativen zur Methode `.downcase` in Ruby, um eine Zeichenkette in Kleinbuchstaben zu konvertieren. Andere Sprachen bieten jedoch unterschiedliche Methoden an.
3. Implementierungsdetails: Intern wird `.downcase` global auf die gesamte Zeichenkette angewendet und konvertiert alle Großbuchstaben in Kleinbuchstaben. Beachte, dass `.downcase` keine Änderungen an der Originalzeichenkette vornimmt. Stattdessen gibt sie eine neue Zeichenkette mit den Änderungen zurück.

## Siehe auch

Für mehr Details:

- Ruby-Dokumentation: [String#downcase](https://ruby-doc.org/core-2.7.1/String.html#method-i-downcase)
- Anwendung von `.downcase` in Ruby-Programmen: [Praktische Beispiele](https://www.rubyguides.com/2018/08/ruby-downcase-method/)
- Unterschieden zwischen `.downcase` und `.downcase!`: [Erläuterungen](https://stackoverflow.com/questions/3832261/what-is-the-difference-between-downcase-and-downcase-in-ruby)