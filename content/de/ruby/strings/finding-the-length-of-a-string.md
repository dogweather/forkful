---
date: 2024-01-20 17:48:19.081076-07:00
description: "Die L\xE4nge eines Strings zu finden bedeutet, die Anzahl der Zeichen,\
  \ aus denen er besteht, zu z\xE4hlen. Programmierer machen das oft, um Benutzereingaben\
  \ zu\u2026"
lastmod: 2024-02-19 22:05:13.329976
model: gpt-4-1106-preview
summary: "Die L\xE4nge eines Strings zu finden bedeutet, die Anzahl der Zeichen, aus\
  \ denen er besteht, zu z\xE4hlen. Programmierer machen das oft, um Benutzereingaben\
  \ zu\u2026"
title: "Ermittlung der Zeichenkettenl\xE4nge"
---

{{< edit_this_page >}}

## Was & Warum?
Die Länge eines Strings zu finden bedeutet, die Anzahl der Zeichen, aus denen er besteht, zu zählen. Programmierer machen das oft, um Benutzereingaben zu validieren, für Textverarbeitungen oder um bei der Datenverarbeitung Grenzfälle zu handhaben.

## So geht's:
Ruby macht's simpel – hier eine kurze Demo:

```ruby
str = "Hallo Ruby!"
str_length = str.length
puts str_length # Ausgabe: 11
```

Oder noch kürzer:

```ruby
puts "Hallo Ruby!".length # Ausgabe: 11
```

## Deep Dive
Die `.length`-Methode in Ruby gibt es schon eine Weile – sie ist simpel, effizient und tut genau das, was sie soll. Alternativ gibt es auch `.size`, die dasselbe macht:

```ruby
puts "Hallo Ruby!".size # Ausgabe: 11
```

Beide Methoden zählen einfach die Anzahl der Zeichen im String und geben diese als Integer zurück. Es ist interessant zu wissen, dass `.length` und `.size` Synonyme sind; Ruby bietet oft mehrere Wege, um das Gleiche zu erreichen, hier also eine Frage des persönlichen Vorliebens.

Strings in Ruby sind Objekte mit vielen Methoden, `.length` ist nur eine davon. Intern repräsentiert Ruby Strings als Array von Zeichen, was die Zählung vereinfacht. Frühere Versionen von Ruby hatten mit multibyte Zeichen einige Probleme (z. B. in UTF-8 kodiert), aber moderne Ruby-Versionen handhaben dies elegant und transparent.

## Siehe auch
- Ruby-Dokumentation zu Strings: [Ruby Docs - String](https://ruby-doc.org/core-2.7.0/String.html)
- Artikel über Ruby Strings: [RubyLearning - Strings](http://rubylearning.com/satishtalim/ruby_strings.html)
- Interessante Diskussion über `.length` vs. `.size`: [Stack Overflow - Length vs Size](https://stackoverflow.com/questions/5956067/ruby-size-vs-length)
