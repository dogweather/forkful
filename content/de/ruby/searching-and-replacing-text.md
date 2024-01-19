---
title:                "Suchen und Ersetzen von Text"
html_title:           "C#: Suchen und Ersetzen von Text"
simple_title:         "Suchen und Ersetzen von Text"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Was & Warum?
Suchen und Ersetzen ist die Manipulation von Text, um bestimmte Muster zu finden und durch anderen Text zu ersetzen. Programmierer tun das, um bestimmte Zeichen, Wörter oder Sätze in einem Textschnipsel oder einer Datei zu ändern.

## Anleitung:
Du kannst in Ruby den `gsub`-Methode verwenden, um Text zu suchen und zu ersetzen. Die Syntax ist `str.gsub(pattern, replacement)`. Hier ist ein einfacher Code-Beispiel:

```ruby
text = "Hallo Welt"
neuer_text = text.gsub("Welt", "Ruby")
puts neuer_text
```

Die Ausgabe wäre: 

```
Hallo Ruby
```

Die `gsub`-Methode kann auch mit regulären Ausdrücken verwendet werden. Hier ist ein Beispiel:

```ruby
text = "10 grüne Flaschen"
neuer_text = text.gsub(/\d/, "keine")
puts neuer_text
```

Die Ausgabe wäre:

```
keine grüne Flaschen
```

## Deep Dive:
Die `gsub`-Methode in Ruby war schon immer ein wichtiger Bestandteil der Textmanipulation. Es gibt jedoch Alternativen wie die `sub`-Methode, die nur das erste Vorkommen ersetzt. Zur Implementierung in Ruby verwendet `gsub` intern die `rb_str_sub_bang`-Funktion, um die Änderungen durchzuführen.

## Siehe auch:
- Ruby-Dokumentation für `gsub`: https://ruby-doc.org/core-2.7.0/String.html#method-i-gsub
- Artikel über reguläre Ausdrücke in Ruby: https://www.rubyguides.com/2015/06/ruby-regex/
- Ruby-Dokumentation für `sub`: https://ruby-doc.org/core-2.7.0/String.html#method-i-sub