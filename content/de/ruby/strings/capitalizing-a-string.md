---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "Wie man es macht: Ruby bietet [einfache Methoden zur Stringmanipulation](https://docs.ruby-lang.org/en/3.3/String.html),\
  \ einschlie\xDFlich der Gro\xDFschreibung."
lastmod: '2024-03-25T19:22:00.225387-06:00'
model: gpt-4-0125-preview
summary: "Ruby bietet [einfache Methoden zur Stringmanipulation](https://docs.ruby-lang.org/en/3.3/String.html),\
  \ einschlie\xDFlich der Gro\xDFschreibung."
title: "Gro\xDFschreibung eines Strings"
weight: 2
---

## Wie man es macht:
Ruby bietet [einfache Methoden zur Stringmanipulation](https://docs.ruby-lang.org/en/3.3/String.html), einschließlich der Großschreibung:

```ruby
# Rubys eingebaute Methode
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Sehr praktisch.

Rubys `.capitalize` Methode ist bequem, aber sie macht nur den ersten Buchstaben groß. Für mehr Kontrolle oder um jedes Wort in einem String zu großzuschreiben (bekannt als Titelfall), möchten Sie vielleicht die `titleize` Methode aus der Rails ActiveSupport-Erweiterung verwenden oder sie selbst implementieren:

```ruby
# 'titleize' in Rails mit ActiveSupport verwenden
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Eine selbstgemachte Lösung
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Diese Methode teilt den String in ein Array von Wörtern auf, setzt jedes in Großbuchstaben um und fügt sie dann wieder mit einem Leerzeichen zusammen.

Persönlich treibe ich diese Idee in meinem Code viel weiter. Ich habe meine eigene [`titleize` Methode geschrieben, die kleine Wörter wie "a" und "the" berücksichtigt](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
