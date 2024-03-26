---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-03-25 18:27:29.132907-06:00
description: "Das Kapitalisieren eines Strings bedeutet normalerweise, das erste Zeichen\
  \ eines Strings in Gro\xDFbuchstaben umzuwandeln und den Rest in Kleinbuchstaben.\u2026"
lastmod: '2024-03-25T18:27:29.134251-06:00'
model: gpt-4-0125-preview
summary: "Das Kapitalisieren eines Strings bedeutet normalerweise, das erste Zeichen\
  \ eines Strings in Gro\xDFbuchstaben umzuwandeln und den Rest in Kleinbuchstaben.\u2026"
title: "Einen String gro\xDFschreiben"
---

## Was & Warum?
Das Kapitalisieren eines Strings bedeutet normalerweise, das erste Zeichen eines Strings in Großbuchstaben umzuwandeln und den Rest in Kleinbuchstaben. Manchmal kann es aber auch nur bedeuten, sicherzustellen, dass das erste Zeichen ein Großbuchstabe ist, während der Rest des Strings unverändert bleibt. Ehrlich gesagt, halte ich es für einen etwas vagen Begriff.

## Wie geht das:
Ruby bietet [einfache Methoden zur Manipulation von Strings](https://docs.ruby-lang.org/en/3.3/String.html), einschließlich der Kapitalisierung:

```ruby
# Rubys eingebaute Methode
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Sehr praktisch.

Die Methode `.capitalize` von Ruby ist praktisch, aber sie setzt nur den ersten Buchstaben groß. Für mehr Kontrolle oder um jedes Wort in einem String großzuschreiben (bekannt als Titel-Kapitalisierung), möchten Sie vielleicht die `titleize`-Methode aus der Rails ActiveSupport-Erweiterung verwenden oder sie selbst implementieren:

```ruby
# Nutzung von 'titleize' von ActiveSupport in Rails
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

Diese Methode teilt den String in ein Array von Wörtern auf, kapitalisiert jedes einzelne und fügt sie dann wieder mit einem Leerzeichen zusammen.

Persönlich treibe ich diese Idee in meinem Code noch viel weiter. Ich habe meine eigene [`titleize`-Methode geschrieben, die kleine Wörter wie "a" und "the" berücksichtigt](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
