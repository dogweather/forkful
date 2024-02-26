---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:05.660565-07:00
description: "Das Gro\xDFschreiben eines Strings in der Programmierung bezieht sich\
  \ oft darauf, den ersten Buchstaben eines Strings in Gro\xDFbuchstaben und den Rest\
  \ in\u2026"
lastmod: '2024-02-25T18:49:51.429676-07:00'
model: gpt-4-0125-preview
summary: "Das Gro\xDFschreiben eines Strings in der Programmierung bezieht sich oft\
  \ darauf, den ersten Buchstaben eines Strings in Gro\xDFbuchstaben und den Rest\
  \ in\u2026"
title: "Einen String gro\xDFschreiben"
---

{{< edit_this_page >}}

## Was & Warum?
Das Großschreiben eines Strings in der Programmierung bezieht sich oft darauf, den ersten Buchstaben eines Strings in Großbuchstaben und den Rest in Kleinbuchstaben umzuwandeln. Programmierer tun dies aus Gründen wie der Einhaltung von Benennungskonventionen, um Ausgaben lesbarer zu machen oder um Datenkonsistenz für Vergleiche und Speicherung zu gewährleisten.

## Wie geht das:
Ruby bietet einfache Methoden für die String-Manipulation, einschließlich der Großschreibung. So können Sie einen String in Ruby großschreiben:

```ruby
# Rubys eingebaute Methode
string = "hallo welt"
capitalized_string = string.capitalize
puts capitalized_string # => "Hallo welt"
```

Rubys `.capitalize` Methode ist praktisch, betrifft aber nur den ersten Buchstaben. Für mehr Kontrolle oder um jedes Wort in einem String großzuschreiben (bekannt als Titelgroßschreibung), möchten Sie vielleicht die `titleize` Methode aus der Rails ActiveSupport-Erweiterung verwenden oder sie selbst implementieren:

```ruby
# Verwendung von ActiveSupports 'titleize' in Rails
require 'active_support/core_ext/string/inflections'
string = "hallo welt"
puts string.titleize # => "Hallo Welt"
```

Wenn Sie Rails nicht verwenden oder eine reine Ruby-Lösung bevorzugen, so könnten Sie jedes Wort in einem String großschreiben:

```ruby
string = "hallo welt"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hallo Welt"
```

Diese Methode teilt den String in ein Array von Wörtern, macht jedes einzelne groß und fügt sie dann wieder mit einem Leerzeichen zusammen.
