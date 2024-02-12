---
title:                "Einen String großschreiben"
aliases:
- /de/ruby/capitalizing-a-string.md
date:                  2024-02-03T19:06:05.660565-07:00
model:                 gpt-4-0125-preview
simple_title:         "Einen String großschreiben"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/ruby/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
