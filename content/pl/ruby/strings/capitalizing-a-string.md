---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "Capitalizacja stringa oznacza zwykle przekszta\u0142cenie pierwszego\
  \ znaku stringa na wielk\u0105 liter\u0119, a reszty na ma\u0142e litery. Ale czasami\
  \ mo\u017Ce to oznacza\u0107 po\u2026"
lastmod: '2024-03-25T19:22:10.053897-06:00'
model: gpt-4-0125-preview
summary: "Capitalizacja stringa oznacza zwykle przekszta\u0142cenie pierwszego znaku\
  \ stringa na wielk\u0105 liter\u0119, a reszty na ma\u0142e litery. Ale czasami\
  \ mo\u017Ce to oznacza\u0107 po\u2026"
title: "Zamiana ma\u0142ych liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
---

## Co i Dlaczego?
Capitalizacja stringa oznacza zwykle przekształcenie pierwszego znaku stringa na wielką literę, a reszty na małe litery. Ale czasami może to oznaczać po prostu upewnienie się, że pierwsza litera jest wielka, pozostawiając resztę stringa bez zmian. Szczerze mówiąc, moim zdaniem, to dość niejasny termin.

## Jak to zrobić:
Ruby dostarcza [prostych metod do manipulacji stringami](https://docs.ruby-lang.org/en/3.3/String.html), w tym capitalizacji:

```ruby
# Wbudowana metoda Ruby'ego
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Bardzo przydatne.

Metoda `.capitalize` w Ruby jest wygodna, ale zamienia na wielką literę tylko pierwszą literę. Dla większej kontroli lub aby zmienić każde słowo w stringu na wielką literę (znane jako "title case"), możesz chcieć użyć metody `titleize` z rozszerzenia Rails ActiveSupport, lub zaimplementować ją samodzielnie:

```ruby
# Użycie 'titleize' z ActiveSupport w Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Rozwiązanie własnoręczne
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Ta metoda dzieli string na tablicę słów, zamienia każde z nich na wielką literę, a następnie łączy je z powrotem razem z przestrzenią.

Osobiście, w moim kodzie idę z tym pomysłem o wiele dalej. Napisałem własną metodę [`titleize`, która uwzględnia małe słowa takie jak "a" i "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
