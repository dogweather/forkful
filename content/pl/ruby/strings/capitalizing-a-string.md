---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:06:18.103640-07:00
description: "Kapitalizacja ci\u0105gu znak\xF3w w programowaniu cz\u0119sto odnosi\
  \ si\u0119 do zamiany pierwszego znaku ci\u0105gu na wielk\u0105 liter\u0119, a\
  \ reszt\u0119 na ma\u0142e litery. Programi\u015Bci\u2026"
lastmod: '2024-03-11T00:14:09.132185-06:00'
model: gpt-4-0125-preview
summary: "Kapitalizacja ci\u0105gu znak\xF3w w programowaniu cz\u0119sto odnosi si\u0119\
  \ do zamiany pierwszego znaku ci\u0105gu na wielk\u0105 liter\u0119, a reszt\u0119\
  \ na ma\u0142e litery. Programi\u015Bci\u2026"
title: "Zamiana liter na wielkie w \u0142a\u0144cuchu znak\xF3w"
---

{{< edit_this_page >}}

## Co i dlaczego?
Kapitalizacja ciągu znaków w programowaniu często odnosi się do zamiany pierwszego znaku ciągu na wielką literę, a resztę na małe litery. Programiści robią to z powodów takich jak przestrzeganie konwencji nazewnictwa, poprawa czytelności wyników lub zapewnienie spójności danych dla porównań i przechowywania.

## Jak to zrobić:
Ruby oferuje proste metody manipulacji ciągami znaków, w tym kapitalizację. Oto jak można zamienić ciąg na kapitałki w Ruby:

```ruby
# Wbudowana metoda Ruby
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Metoda `.capitalize` w Ruby jest wygodna, ale wpływa tylko na pierwszą literę. Dla większej kontroli lub aby zamienić każde słowo w ciągu na kapitałki (znane jako styl tytułowy), możesz chcieć użyć metody `titleize` z rozszerzenia Rails ActiveSupport, lub zaimplementować ją samodzielnie:

```ruby
# Używanie 'titleize' z ActiveSupport w Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

Jeśli nie używasz Rails lub preferujesz czyste rozwiązanie w Ruby, oto jak możesz zamienić każde słowo w ciągu na kapitałki:

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Ta metoda dzieli ciąg na tablicę słów, zamienia każde z nich na kapitałki, a następnie łączy je z powrotem razem z przestrzenią.
