---
title:                "Zamiana liter w ciągu na wielkie"
date:                  2024-03-25T17:32:29.927702-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Co i dlaczego?
Zazwyczaj wielką literą piszemy pierwszy znak ciągu, a resztę zmniejszamy. Ale czasem może to oznaczać po prostu upewnienie się, że pierwsza litera jest wielka, pozostawiając resztę ciągu bez zmian. Szczerze mówiąc, moim zdaniem, jest to termin dość niejasny.

## Jak to zrobić:
Ruby oferuje [proste metody manipulacji ciągami](https://docs.ruby-lang.org/en/3.3/String.html), w tym kapitalizację:

```ruby
# Wbudowana metoda Ruby
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Bardzo wygodne.

Metoda `.capitalize` Ruby jest wygodna, ale zmienia na wielką literę tylko pierwszą literę. Dla większej kontroli lub aby kapitalizować każde słowo w ciągu (znane jako styl tytułowy), możesz chcieć użyć metody `titleize` z rozszerzenia ActiveSupport Rails, lub zaimplementować ją samodzielnie:

```ruby
# Użycie 'titleize' z ActiveSupport w Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Rozwiązanie własne
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Ta metoda dzieli ciąg na tablicę słów, kapitalizuje każde z nich, a następnie łączy je z powrotem razem z odstępem.

Osobiście w swoim kodzie posuwam tę ideę dużo dalej. Napisałem własną metodę [`titleize`, która uwzględnia małe słówka takie jak "a" i "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
