---
title:                "Преобразование строки в нижний регистр"
aliases:
- ru/ruby/converting-a-string-to-lower-case.md
date:                  2024-01-28T23:57:00.608259-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в нижний регистр"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/converting-a-string-to-lower-case.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

В Ruby конвертация строки в нижний регистр означает изменение всех букв в строке верхнего регистра на их аналоги в нижнем регистре. Программисты делают это для обеспечения единообразия, особенно в задачах, связанных с сравнением пользовательского ввода или сортировкой.

## Как это сделать:

```ruby
# Используя метод downcase
my_string = "Hello World!"
puts my_string.downcase  # => "hello world!"
```

```ruby
# Используя downcase! для преобразования на месте
my_string = "Hello World!"
my_string.downcase!
puts my_string           # => "hello world!"
```

## Подробнее

Исторически конвертация регистра была важной частью языков программирования для обеспечения единообразия текста. Она поддерживает сравнения и поиски без учета регистра, поэтому имеет большое значение.

Методы `downcase` и `downcase!` в Ruby исходят из принципа языка предоставлять как недеструктивные, так и деструктивные методы для манипуляций со строками. Недеструктивный `downcase` возвращает новую строку, оставляя оригинал без изменений, в то время как деструктивный `downcase!` модифицирует оригинальную строку на месте, что может быть более эффективно с точки зрения использования памяти.

Существуют альтернативы для случаев, когда применяются правила, специфичные для локали. `String#mb_chars` в сочетании с `ActiveSupport::Multibyte::Chars#downcase` из библиотеки Rails ActiveSupport могут обрабатывать более сложные ситуации, такие как символы с акцентами или другие диакритические знаки:
```ruby
require 'active_support/core_ext/string/multibyte'

my_string = "ÄÖÜ"
puts my_string.mb_chars.downcase  # => "äöü"
```

Что касается реализации, `downcase` и `downcase!` в Ruby внутренне используют маппинг Unicode для конвертации каждого символа строки в его эквивалент в нижнем регистре.

## Смотрите также

- Документация Ruby для `downcase` и `downcase!`: [Ruby Doc downcase](https://ruby-doc.org/core-3.1.2/String.html#method-i-downcase), [Ruby Doc downcase!](https://ruby-doc.org/core-3.1.2/String.html#method-i-downcase-21)
- Для сложной конвертации регистра смотрите расширения ActiveSupport Core: [ActiveSupport String](https://api.rubyonrails.org/classes/String.html)
