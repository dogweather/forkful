---
title:                "Преобразование строки в верхний регистр"
date:                  2024-03-25T17:31:54.488974-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и почему?
Капитализация строки обычно подразумевает преобразование первого символа строки в верхний регистр и остальные в нижний. Но иногда это может означать просто удостовериться, что первый символ в верхнем регистре, оставив остальную часть строки без изменений. Честно говоря, на мой взгляд, это довольно неоднозначный термин.

## Как сделать:
Ruby предоставляет [прямые методы для манипулирования строками](https://docs.ruby-lang.org/en/3.3/String.html), включая капитализацию:

```ruby
# Встроенный метод Ruby
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Очень удобно.

Метод `.capitalize` в Ruby удобен, но делает заглавной только первую букву. Для более тщательного контроля или для капитализации каждого слова в строке (известного как написание с заглавной буквы каждого слова), вы можете использовать метод `titleize` из расширения Rails ActiveSupport или реализовать его самостоятельно:

```ruby
# Используя 'titleize' из ActiveSupport в Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Собственная реализация
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Этот метод разбивает строку на массив слов, делает каждое слово с заглавной буквы, а затем соединяет их обратно с пробелом.

Лично я в своем коде захожу с этой идеей гораздо дальше. Я написал свой собственный метод [`titleize`, который учитывает маленькие слова, такие как "a" и "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
