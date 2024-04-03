---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Ruby \u043F\u0440\u0435\u0434\u043B\u0430\u0433\u0430\u0435\u0442\
  \ [\u043F\u0440\u043E\u0441\u0442\u044B\u0435 \u043C\u0435\u0442\u043E\u0434\u044B\
  \ \u0434\u043B\u044F \u043C\u0430\u043D\u0438\u043F\u0443\u043B\u044F\u0446\u0438\
  \u0438 \u0441\u043E \u0441\u0442\u0440\u043E\u043A\u0430\u043C\u0438](https://docs.ruby-lang.org/en/3.3/String.html),\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u044F \u043F\u0440\u0435\u043E\u0431\u0440\
  \u0430\u0437\u043E\u0432\u0430\u043D\u0438\u0435 \u043A\u2026"
lastmod: '2024-03-25T19:22:17.177185-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u043F\u0440\u0435\u0434\u043B\u0430\u0433\u0430\u0435\u0442 [\u043F\
  \u0440\u043E\u0441\u0442\u044B\u0435 \u043C\u0435\u0442\u043E\u0434\u044B \u0434\
  \u043B\u044F \u043C\u0430\u043D\u0438\u043F\u0443\u043B\u044F\u0446\u0438\u0438\
  \ \u0441\u043E \u0441\u0442\u0440\u043E\u043A\u0430\u043C\u0438](https://docs.ruby-lang.org/en/3.3/String.html),\
  \ \u0432\u043A\u043B\u044E\u0447\u0430\u044F \u043F\u0440\u0435\u043E\u0431\u0440\
  \u0430\u0437\u043E\u0432\u0430\u043D\u0438\u0435 \u043A \u0432\u0438\u0434\u0443\
  \ \u0441 \u0437\u0430\u0433\u043B\u0430\u0432\u043D\u043E\u0439 \u043F\u0435\u0440\
  \u0432\u043E\u0439 \u0431\u0443\u043A\u0432\u044B."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0441\u0442\u0440\u043E\u043A\u0438 \u0432 \u0432\u0435\u0440\u0445\u043D\
  \u0438\u0439 \u0440\u0435\u0433\u0438\u0441\u0442\u0440"
weight: 2
---

## Как это сделать:
Ruby предлагает [простые методы для манипуляции со строками](https://docs.ruby-lang.org/en/3.3/String.html), включая преобразование к виду с заглавной первой буквы:

```ruby
# Встроенный метод Ruby
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Очень удобно.

Метод `.capitalize` в Ruby удобен, но он делает заглавной только первую букву. Для большего контроля или для преобразования каждого слова в строке к виду с заглавной первой буквы (известному как написание с заглавной буквы всех слов), вы можете использовать метод `titleize` из расширения ActiveSupport в Rails, или реализовать его самостоятельно:

```ruby
# Использование 'titleize' из ActiveSupport в Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Собственное решение
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Этот метод разбивает строку на массив слов, делает каждое слово с заглавной первой буквой, затем соединяет их обратно вместе с пробелом.

Лично я в своём коде развиваю эту идею гораздо дальше. Я написал собственный метод [`titleize`, который учитывает маленькие слова, такие как "a" и "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
