---
changelog:
- 2024-03-25, dogweather, edited and tested
- 2024-03-25, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:02:29.358527-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : Ruby \u043D\u0430\u0434\u0430\u0454 [\u043F\u0440\u044F\u043C\u0456 \u043C\u0435\
  \u0442\u043E\u0434\u0438 \u0434\u043B\u044F \u043C\u0430\u043D\u0456\u043F\u0443\
  \u043B\u044F\u0446\u0456\u0439 \u0437 \u0440\u044F\u0434\u043A\u0430\u043C\u0438\
  ](https://docs.ruby-lang.org/en/3.3/String.html), \u0432\u043A\u043B\u044E\u0447\
  \u0430\u044E\u0447\u0438 \u043F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\
  \u043D\u044F \u0437 \u0432\u0435\u043B\u0438\u043A\u043E\u0457 \u043B\u0456\u0442\
  \u0435\u0440\u0438."
lastmod: '2024-03-25T19:22:10.717773-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u043D\u0430\u0434\u0430\u0454 [\u043F\u0440\u044F\u043C\u0456 \u043C\
  \u0435\u0442\u043E\u0434\u0438 \u0434\u043B\u044F \u043C\u0430\u043D\u0456\u043F\
  \u0443\u043B\u044F\u0446\u0456\u0439 \u0437 \u0440\u044F\u0434\u043A\u0430\u043C\
  \u0438](https://docs.ruby-lang.org/en/3.3/String.html), \u0432\u043A\u043B\u044E\
  \u0447\u0430\u044E\u0447\u0438 \u043F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\
  \u043D\u043D\u044F \u0437 \u0432\u0435\u043B\u0438\u043A\u043E\u0457 \u043B\u0456\
  \u0442\u0435\u0440\u0438."
title: "\u041F\u0435\u0440\u0435\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u0440\u044F\
  \u0434\u043A\u0430 \u0432 \u0432\u0435\u0440\u0445\u043D\u0456\u0439 \u0440\u0435\
  \u0433\u0456\u0441\u0442\u0440"
weight: 2
---

## Як це зробити:
Ruby надає [прямі методи для маніпуляцій з рядками](https://docs.ruby-lang.org/en/3.3/String.html), включаючи перетворення з великої літери:

```ruby
# Вбудований метод Ruby
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Дуже зручно.

Метод `.capitalize` в Ruby зручний, але перетворює в велику літеру лише першу букву. Для більшого контролю або щоб перетворити кожне слово в рядку на письмо з великої літери (відоме як назва з великої літери), ви можете використати метод `titleize` з розширення Rails ActiveSupport або реалізувати його самостійно:

```ruby
# Використання 'titleize' від ActiveSupport в Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Саморобне рішення
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Цей метод розбиває рядок на масив слів, перетворює кожне на велику літеру, а потім з'єднує їх назад разом із пробілом.

Особисто я в своєму коді застосовую цю ідею набагато ширше. Я написав власний метод [`titleize`, який враховує маленькі слова, такі як "a" та "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
