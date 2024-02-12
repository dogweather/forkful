---
title:                "Зробити першу літеру рядка великою"
aliases: - /uk/ruby/capitalizing-a-string.md
date:                  2024-02-03T19:06:21.461017-07:00
model:                 gpt-4-0125-preview
simple_title:         "Зробити першу літеру рядка великою"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і Чому?
Заглавлення рядка у програмуванні часто має на увазі перетворення першого символу рядка на велику букву, а решту - на малі. Програмісти роблять це з різних причин, таких як дотримання конвенцій іменування, підвищення читабельності виводу або забезпечення консистенції даних для порівнянь та зберігання.

## Як:
Ruby надає прості методи для маніпулювання рядками, включаючи заглавлення. Ось як ви можете заглавити рядок в Ruby:

```ruby
# Вбудований метод Ruby
string = "hello world"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Метод `.capitalize` в Ruby зручний, але впливає лише на першу літеру. Для більшого контролю або для заглавлення кожного слова у рядку (відомого як регістр заголовку), можна використати метод `titleize` з розширення Rails ActiveSupport або реалізувати його самостійно:

```ruby
# Використання 'titleize' від ActiveSupport в Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

Якщо ви не використовуєте Rails або віддаєте перевагу чистому рішенню на Ruby, ось як ви можете заглавити кожне слово в рядку:

```ruby
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Цей метод розбиває рядок на масив слів, заглавляє кожне з них, а потім з'єднує їх знову разом з пробілом.
