---
title:                "Перетворення рядка у нижній регістр"
date:                  2024-01-20T17:39:16.184363-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Що і чому?

Коли ми кажемо про перетворення рядка в нижній регістр, маємо на увазі зміну всіх великих літер на маленькі. Програмісти роблять це для уніфікації тексту - так легше порівнювати й обробляти дані.

## Як це зробити:

Ruby робить це просто. Ось приклад того, як змінити рядок на нижній регістр:

```ruby
original_string = "Це ПРИКЛАД Рядка"
lowercase_string = original_string.downcase

puts lowercase_string
```

Це виведе:

```
це приклад рядка
```

А ось якщо вам потрібно змінити лише латиницю:

```ruby
mixed_string = "Ruby 3.1.2 Є Найкращим!"
lowercase_latin = mixed_string.gsub(/[A-Z]/, &:downcase)

puts lowercase_latin
```

Ви отримаєте:

```
ruby 3.1.2 Є Найкращим!
```

## Поглиблений аналіз:

В давні часи, коли комп'ютери лише розвивалися, великий і малий регістри часто трактувалися як різні символи. Це робило текстову обробку складною. З часом, методи як `downcase` стали стандартом в більшості мов програмування, у тому числі і в Ruby.

Варто зазначити, що метод `downcase` працює відмінно з латинськими літерами. Але коли справа доходить до Unicode символів, як то кирилиця, необхідний метод `mb_chars.downcase.to_s`, якщо ви використовуєте Rails. Натомість, в чистому Ruby, можна користуватися бібліотекою 'unicode_utils':

```ruby
require 'unicode_utils/downcase'

original_string = "Це ПРИКЛАД Рядка"
lowercase_string = UnicodeUtils.downcase(original_string)

puts lowercase_string
```

## Дивіться також:

- [Ruby-Doc.org String#downcase](https://ruby-doc.org/core-3.1.2/String.html#method-i-downcase)
- [UnicodeUtils Gem](https://rubygems.org/gems/unicode_utils/versions/1.4.0)
- [Stack Overflow: Convert string to lower case in Ruby](https://stackoverflow.com/questions/4739596/convert-string-to-lower-case-in-ruby)

Прочитайте, експериментуйте, і не бійтеся пробувати нові підходи. Ruby відомий своєю гнучкістю — користуйтесь цим!