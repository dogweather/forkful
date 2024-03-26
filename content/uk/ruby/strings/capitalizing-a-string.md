---
title:                "Перетворення рядка на великі літери"
date:                  2024-03-25T17:32:07.879346-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-25, dogweather, edited and tested
  - 2024-03-25, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що і чому?
Приголоснення рядка зазвичай означає перетворення першого символа рядка у верхній регістр, а решту - у нижній. Але іноді це може означати просто переконатися, що перший символ є великою літерою, залишаючи решту рядка без змін. Чесно кажучи, на мою думку, це досить неоднозначний термін.

## Як це зробити:
Ruby надає [прості методи для маніпуляцій з рядками](https://docs.ruby-lang.org/en/3.3/String.html), включаючи приголоснення:

```ruby
# Вбудований метод Ruby
string = "hello WORLD"
capitalized_string = string.capitalize
puts capitalized_string # => "Hello world"
```

Дуже зручно.

Метод `.capitalize` в Ruby є зручним, але він робить великою тільки першу літеру. Для більшого контролю або для приголоснення кожного слова в рядку (що відомо як формат назви), ви можете захотіти використати метод `titleize` з розширення Rails ActiveSupport або реалізувати його самостійно:

```ruby
# Використання 'titleize' від ActiveSupport в Rails
require 'active_support/core_ext/string/inflections'
string = "hello world"
puts string.titleize # => "Hello World"
```

```ruby
# Власний варіант рішення
string = "hello world"
capitalized_each_word = string.split.map(&:capitalize).join(' ')
puts capitalized_each_word # => "Hello World"
```

Цей метод розділяє рядок на масив слів, робить кожне слово з великої літери, а потім з'єднує їх назад разом з пробілом.

Особисто я візьму цю ідею значно далі у моєму коді. Я написав власний метод [`titleize`, який враховує маленькі слова, такі як "a" та "the"](https://github.com/public-law/law_string/blob/master/lib/law_string.rb).
