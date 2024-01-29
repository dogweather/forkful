---
title:                "Преобразование строки в верхний регистр"
date:                  2024-01-28T23:55:45.899724-07:00
model:                 gpt-4-0125-preview
simple_title:         "Преобразование строки в верхний регистр"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?

Преобразование строки в строку с заглавной буквы означает преобразование первого символа в верхний регистр и остальных в нижний. Программисты делают это для форматирования вывода данных для обеспечения консистентности или для соответствия определенным стандартам данных.

## Как:

В Ruby преобразовать строку с заглавной буквы можно с помощью метода `.capitalize`:

```Ruby
puts "hello world".capitalize  # Вывод: "Hello world"
```

Чтобы сделать с заглавной буквы все слова в строке, используйте:

```Ruby
puts "hello world".split.map(&:capitalize).join(' ')  # Вывод: "Hello World"
```

Обратите внимание, что `.capitalize` влияет только на первое слово:

```Ruby
puts "hello WORLD".capitalize  # Вывод: "Hello world"
```

## Подробнее

Преобразование строк в строку с заглавной буквы необходимо с тех пор, как компьютеры начали взаимодействовать с людьми. Это обеспечивает корректное написание собственных имен и начала предложений, соответствуя грамматическим стандартам.

В некоторых языках, таких как Ruby, метод `.capitalize` встроен. В других необходимы пользовательские функции или библиотеки. Метод в Ruby также переводит в нижний регистр остальную часть строки, что видно в приведенных выше примерах.

Альтернативой в Ruby является использование метода `titleize` из методов `ActiveSupport::Inflector`, часто используемого в Rails:

```Ruby
require 'active_support/core_ext/string/inflector'
puts "hello world".titleize  # Вывод: "Hello World"
```

Однако `titleize` является более тяжеловесным и не входит в стандартную библиотеку Ruby.

С точки зрения реализации, когда вы вызываете `.capitalize`, Ruby создает новую строку с первым символом, преобразованным в верхний регистр, и остальными в нижний. Это удобно для обеспечения консистентности форматирования в пользовательских интерфейсах и при обработке данных.

## Смотрите также

- Документация Ruby по `.capitalize`: [Ruby Docs - capitalize](https://ruby-doc.org/core-2.7.0/String.html#method-i-capitalize)
- О `ActiveSupport::Inflector` и `titleize`: [API Dock - titleize](https://apidock.com/rails/String/titleize)
- Чтобы узнать о других методах строк в Ruby: [Ruby Docs - String](https://ruby-doc.org/core-2.7.0/String.html)
