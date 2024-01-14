---
title:                "Ruby: Капіталізація рядка"
programming_language: "Ruby"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Навіщо

Часто у програмуванні потрібно використовувати різні методи для перетворення тексту. Одним з таких методів є `capitalize`, який робить першу літеру рядка великою. Це може бути корисно, наприклад, для правильного підписування назви користувача або виправлення помилок в словах, що починаються з малої літери.

## Як

Щоб скористатися методом `capitalize` в Ruby, просто викличте його на рядку, який потрібно змінити:

```Ruby
"привіт".capitalize    # Виведе "Привіт"
```

Якщо рядок вже містить велику літеру, то метод `capitalize` залишить все без змін:

```Ruby
"Привіт".capitalize    # Виведе те ж "Привіт"
```

Також існує метод `capitalize!`, який змінює оригінальний рядок, а не повертає новий.

```Ruby
s = "привіт"
s.capitalize!          # Змінить рядок s на "Привіт"
puts s                 # Виведе "Привіт"
```

## Глибоке дослідження

Натомість, якщо потрібно змінити першу літеру кожного слова в рядку на велику, тоді можна скористатися методом `capitalize_words` з бібліотеки `strscan`. Він працює наступним чином:

```Ruby
require 'strscan'
s = "привіт, як справи?"
s = s.capitalize_words     # Змінить рядок на "Привіт, Як Справи?"
```

Зверніть увагу, що перша літера слова "як" тепер теж велика, адже метод `capitalize_words` ігнорує слова з великими літерами.

## Дивіться також

- [Документація по методу `capitalize` у Ruby](https://ruby-doc.org/core-2.7.2/String.html#method-i-capitalize)
- [Документація по методу `capitalize!` у Ruby](https://ruby-doc.org/core-2.7.2/String.html#method-i-capitalize-21)
- [Документація по бібліотеці `strscan` у Ruby](https://ruby-doc.org/stdlib-2.7.2/libdoc/strscan/rdoc/StringScanner.html#method-i-capitalize_words)