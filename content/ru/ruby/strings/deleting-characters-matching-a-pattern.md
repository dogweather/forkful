---
title:                "Удаление символов, соответствующих шаблону"
aliases: - /ru/ruby/deleting-characters-matching-a-pattern.md
date:                  2024-01-28T23:57:38.540094-07:00
model:                 gpt-4-0125-preview
simple_title:         "Удаление символов, соответствующих шаблону"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/ruby/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и Почему?
Удаление символов, соответствующих шаблону в строках, заключается в точечном удалении ненужных фрагментов, например, удаление хэштегов из твитов. Программисты делают это для очистки данных, их стандартизации или подготовки к дальнейшей обработке.

## Как это сделать:
```Ruby
# Простое удаление с использованием String#gsub
example = "Hello, #World!"
cleaned_example = example.gsub(/#/, '') # => "Hello, World!"

puts cleaned_example # Вывод: Hello, World!

# Удаление последовательности символов
sequence_example = "Th1s is 2 an example3."
cleaned_sequence = sequence_example.gsub(/[0-9]/, '') # => "This is an example."

puts cleaned_sequence # Вывод: This is an example.

# Удаление с использованием String#delete
delete_example = "Remove vowels from this line."
cleaned_delete = delete_example.delete('aeiou') # => "Rmv vwls frm ths ln."

puts cleaned_delete # Вывод: Rmv vwls frm ths ln.
```

## Подробнее
Исторически Ruby является языком с сильным акцентом на обработку текста, унаследовав некоторые свои философии от Perl. Вот почему он предоставляет вам инструменты вроде `gsub` и `delete` прямо из коробки.

`gsub` означает глобальная замена. Часто его используют для замены частей строк, соответствующих шаблону (регулярное выражение), на другую строку. При использовании пустой строки замены он эффективно удаляет соответствующие символы.

`delete` менее гибкий, чем `gsub`, но быстрее, когда вам просто нужно удалить определенные символы. Вы не можете использовать регулярные выражения с `delete`, но для простого удаления символов это простой выбор.

Есть и другие способы справиться с этой задачей. Библиотеки вроде `scan` и `split` могут разбирать строки на части, и затем вы можете собрать их уже без нежелательных символов. Но для прямого удаления символов `gsub` и `delete` - ваши лучшие друзья.

## Смотрите также
- Документация Ruby `gsub`: [Ruby Doc gsub](https://ruby-doc.org/core-3.1.0/String.html#method-i-gsub)
- Документация Ruby `delete`: [Ruby Doc delete](https://ruby-doc.org/core-3.1.0/String.html#method-i-delete)
- Регулярные выражения в Ruby: [Ruby Regexp](https://ruby-doc.org/core-3.1.0/Regexp.html)
- "Programming Ruby: The Pragmatic Programmer’s Guide" для глубокого изучения возможностей обработки текста в Ruby.
