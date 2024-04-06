---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:57:38.540094-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0418\u0441\u0442\u043E\u0440\u0438\u0447\u0435\u0441\u043A\u0438\
  \ Ruby \u044F\u0432\u043B\u044F\u0435\u0442\u0441\u044F \u044F\u0437\u044B\u043A\
  \u043E\u043C \u0441 \u0441\u0438\u043B\u044C\u043D\u044B\u043C \u0430\u043A\u0446\
  \u0435\u043D\u0442\u043E\u043C \u043D\u0430 \u043E\u0431\u0440\u0430\u0431\u043E\
  \u0442\u043A\u0443 \u0442\u0435\u043A\u0441\u0442\u0430, \u0443\u043D\u0430\u0441\
  \u043B\u0435\u0434\u043E\u0432\u0430\u0432 \u043D\u0435\u043A\u043E\u0442\u043E\u0440\
  \u044B\u0435 \u0441\u0432\u043E\u0438 \u0444\u0438\u043B\u043E\u0441\u043E\u0444\
  \u0438\u0438 \u043E\u0442 Perl. \u0412\u043E\u0442 \u043F\u043E\u0447\u0435\u043C\
  \u0443 \u043E\u043D\u2026"
lastmod: '2024-04-05T21:53:46.298472-06:00'
model: gpt-4-0125-preview
summary: "\u0418\u0441\u0442\u043E\u0440\u0438\u0447\u0435\u0441\u043A\u0438 Ruby\
  \ \u044F\u0432\u043B\u044F\u0435\u0442\u0441\u044F \u044F\u0437\u044B\u043A\u043E\
  \u043C \u0441 \u0441\u0438\u043B\u044C\u043D\u044B\u043C \u0430\u043A\u0446\u0435\
  \u043D\u0442\u043E\u043C \u043D\u0430 \u043E\u0431\u0440\u0430\u0431\u043E\u0442\
  \u043A\u0443 \u0442\u0435\u043A\u0441\u0442\u0430, \u0443\u043D\u0430\u0441\u043B\
  \u0435\u0434\u043E\u0432\u0430\u0432 \u043D\u0435\u043A\u043E\u0442\u043E\u0440\u044B\
  \u0435 \u0441\u0432\u043E\u0438 \u0444\u0438\u043B\u043E\u0441\u043E\u0444\u0438\
  \u0438 \u043E\u0442 Perl."
title: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u0441\u0438\u043C\u0432\u043E\
  \u043B\u043E\u0432, \u0441\u043E\u043E\u0442\u0432\u0435\u0442\u0441\u0442\u0432\
  \u0443\u044E\u0449\u0438\u0445 \u0448\u0430\u0431\u043B\u043E\u043D\u0443"
weight: 5
---

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
