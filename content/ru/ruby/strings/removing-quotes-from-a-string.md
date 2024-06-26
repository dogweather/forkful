---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:01:38.667672-07:00
description: "\u041A\u0430\u043A: \u0423 Ruby \u0435\u0441\u0442\u044C \u043D\u0435\
  \u0441\u043A\u043E\u043B\u044C\u043A\u043E \u0445\u0438\u0442\u0440\u043E\u0441\u0442\
  \u0435\u0439 \u0434\u043B\u044F \u0443\u0434\u0430\u043B\u0435\u043D\u0438\u044F\
  \ \u044D\u0442\u0438\u0445 \u043D\u0430\u0434\u043E\u0435\u0434\u043B\u0438\u0432\
  \u044B\u0445 \u043A\u0430\u0432\u044B\u0447\u0435\u043A. \u0412\u044B \u043C\u043E\
  \u0436\u0435\u0442\u0435 \u0438\u0441\u043F\u043E\u043B\u044C\u0437\u043E\u0432\u0430\
  \u0442\u044C \u043C\u0435\u0442\u043E\u0434\u044B `gsub` \u0438\u043B\u0438 `delete`\
  \ \u0434\u043B\u044F \u0432\u044B\u043F\u043E\u043B\u043D\u0435\u043D\u0438\u044F\
  \ \u044D\u0442\u043E\u0439 \u0437\u0430\u0434\u0430\u0447\u0438.\u2026"
lastmod: '2024-03-13T22:44:45.972097-06:00'
model: gpt-4-0125-preview
summary: "\u0423 Ruby \u0435\u0441\u0442\u044C \u043D\u0435\u0441\u043A\u043E\u043B\
  \u044C\u043A\u043E \u0445\u0438\u0442\u0440\u043E\u0441\u0442\u0435\u0439 \u0434\
  \u043B\u044F \u0443\u0434\u0430\u043B\u0435\u043D\u0438\u044F \u044D\u0442\u0438\
  \u0445 \u043D\u0430\u0434\u043E\u0435\u0434\u043B\u0438\u0432\u044B\u0445 \u043A\
  \u0430\u0432\u044B\u0447\u0435\u043A."
title: "\u0423\u0434\u0430\u043B\u0435\u043D\u0438\u0435 \u043A\u0430\u0432\u044B\u0447\
  \u0435\u043A \u0438\u0437 \u0441\u0442\u0440\u043E\u043A\u0438"
weight: 9
---

## Как:
У Ruby есть несколько хитростей для удаления этих надоедливых кавычек. Вы можете использовать методы `gsub` или `delete` для выполнения этой задачи. Вот некоторый код для размышлений:

```ruby
# Использование gsub для удаления двойных и одинарных кавычек
quoted_string = "\"Say 'hello' to my little friend!\""
unquoted_string = quoted_string.gsub(/'|"/, '')
puts unquoted_string 
# Вывод: Say hello to my little friend!

# Если вы знаете, что будете иметь дело только с одним типом кавычек
single_quoted_string = "'Stay a while and listen!'"
clean_string = single_quoted_string.delete("'")
puts clean_string 
# Вывод: Stay a while and listen!
```

## Подробнее
История кавычек уходит корнями в самые ранние дни программирования, где они часто служили разделителями строк. Сейчас, как и тогда, вам может понадобиться удалить эти символы кавычек, когда они не нужны или когда они могут мешать хранению и манипуляции данными.

Мы говорили о `gsub` и `delete`, но есть и другие методы, как `tr` или `tr_s`, которые дают вам немного больше контроля или могут обрабатывать некоторые другие случаи использования:

```ruby
# tr также может удалять кавычки
double_quoted_string = "\"Do or do not, there is no try.\""
clean_string = double_quoted_string.tr('\"', '')
puts clean_string 
# Вывод: Do or do not, there is no try.
```

Помните, каждый из этих методов имеет свои случаи использования. `gsub` более мощный, когда вы имеете дело со сложными шаблонами или несколькими заменами. `delete` и `tr` прекрасно работают для простого, прямолинейного удаления символов.

## Смотрите также
Для дополнительного чтения и чтобы увидеть эти методы в действии в более крупных кодовых базах, ознакомьтесь с:
- Документацией Ruby для [String#gsub](https://ruby-doc.org/core-3.1.2/String.html#method-i-gsub), [String#delete](https://ruby-doc.org/core-3.1.2/String.html#method-i-delete) и [String#tr](https://ruby-doc.org/core-3.1.2/String.html#method-i-tr).
- У Ruby Monstas есть отличный [набор упражнений на строки](http://ruby-for-beginners.rubymonstas.org/built_in_classes/strings.html), включающий работу с кавычками.
- Обсуждения на Stack Overflow по [манипуляции со строками](https://stackoverflow.com/search?q=ruby+remove+quotes+from+string) предоставляют проблемы и решения из реального мира от других ruby-программистов.
