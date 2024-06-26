---
changelog:
- 2024-01-29, gpt-4-0125-preview, translated from English
date: 2024-01-29 00:02:17.686940-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Ruby \u044D\u0442\u043E \u043B\u0435\u0433\u043A\u043E. \u0418\
  \u0441\u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435 `gsub` \u0434\u043B\
  \u044F \u0433\u043B\u043E\u0431\u0430\u043B\u044C\u043D\u043E\u0439 \u0437\u0430\
  \u043C\u0435\u043D\u044B \u0442\u0435\u043A\u0441\u0442\u0430, \u0438\u043B\u0438\
  \ `sub` \u0434\u043B\u044F \u043E\u0434\u043D\u043E\u0433\u043E \u0441\u043B\u0443\
  \u0447\u0430\u044F. \u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439\
  \ \u043F\u0440\u0438\u043C\u0435\u0440."
lastmod: '2024-04-05T22:38:45.104358-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Ruby \u044D\u0442\u043E \u043B\u0435\u0433\u043A\u043E. \u0418\u0441\
  \u043F\u043E\u043B\u044C\u0437\u0443\u0439\u0442\u0435 `gsub` \u0434\u043B\u044F\
  \ \u0433\u043B\u043E\u0431\u0430\u043B\u044C\u043D\u043E\u0439 \u0437\u0430\u043C\
  \u0435\u043D\u044B \u0442\u0435\u043A\u0441\u0442\u0430, \u0438\u043B\u0438 `sub`\
  \ \u0434\u043B\u044F \u043E\u0434\u043D\u043E\u0433\u043E \u0441\u043B\u0443\u0447\
  \u0430\u044F. \u0412\u043E\u0442 \u0431\u044B\u0441\u0442\u0440\u044B\u0439 \u043F\
  \u0440\u0438\u043C\u0435\u0440."
title: "\u041F\u043E\u0438\u0441\u043A \u0438 \u0437\u0430\u043C\u0435\u043D\u0430\
  \ \u0442\u0435\u043A\u0441\u0442\u0430"
weight: 10
---

## Как это сделать:
В Ruby это легко. Используйте `gsub` для глобальной замены текста, или `sub` для одного случая. Вот быстрый пример:

```ruby
# Исходная строка
phrase = "Hello, world!"

# Замените 'world' на 'Ruby'
puts phrase.gsub('world', 'Ruby')
# => Hello, Ruby!

# Замените только первое вхождение 'l'
puts phrase.sub('l', '7')
# => He7lo, world!
```
Выходные данные? Первый принт показывает `"Hello, Ruby!"`, второй дает `"He7lo, world!"`.

## Подробнее
Методы `gsub` и `sub` существуют в Ruby с его первых дней, отражая концепцию замены из более старых языков, таких как Perl. Альтернативы? Конечно, вы могли бы использовать регулярные выражения для более сложных шаблонов, или даже соединить `split` и `join`, если чувствуете в себе творческую жилку.

Что круто, так это возможность Ruby использовать блоки с `gsub`. Вместо простого поиска и замены вы можете сделать серьезную работу внутри этого блока:

```ruby
# Сделать каждое слово с заглавной буквы
puts "make me pretty".gsub(/\b\w/) { |match| match.upcase }
# => Make Me Pretty
```

Зачем это нужно? Для начала, использование регулярных выражений с `gsub` позволяет вам бороться с нюансированными случаями, когда вам нужно больше утонченности, чем простое 'найди это, замени на то'.

## Смотрите также
Оттачивайте свои навыки - окунитесь в документацию или ознакомьтесь с этими ресурсами:
- [Документация Ruby String#gsub](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)
- [Регулярные выражения в Ruby](https://www.rubyguides.com/2015/06/ruby-regex/)

Поняли? Отлично. Теперь идите играть со строками.
