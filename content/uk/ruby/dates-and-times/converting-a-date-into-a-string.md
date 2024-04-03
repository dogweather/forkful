---
date: 2024-01-20 17:37:52.184115-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : ."
lastmod: '2024-03-13T22:44:50.247841-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0434\
  \u0430\u0442\u0438 \u0432 \u0440\u044F\u0434\u043E\u043A"
weight: 28
---

## Як це зробити:
```Ruby
require 'date'

# Створення об'єкта дати
date = Date.new(2023, 4, 1)

# Конвертація у рядок у стандартному форматі
date_string = date.to_s
puts date_string  # Вивід: 2023-04-01

# Конвертація з форматуванням
formatted_date_string = date.strftime('%d-%m-%Y')
puts formatted_date_string  # Вивід: 01-04-2023

# Більше форматувань
friendly_date_string = date.strftime('%A, %d %B %Y')
puts friendly_date_string  # Вивід: Saturday, 01 April 2023
```

## Поглиблений розгляд
У старших версіях Ruby для роботи із датами потрібен був зовнішній гем, але з появою стандартної бібліотеки Date вона стала вбудованою. Метод `strftime` дозволяє детально налаштовувати формат виведення дати, використовуючи різноманітні директиви для дня, місяця, року тощо. Існують також альтернативи, наприклад: бібліотеки Time і DateTime, які надають додаткові функції для роботи із часовими зонами.

## Також загляньте:
- [Клас Date у документації Ruby](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [Довідка по методу strftime](https://apidock.com/ruby/DateTime/strftime)
- [Ruby DateTime документація](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/DateTime.html)
