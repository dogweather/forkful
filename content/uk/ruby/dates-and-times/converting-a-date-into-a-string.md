---
title:                "Перетворення дати в рядок"
aliases:
- /uk/ruby/converting-a-date-into-a-string.md
date:                  2024-01-20T17:37:52.184115-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення дати в рядок"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що це таке & Навіщо?
Конвертація дати у рядок дозволяє представити дату у зручному для сприйняття форматі; це корисно для відображення на UI або при збереженні даних. Програмісти роблять це, щоби легше обмінюватися датами між системами та користувачами.

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
