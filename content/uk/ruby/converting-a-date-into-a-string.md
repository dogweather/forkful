---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що і чому?
Перетворення дати в рядок - це процес, коли ми перебиваємо дату з її звичайного формату в рядок формату. Програмісти це роблять, щоб легше відображати дату деінде або зберегти її в базах даних, що вимагають рядкового формату.

## Як це робиться:
Ось простий приклад коду на Ruby, який демонструє, як перетворити дату на рядок:

```Ruby
require 'date'

dt = Date.new(2022, 6, 7)
puts dt.to_s
```

Вихід цього коду буде таким:

```Ruby
"2022-06-07"
```

## Поглиблена інформація
Для перетворення дати в рядок ми використовуємо метод `to_s`, який був введений в Ruby 1.8. Раніше, у програмістів не було прямого способу зробити це.

Гарною альтернативою `to_s` у Ruby вважається метод `strftime`. Цей метод дає більше контролю, оскільки ви можете визначити формат вихідного рядка.

```Ruby
puts dt.strftime("%d/%m/%Y")
```

Ви отримаєте:

```Ruby
"07/06/2022"
```

Метод `to_s` просто повертає рядок, представлення дати за принципом ISO 8601.

## Дивіться також
1. Офіційна документація Ruby про [Class: Date](https://ruby-doc.org/stdlib-2.5.1/libdoc/date/rdoc/Date.html).
2. Матеріали StackOverflow на тему [строкового представлення дати в Ruby](https://stackoverflow.com/questions/1582431/ruby-way-to-convert-a-date-object-to-a-string).
3. Практичний посібник від [RubyGuides](https://www.rubyguides.com/2015/12/ruby-time/) про роботу з часом і датами на Ruby.