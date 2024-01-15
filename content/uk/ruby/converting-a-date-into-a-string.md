---
title:                "Перетворення дати у рядок"
html_title:           "Ruby: Перетворення дати у рядок"
simple_title:         "Перетворення дати у рядок"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

#Чому
Багато Ruby-кодерів часто мають необхідність перетворювати дати у рядки (strings) для збереження в базах даних або відображення на сторінках веб-додатків.

##Як це зробити
Існує кілька шляхів конвертувати дати в рядки в Ruby. Перший - використовувати метод `strftime`, що дозволяє вказувати необхідний формат дати. Наприклад:

```ruby
current_date = Date.today
current_date_string = current_date.strftime("%d.%m.%Y") 
puts current_date_string # виведе "10.07.2021"
```

Інший спосіб - використовувати метод `to_s`, який за замовчуванням передає дату у форматі "yyyy-mm-dd":

```ruby
current_date = Date.today
current_date_string = current_date.to_s
puts current_date_string # виведе "2021-07-10"
```

Зверніть увагу, що у випадку з методом `strftime`, в певному форматі можна вказувати лише певні компоненти дати (рік, місяць, день), тоді як метод `to_s` повертає всі значення дати у вигляді рядка.

##Глибше
Якщо детальніше заглянути до методів `strftime` та `to_s`, можна помітити, що вони використовують деякі властивості класу `Date` та `DateTime`, який описує дату та час. Строкове представлення дати залежить від налаштувань локалізації (при наявності).

#Дивіться також
- [Ruby Date Class](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/Date.html)
- [Ruby DateTime Class](https://ruby-doc.org/stdlib-2.7.2/libdoc/date/rdoc/DateTime.html)
- [Date and Time Formatting in Ruby](https://www.rubyguides.com/2015/05/ruby-date-time/) (англійською)