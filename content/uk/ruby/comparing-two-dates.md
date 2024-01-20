---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що й Чому?
Порівняння двох дат є функцією, яка дозволяє визначити, яка дата раніша, а яка пізніша. Програмісти використовують це для обробки часових рамок та впорядкування подій в часі.

## Як це зробити:
Подивимося як порівняти дві дати в Ruby. Припустимо ми маємо дві дати: `date1` і `date2`.

```Ruby
require 'date'

date1 = Date.new(2022, 6, 21)
date2 = Date.new(2022, 6, 22)

if date1 > date2
  puts "Дата1 пізніше Дати2"
elsif date1 < date2
  puts "Дата1 раніше Дати2"
else
  puts "Дата1 та Дата2 однакові"
end
```

В результаті, якщо запустити цей код, ви побачите "Дата1 раніше Дати2", тому що 21 червня було раніше ніж 22 червня.

## В глибинах:
Історично, системи дат були дуже різноманітні та багато в чому залежали від культури. Ruby використовує Григоріанський календар, який є найпоширенішим сьогодні.

Що стосується альтернатив, ви можете також використовувати часові мітки (timestamps) для порівняння часових точок. Але для високорівневих дат, використання об'єктів `Date` є більш зручним.

В Ruby, "Date" є вбудованим класом, який містить методи для виконання операцій порівняння. За замовчуванням, "Date" порівнює дати на основі днів від початку Ери (подібно до того, як Unix час вимірюється в секундах від 1970 року).

## Дивіться також:
- Ruby Date documentation: https://ruby-doc.org/stdlib-3.0.2/libdoc/date/rdoc/Date.html
- Gregorian calendar: https://en.wikipedia.org/wiki/Gregorian_calendar
- Unix time: https://en.wikipedia.org/wiki/Unix_time
- An Introduction to Ruby Time: https://www.rubyguides.com/ruby-tutorial/ruby-time/