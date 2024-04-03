---
date: 2024-01-20 17:33:57.054866-07:00
description: "\u042F\u043A \u0446\u0435 \u0440\u043E\u0431\u0438\u0442\u0438: ."
lastmod: '2024-03-13T22:44:50.249444-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u041F\u043E\u0440\u0456\u0432\u043D\u044F\u043D\u043D\u044F \u0434\u0432\u043E\
  \u0445 \u0434\u0430\u0442"
weight: 27
---

## Як це робити:
```Ruby
require 'date'

date1 = Date.new(2023, 4, 5)
date2 = Date.new(2023, 4, 10)

# Порівняння за допомогою операторів
puts date1 > date2   # Виводить: false
puts date1 < date2   # Виводить: true
puts date1 == date2  # Виводить: false

# Різниця між датами у днях
difference = date2 - date1
puts difference.to_i # Виводить: 5
```

## Поглиблений Огляд:
В Ruby, об'єкти Date можна порівняти, як числа, тому що дати відлічують час від так званого "Epoch" - 1 січня 1970 року. Колись програмісти встановлювали дати вручну, але сьогодні ми маємо зручні бібліотеки, такі як `Date` і `Time`. Є також альтернативи, наприклад, бібліотека `ActiveSupport` з Rails надає розширені можливості для роботи з датами та часом. Коли порівнюєш дати, рубі перетворює їх в секунди від "Epoch", та порівнює ці значення.

## Див. також:
- [Ruby Date Class documentation](https://ruby-doc.org/stdlib-3.0.0/libdoc/date/rdoc/Date.html)
- [Ruby Time Class documentation](https://ruby-doc.org/core-3.0.0/Time.html)
- [ActiveSupport::TimeWithZone documentation](https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html)

Ці ресурси дадуть вам більше інформації про роботу з датами і часом у Ruby.
