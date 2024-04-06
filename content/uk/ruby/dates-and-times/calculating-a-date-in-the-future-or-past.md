---
date: 2024-01-20 17:31:49.848402-07:00
description: "\u042F\u043A \u0446\u0435 \u0437\u0440\u043E\u0431\u0438\u0442\u0438\
  : \u0412\u0438\u0445\u0456\u0434."
lastmod: '2024-04-05T21:53:50.273686-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u041E\u0431\u0447\u0438\u0441\u043B\u0435\u043D\u043D\u044F \u0434\u0430\u0442\
  \u0438 \u0443 \u043C\u0430\u0439\u0431\u0443\u0442\u043D\u044C\u043E\u043C\u0443\
  \ \u0430\u0431\u043E \u043C\u0438\u043D\u0443\u043B\u043E\u043C\u0443"
weight: 26
---

## Як це зробити:
```Ruby
require 'date'

# Сьогоднішня дата
today = Date.today
puts "Сьогодні: #{today}"

# Додавання 10 днів
future_date = today + 10
puts "Дата в майбутньому: #{future_date}"

# Відняття 5 днів
past_date = today - 5
puts "Дата в минулому: #{past_date}"
```
Вихід:
```
Сьогодні: 2023-04-12
Дата в майбутньому: 2023-04-22
Дата в минулому: 2023-04-07
```

## Підводимо підсумки
Обчислення дати — звичайна задача у програмуванні, яка має давні коріння ще з часів ранніх календарних систем. У Ruby це можна зробити просто за допомогою вбудованого класу `Date`. Є й інші джеми, як-от `active_support`, що надають додаткові можливості для роботи з часом. Важливо пам'ятати зміщення часових поясів та літнього часу при роботі з датами.

## Дивіться також:
- [Ruby Date Class](https://ruby-doc.org/stdlib-3.1.2/libdoc/date/rdoc/Date.html) — офіційна документація Ruby.
- [Active Support Core Extensions](https://edgeguides.rubyonrails.org/active_support_core_extensions.html) — розширення класів Ruby від Rails.
- [Understanding Time Zones in Ruby](https://thoughtbot.com/blog/its-about-time-zones) — стаття про часові зони у Ruby.
