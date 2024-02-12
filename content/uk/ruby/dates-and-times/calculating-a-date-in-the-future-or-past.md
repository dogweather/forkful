---
title:                "Обчислення дати у майбутньому або минулому"
aliases:
- /uk/ruby/calculating-a-date-in-the-future-or-past/
date:                  2024-01-20T17:31:49.848402-07:00
model:                 gpt-4-1106-preview
simple_title:         "Обчислення дати у майбутньому або минулому"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що & Чому?
Обчислення дати в майбутньому або минулому — це процес знаходження дат поза сьогоденням. Програмісти роблять це для реалізації функціоналу, як-от нагадування, планування подій та архівація даних.

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
