---
title:                "Розрахунок дати у майбутньому або минулому"
html_title:           "Ruby: Розрахунок дати у майбутньому або минулому"
simple_title:         "Розрахунок дати у майбутньому або минулому"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

---

## Що і чому?

Обрахування дати у майбутньому або минулому - це знаходження конкретної дати, додавши або віднявши певну кількість днів, тижнів, місяців або років від заданої дати. Програмісти роблять це, єдинственное слово при створенні функцій, пов'язаних з плануванням, прогнозуванням або відслідковуванням зміни часу.

## Як це зробити:

В Ruby є декілька відмінних бібліотек для роботи з датами. Ось кілька прикладів:

```Ruby
require 'date'

# Обрахування дати в майбутньому
future_date = Date.today + 5
puts future_date

# Обрахування дати в минулому
past_date = Date.today - 10
puts past_date
```

## Поглиблений розділ:

У минулому, програмісти частіше використовували бібліотеку Time для обрахунку дат, але вона не повністю підтримує григоріанський календар. Бібліотека Date була введена в Ruby 1.9, щоб вирішити цю проблему.

Існують інші альтернативи, наприклад, бібліотека `active_support/time`.
Ця бібліотека надає ще більше методів і можливостей для обрахунку дат. Наприклад:

```Ruby
require 'active_support/time'

# Обрахування дати в майбутньому
future_date = 5.days.from_now
puts future_date

# Обрахування дати в минулому
past_date = 10.days.ago
puts past_date
```

Однак, якщо вам не потрібні додаткові можливості active_support, ви можете залишитися з бібліотекою Date.

## Дивіться також:

1. Офіційна документація Ruby про бібліотеки для роботи з часом і датами:
   - `Date`: https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html
   - `Time`: https://ruby-doc.org/stdlib-2.7.0/libdoc/time/rdoc/Time.html
   - `active_support/time`: https://api.rubyonrails.org/classes/ActiveSupport/TimeWithZone.html

2. Цікава стаття про роботу з датами і часом у Ruby: [Working with Time in Ruby](https://www.toptal.com/software/working-with-dates-in-ruby-on-rails).

---