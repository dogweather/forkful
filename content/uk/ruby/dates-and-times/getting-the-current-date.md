---
title:                "Отримання поточної дати"
aliases:
- /uk/ruby/getting-the-current-date.md
date:                  2024-02-03T19:10:57.627190-07:00
model:                 gpt-4-0125-preview
simple_title:         "Отримання поточної дати"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/ruby/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Що та Чому?
Отримання поточної дати є суттєвим завданням у майже будь-якому програмувальному проєкті, від логування діяльності в додатку до генерації звітів із датами. У Ruby це можна легко здійснити за допомогою стандартної бібліотеки, спрощуючи операції, які включають роботу з датами.

## Як це зробити:
Стандартна бібліотека Ruby включає класи `Date` та `Time` для роботи з датами та часом. Ось як отримати поточну дату:

```ruby
require 'date'

current_date = Date.today
puts current_date
```

Приклад виводу: 
```
2023-04-12
```

Для включення часу до дати більш підходящим є клас `Time` Ruby:

```ruby
current_time = Time.now
puts current_time
```

Приклад виводу: 
```
2023-04-12 14:33:07 +0200
```

Якщо вам потрібно більше функціоналу, наприклад, управління часовими зонами, ви можете використовувати сторонній gem як-от `ActiveSupport` (частина Rails, але може використовуватися самостійно).

Спочатку додайте `activesupport` до вашого Gemfile і виконайте `bundle install`:

```ruby
gem 'activesupport'
```

Потім використовуйте його для управління часовими зонами:

```ruby
require 'active_support/time'

Time.zone = 'Eastern Time (US & Canada)'  # Встановіть бажану часову зону
current_time_with_zone = Time.zone.now
puts current_time_with_zone
```

Приклад виводу:
```
Wed, 12 Apr 2023 08:33:07 EDT -04:00
```
