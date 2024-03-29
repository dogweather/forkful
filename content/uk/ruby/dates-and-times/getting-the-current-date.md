---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:57.627190-07:00
description: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\
  \u043E\u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438 \u0454 \u0441\u0443\u0442\
  \u0442\u0454\u0432\u0438\u043C \u0437\u0430\u0432\u0434\u0430\u043D\u043D\u044F\u043C\
  \ \u0443 \u043C\u0430\u0439\u0436\u0435 \u0431\u0443\u0434\u044C-\u044F\u043A\u043E\
  \u043C\u0443 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043B\u044C\
  \u043D\u043E\u043C\u0443 \u043F\u0440\u043E\u0454\u043A\u0442\u0456, \u0432\u0456\
  \u0434 \u043B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F \u0434\u0456\u044F\
  \u043B\u044C\u043D\u043E\u0441\u0442\u0456 \u0432 \u0434\u043E\u0434\u0430\u0442\
  \u043A\u0443 \u0434\u043E \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0456\u0457\
  \ \u0437\u0432\u0456\u0442\u0456\u0432 \u0456\u0437\u2026"
lastmod: '2024-03-13T22:44:50.246053-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\
  \u043E\u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438 \u0454 \u0441\u0443\u0442\
  \u0442\u0454\u0432\u0438\u043C \u0437\u0430\u0432\u0434\u0430\u043D\u043D\u044F\u043C\
  \ \u0443 \u043C\u0430\u0439\u0436\u0435 \u0431\u0443\u0434\u044C-\u044F\u043A\u043E\
  \u043C\u0443 \u043F\u0440\u043E\u0433\u0440\u0430\u043C\u0443\u0432\u0430\u043B\u044C\
  \u043D\u043E\u043C\u0443 \u043F\u0440\u043E\u0454\u043A\u0442\u0456, \u0432\u0456\
  \u0434 \u043B\u043E\u0433\u0443\u0432\u0430\u043D\u043D\u044F \u0434\u0456\u044F\
  \u043B\u044C\u043D\u043E\u0441\u0442\u0456 \u0432 \u0434\u043E\u0434\u0430\u0442\
  \u043A\u0443 \u0434\u043E \u0433\u0435\u043D\u0435\u0440\u0430\u0446\u0456\u0457\
  \ \u0437\u0432\u0456\u0442\u0456\u0432 \u0456\u0437\u2026"
title: "\u041E\u0442\u0440\u0438\u043C\u0430\u043D\u043D\u044F \u043F\u043E\u0442\u043E\
  \u0447\u043D\u043E\u0457 \u0434\u0430\u0442\u0438"
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
