---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:15:38.501008-07:00
description: "\u0410\u043D\u0430\u043B\u0456\u0437 (\u043F\u0430\u0440\u0441\u0438\
  \u043D\u0433) \u0434\u0430\u0442\u0438 \u0437\u0456 \u0441\u0442\u0440\u043E\u043A\
  \u0438 \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0432 \u043F\u0435\u0440\u0435\
  \u0442\u0432\u043E\u0440\u0435\u043D\u043D\u0456 \u0442\u0435\u043A\u0441\u0442\u0443\
  , \u0449\u043E \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\u043B\u044F\u0454\
  \ \u0434\u0430\u0442\u0443, \u043D\u0430 \u043E\u0431\u2019\u0454\u043A\u0442 `Date`\
  \ \u0430\u0431\u043E `DateTime`, \u044F\u043A\u0438\u0439 \u0440\u043E\u0437\u0443\
  \u043C\u0456\u0454 Ruby. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\
  \u0438\u2026"
lastmod: '2024-03-13T22:44:50.243894-06:00'
model: gpt-4-0125-preview
summary: "\u0410\u043D\u0430\u043B\u0456\u0437 (\u043F\u0430\u0440\u0441\u0438\u043D\
  \u0433) \u0434\u0430\u0442\u0438 \u0437\u0456 \u0441\u0442\u0440\u043E\u043A\u0438\
  \ \u043F\u043E\u043B\u044F\u0433\u0430\u0454 \u0432 \u043F\u0435\u0440\u0435\u0442\
  \u0432\u043E\u0440\u0435\u043D\u043D\u0456 \u0442\u0435\u043A\u0441\u0442\u0443\
  , \u0449\u043E \u043F\u0440\u0435\u0434\u0441\u0442\u0430\u0432\u043B\u044F\u0454\
  \ \u0434\u0430\u0442\u0443, \u043D\u0430 \u043E\u0431\u2019\u0454\u043A\u0442 `Date`\
  \ \u0430\u0431\u043E `DateTime`, \u044F\u043A\u0438\u0439 \u0440\u043E\u0437\u0443\
  \u043C\u0456\u0454 Ruby. \u041F\u0440\u043E\u0433\u0440\u0430\u043C\u0456\u0441\u0442\
  \u0438\u2026"
title: "\u0420\u043E\u0437\u0431\u0456\u0440 \u0434\u0430\u0442\u0438 \u0437 \u0440\
  \u044F\u0434\u043A\u0430"
---

{{< edit_this_page >}}

## Що і чому?
Аналіз (парсинг) дати зі строки полягає в перетворенні тексту, що представляє дату, на об’єкт `Date` або `DateTime`, який розуміє Ruby. Програмісти роблять це, щоб виконувати операції, як-от порівняння, обчислення або форматування дат, що є загальними завданнями в додатках, пов’язаних із плануванням, аналітикою або обробкою даних.

## Як це зробити:
У Ruby стандартна бібліотека надає прямі способи аналізу дат з рядків за допомогою класів `Date` та `DateTime`. Ось як це робиться за допомогою вбудованих методів Ruby:

```ruby
require 'date'

# Аналіз дати з рядка
date_string = "2023-04-01"
parsed_date = Date.parse(date_string)
puts parsed_date
# => 2023-04-01

# DateTime для детальнішого представлення часу
datetime_string = "2023-04-01T15:30:45+00:00"
parsed_datetime = DateTime.parse(datetime_string)
puts parsed_datetime
# => 2023-04-01T15:30:45+00:00
```

Для більшого контролю або для обробки форматів, які метод `parse` може не розпізнати напряму, можна використовувати `strptime` (аналіз рядка часу), вказуючи формат явно:

```ruby
# Використання strptime для спеціальних форматів
custom_date_string = "01-04-2023"
parsed_date_custom = Date.strptime(custom_date_string, '%d-%m-%Y')
puts parsed_date_custom
# => 2023-04-01
```

### Використання сторонніх бібліотек:

Хоча вбудовані можливості Ruby є потужними, іноді вам може знадобитися стороння бібліотека для додаткових функцій або простішого синтаксису. Одним з популярних виборів є гем `Chronic` для аналізу природної мови:

1. Спочатку додайте Chronic до вашого Gemfile і виконайте `bundle install`:
```ruby
gem 'chronic'
```

2. Потім використовуйте його так:
```ruby
require 'chronic'

parsed_chronic = Chronic.parse('next Tuesday')
puts parsed_chronic
# Вивід буде змінюватися залежно від поточної дати; припускається аналіз на 2023-04-01
# => 2023-04-04 12:00:00 +0000
```

`Chronic` дуже корисний для обробки вводу користувачів, оскільки він може розуміти широкий діапазон форматів дат природної мови, що робить його потужним інструментом для додатків, які вимагають гнучкого введення дат.
