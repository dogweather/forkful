---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:02.591766-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: Ruby \u0434\u0435\u043B\u0430\u0435\u0442 \u0440\u0430\u0431\u043E\
  \u0442\u0443 \u0441 \u0434\u0430\u0442\u0430\u043C\u0438 \u043E\u0447\u0435\u043D\
  \u044C \u0443\u0434\u043E\u0431\u043D\u043E\u0439 \u0441 \u043F\u043E\u043C\u043E\
  \u0449\u044C\u044E \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u043E\u0433\u043E\
  \ \u043A\u043B\u0430\u0441\u0441\u0430 `Date` \u0438 \u0434\u043E\u043F\u043E\u043B\
  \u043D\u0438\u0442\u0435\u043B\u044C\u043D\u043E\u0439 \u0431\u0438\u0431\u043B\u0438\
  \u043E\u0442\u0435\u043A\u0438 `active_support` \u0434\u043B\u044F \u043D\u0435\u043A\
  \u043E\u0442\u043E\u0440\u044B\u0445\u2026"
lastmod: '2024-03-13T22:44:46.022295-06:00'
model: gpt-4-0125-preview
summary: "Ruby \u0434\u0435\u043B\u0430\u0435\u0442 \u0440\u0430\u0431\u043E\u0442\
  \u0443 \u0441 \u0434\u0430\u0442\u0430\u043C\u0438 \u043E\u0447\u0435\u043D\u044C\
  \ \u0443\u0434\u043E\u0431\u043D\u043E\u0439 \u0441 \u043F\u043E\u043C\u043E\u0449\
  \u044C\u044E \u0432\u0441\u0442\u0440\u043E\u0435\u043D\u043D\u043E\u0433\u043E\
  \ \u043A\u043B\u0430\u0441\u0441\u0430 `Date` \u0438 \u0434\u043E\u043F\u043E\u043B\
  \u043D\u0438\u0442\u0435\u043B\u044C\u043D\u043E\u0439 \u0431\u0438\u0431\u043B\u0438\
  \u043E\u0442\u0435\u043A\u0438 `active_support` \u0434\u043B\u044F \u043D\u0435\u043A\
  \u043E\u0442\u043E\u0440\u044B\u0445 \u0443\u0434\u043E\u0431\u043D\u044B\u0445\
  \ \u0444\u0443\u043D\u043A\u0446\u0438\u0439."
title: "\u0420\u0430\u0441\u0447\u0435\u0442 \u0434\u0430\u0442\u044B \u0432 \u0431\
  \u0443\u0434\u0443\u0449\u0435\u043C \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\
  \u043B\u043E\u043C"
weight: 26
---

## Как это сделать:
Ruby делает работу с датами очень удобной с помощью встроенного класса `Date` и дополнительной библиотеки `active_support` для некоторых удобных функций. Вот как это делается:

```Ruby
require 'date'
require 'active_support/core_ext/integer'

# Получаем сегодняшнюю дату
today = Date.today
puts "Сегодня: #{today}"

# Рассчитываем дату через 10 дней
future_date = today + 10
puts "Через 10 дней будет: #{future_date}"

# Рассчитываем дату 30 дней назад
past_date = today - 30
puts "30 дней назад было: #{past_date}"

# Более сложные расчеты с active_support
puts "Через 2 месяца будет: #{2.months.from_now.to_date}"
puts "100 дней назад было: #{100.days.ago.to_date}"
```

Пример вывода:

```
Сегодня: 2023-04-07
Через 10 дней будет: 2023-04-17
30 дней назад было: 2023-03-08
Через 2 месяца будет: 2023-06-07
100 дней назад было: 2022-12-28
```

## Подробно
До того, как Ruby включил функциональность расчета дат в свои стандартные и дополнительные библиотеки, разработчикам часто приходилось вручную рассчитывать даты, учитывая високосные годы, разную длину месяцев и часовые пояса — довольно большая головная боль.

Стандартный класс `Date` делает многое "из коробки". Вы можете легко добавить (`+`) или вычесть (`-`) дни. Однако, для более интуитивного манипулирования временными периодами, например, "через 2 месяца", мы полагаемся на `active_support`, извлеченный из Ruby on Rails. Эта библиотека использует расширения для стандартных классов Ruby, делая такие расчеты удобными для восприятия.

При расчете прошлых или будущих дат учитывайте часовые пояса, если вы также учитываете время (`объекты DateTime` или `Time`). Класс `Time` Ruby и `active_support` могут с этим справиться, но требуют немного больше настройки.

Существуют альтернативы, такие как библиотеки `time-lord` и `ice_cube`, которые предлагают больше синтаксического сахара или специализированные функции (например, повторяющиеся события) соответственно.

## Смотрите также
- Работа с часовыми поясами в Ruby: [https://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html](https://api.rubyonrails.org/classes/ActiveSupport/TimeZone.html)
- Библиотека 'time-lord' для более человекопонятных выражений: [https://github.com/krainboltgreene/time-lord](https://github.com/krainboltgreene/time-lord)
- Библиотека 'ice_cube' для работы с повторяющимися событиями: [https://github.com/seejohnrun/ice_cube](https://github.com/seejohnrun/ice_cube)
