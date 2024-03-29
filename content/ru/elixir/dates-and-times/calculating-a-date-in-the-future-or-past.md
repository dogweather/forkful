---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:55:50.115748-07:00
description: "\u041E\u043F\u0440\u0435\u0434\u0435\u043B\u0435\u043D\u0438\u0435 \u0431\
  \u0443\u0434\u0443\u0449\u0435\u0439 \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\
  \u043B\u043E\u0439 \u0434\u0430\u0442\u044B \u0432\u043A\u043B\u044E\u0447\u0430\
  \u0435\u0442 \u0432 \u0441\u0435\u0431\u044F \u0440\u0430\u0441\u0447\u0451\u0442\
  \ \u0434\u0430\u0442\u044B, \u043A\u043E\u0442\u043E\u0440\u0430\u044F \u043D\u0430\
  \u0441\u0442\u0443\u043F\u0438\u0442 \u0438\u043B\u0438 \u043D\u0430\u0441\u0442\
  \u0443\u043F\u0438\u043B\u0430 \u0441\u0442\u043E\u043B\u044C\u043A\u043E-\u0442\
  \u043E \u0434\u043D\u0435\u0439, \u043C\u0435\u0441\u044F\u0446\u0435\u0432 \u0438\
  \u043B\u0438 \u043B\u0435\u0442 \u043E\u0442 \u043E\u043F\u0440\u0435\u0434\u0435\
  \u043B\u0451\u043D\u043D\u043E\u0439 \u0442\u043E\u0447\u043A\u0438 \u0432\u043E\
  \u2026"
lastmod: '2024-03-13T22:44:44.455091-06:00'
model: gpt-4-0125-preview
summary: "\u041E\u043F\u0440\u0435\u0434\u0435\u043B\u0435\u043D\u0438\u0435 \u0431\
  \u0443\u0434\u0443\u0449\u0435\u0439 \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\
  \u043B\u043E\u0439 \u0434\u0430\u0442\u044B \u0432\u043A\u043B\u044E\u0447\u0430\
  \u0435\u0442 \u0432 \u0441\u0435\u0431\u044F \u0440\u0430\u0441\u0447\u0451\u0442\
  \ \u0434\u0430\u0442\u044B, \u043A\u043E\u0442\u043E\u0440\u0430\u044F \u043D\u0430\
  \u0441\u0442\u0443\u043F\u0438\u0442 \u0438\u043B\u0438 \u043D\u0430\u0441\u0442\
  \u0443\u043F\u0438\u043B\u0430 \u0441\u0442\u043E\u043B\u044C\u043A\u043E-\u0442\
  \u043E \u0434\u043D\u0435\u0439, \u043C\u0435\u0441\u044F\u0446\u0435\u0432 \u0438\
  \u043B\u0438 \u043B\u0435\u0442 \u043E\u0442 \u043E\u043F\u0440\u0435\u0434\u0435\
  \u043B\u0451\u043D\u043D\u043E\u0439 \u0442\u043E\u0447\u043A\u0438 \u0432\u043E\
  \u2026"
title: "\u0420\u0430\u0441\u0447\u0435\u0442 \u0434\u0430\u0442\u044B \u0432 \u0431\
  \u0443\u0434\u0443\u0449\u0435\u043C \u0438\u043B\u0438 \u043F\u0440\u043E\u0448\
  \u043B\u043E\u043C"
---

{{< edit_this_page >}}

## Что и Почему?
Определение будущей или прошлой даты включает в себя расчёт даты, которая наступит или наступила столько-то дней, месяцев или лет от определённой точки во времени. Программистам часто нужно это для отслеживания событий, планирования задач или управления сроками действия.

## Как:
Используя встроенный в Elixir модуль `Date`, вы можете легко оперировать временем.

```elixir
# Добавить или отнять от заданной даты
date_today = ~D[2023-04-15]
{year, month, day} = date_today

# Расчёт даты через 10 дней в будущем
date_future = Date.add(date_today, 10)
IO.inspect(date_future)  # => ~D[2023-04-25]

# Расчёт даты 30 дней назад
date_past = Date.add(date_today, -30)
IO.inspect(date_past)  # => ~D[2023-03-16]
```

Обратите внимание, как `Date.add/2` просто берёт число дней, на которое вы хотите путешествовать во временном континууме.

## Подробнее
Способность вычислять даты в будущем или прошлом не является новинкой. Исторические языки программирования тоже имели свои методы — подумайте о COBOL или FORTRAN. Однако Elixir привносит функциональный колорит и неизменяемость данных на стол, делая расчёты дат простыми и менее подверженными ошибкам.

Альтернативы? Вы могли бы вручную рассчитывать, добавляя секунды, минуты и так далее, но зачем изобретать велосипед, если Elixir предлагает надёжный модуль `Date`? Особенно учитывая, что временные расчёты могут быть сложными с учётом високосных лет, часовых поясов и изменений, связанных с переходом на летнее время.

Детали реализации заключаются в понимании модуля `:calendar` в Elixir и соответствующих реализаций на Erlang. Мы стоим на плечах эпох эволюции функционала даты и времени, при этом синтаксический сахар Elixir делает всё это ещё приятнее.

## Смотрите также
- Официальная документация модуля `Date` в Elixir: https://hexdocs.pm/elixir/Date.html
- "Дата, время и часовые пояса в Elixir": Статья, глубоко исследующая способности Elixir к работе со временем.
- Документация модуля `:calendar` в Erlang: http://erlang.org/doc/apps/erts/calendar.html
