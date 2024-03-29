---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:58:23.776510-07:00
description: "\u041F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\
  \u0443\u0449\u0435\u0439 \u0434\u0430\u0442\u044B \u0432 \u043F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0435 \u043F\u043E\u0445\u043E\u0436\u0435 \u043D\u0430\
  \ \u0432\u043E\u043F\u0440\u043E\u0441: \"\u041F\u0440\u0438\u0432\u0435\u0442,\
  \ \u043A\u0430\u043A\u0430\u044F \u0441\u0435\u0433\u043E\u0434\u043D\u044F \u0434\
  \u0430\u0442\u0430?\" \u041C\u044B \u0434\u0435\u043B\u0430\u0435\u043C \u044D\u0442\
  \u043E \u0434\u043B\u044F \u0442\u043E\u0433\u043E, \u0447\u0442\u043E\u0431\u044B\
  \ \u0441\u0442\u0430\u0432\u0438\u0442\u044C \u0432\u0440\u0435\u043C\u0435\u043D\
  \u043D\u0443\u044E \u043C\u0435\u0442\u043A\u0443 \u0441\u043E\u0431\u044B\u0442\
  \u0438\u044F\u043C,\u2026"
lastmod: '2024-03-13T22:44:44.449123-06:00'
model: gpt-4-0125-preview
summary: "\u041F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\
  \u0443\u0449\u0435\u0439 \u0434\u0430\u0442\u044B \u0432 \u043F\u0440\u043E\u0433\
  \u0440\u0430\u043C\u043C\u0435 \u043F\u043E\u0445\u043E\u0436\u0435 \u043D\u0430\
  \ \u0432\u043E\u043F\u0440\u043E\u0441: \"\u041F\u0440\u0438\u0432\u0435\u0442,\
  \ \u043A\u0430\u043A\u0430\u044F \u0441\u0435\u0433\u043E\u0434\u043D\u044F \u0434\
  \u0430\u0442\u0430?\" \u041C\u044B \u0434\u0435\u043B\u0430\u0435\u043C \u044D\u0442\
  \u043E \u0434\u043B\u044F \u0442\u043E\u0433\u043E, \u0447\u0442\u043E\u0431\u044B\
  \ \u0441\u0442\u0430\u0432\u0438\u0442\u044C \u0432\u0440\u0435\u043C\u0435\u043D\
  \u043D\u0443\u044E \u043C\u0435\u0442\u043A\u0443 \u0441\u043E\u0431\u044B\u0442\
  \u0438\u044F\u043C,\u2026"
title: "\u041F\u043E\u043B\u0443\u0447\u0435\u043D\u0438\u0435 \u0442\u0435\u043A\u0443\
  \u0449\u0435\u0439 \u0434\u0430\u0442\u044B"
---

{{< edit_this_page >}}

## Что и Почему?
Получение текущей даты в программе похоже на вопрос: "Привет, какая сегодня дата?" Мы делаем это для того, чтобы ставить временную метку событиям, управлять планированием или просто показывать пользователям, какой сегодня день.

## Как это сделать:
Elixir упрощает получение текущей даты с помощью модуля `Date`:

```elixir
# Получить текущую дату
current_date = Date.utc_today()

# Вывести ее
IO.inspect(current_date)
```

Пример вывода:

```elixir
~D[2023-04-06]
```

## Подробнее
В стародавние времена программисты работали с более примитивными языками и должны были вручную рассчитывать даты на основе секунд с начала эпохи (обычно с 1 января 1970 года). В настоящее время Elixir предоставляет модуль `Date`, упрощающий работу с датами.

К альтернативам относятся использование `DateTime.utc_now()`, если вам нужно точное время помимо даты, или `NaiveDateTime.local_now()`, если вы работаете с местным временем без информации о временной зоне.

Под капотом Elixir использует возможности Erlang для работы со временем. Когда вы вызываете `Date.utc_today()`, он взаимодействует с Erlang для получения Всемирного координированного времени (UTC).

## Смотрите также
- Документация по модулю `Date` в Elixir: https://hexdocs.pm/elixir/Date.html
- Модуль `DateTime` в Elixir для более сложных задач, связанных со временем: https://hexdocs.pm/elixir/DateTime.html
- Введение в `NaiveDateTime`: https://hexdocs.pm/elixir/NaiveDateTime.html
