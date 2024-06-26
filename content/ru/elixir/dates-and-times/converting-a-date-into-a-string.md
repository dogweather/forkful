---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:56:20.884545-07:00
description: "\u041A\u0430\u043A: \u0412 Elixir \u043C\u043E\u0434\u0443\u043B\u044C\
  \ `Date` \u0441\u043E\u0434\u0435\u0440\u0436\u0438\u0442 \u0444\u0443\u043D\u043A\
  \u0446\u0438\u044E `to_string/1`, \u043A\u043E\u0442\u043E\u0440\u0430\u044F \u043F\
  \u0440\u0435\u043E\u0431\u0440\u0430\u0437\u0443\u0435\u0442 \u0434\u0430\u0442\u0443\
  \ \u0432 \u0441\u0442\u0440\u043E\u043A\u0443."
lastmod: '2024-03-13T22:44:44.451294-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Elixir \u043C\u043E\u0434\u0443\u043B\u044C `Date` \u0441\u043E\u0434\
  \u0435\u0440\u0436\u0438\u0442 \u0444\u0443\u043D\u043A\u0446\u0438\u044E `to_string/1`,\
  \ \u043A\u043E\u0442\u043E\u0440\u0430\u044F \u043F\u0440\u0435\u043E\u0431\u0440\
  \u0430\u0437\u0443\u0435\u0442 \u0434\u0430\u0442\u0443 \u0432 \u0441\u0442\u0440\
  \u043E\u043A\u0443."
title: "\u041F\u0440\u0435\u043E\u0431\u0440\u0430\u0437\u043E\u0432\u0430\u043D\u0438\
  \u0435 \u0434\u0430\u0442\u044B \u0432 \u0441\u0442\u0440\u043E\u043A\u0443"
weight: 28
---

## Как:
В Elixir модуль `Date` содержит функцию `to_string/1`, которая преобразует дату в строку.

```elixir
date = ~D[2023-03-14]
date_string = Date.to_string(date)
IO.puts(date_string)  # "2023-03-14"
```

Для более индивидуального форматирования вы можете использовать `Timex`:
```elixir
{:ok, datetime} = DateTime.new(~D[2023-03-14], {0, 0, 0})
formatted_date = Timex.format!(datetime, "{YYYY}-{0M}-{0D}")
IO.puts(formatted_date)  # "2023-03-14"
```

## Подробнее
До версии Elixir 1.3 манипулирование датами и временем было более громоздким и зависимым от сторонних библиотек. Начиная с версии 1.3 и позднее, Elixir включил модули `Date`, `Time` и `DateTime` для лучшего управления датами и временем.

Когда вам нужно форматирование за пределами стандарта ISO8601, рассмотрите библиотеку `Timex`, пакет Elixir, предоставляющий полный опыт работы с датой-временем.

Преобразование в строку не является магией. Речь идет о представлении сложной структуры `Date` во что-то универсально понятное. Структура содержит больше информации, чем ее строковое представление, поэтому имейте в виду, что преобразование обратно из строки в дату потеряет этот дополнительный контекст, если он не закодирован должным образом.

## См. также
- Модуль Date в Elixir: https://hexdocs.pm/elixir/Date.html
- Документация Timex: https://hexdocs.pm/timex/readme.html
- Формат ISO8601: https://ru.wikipedia.org/wiki/ISO_8601
