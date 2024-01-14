---
title:                "Elixir: Отримання поточної дати"
programming_language: "Elixir"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Чому

Нагадати собі про поточну дату може бути дуже корисно, якщо ви працюєте з програмами або скриптами, які пов'язані зі змінами часу. Це може включати різні процеси, такі як резервне копіювання даних, планування завдань або відстеження часу виконання програм.

## Як

У Elixir є вбудована функція `DateTime.utc_now`, яка повертає поточний час у вигляді об'єкту `DateTime`. Щоб отримати поточну дату, ми можемо використати цю функцію і викликати метод `to_date`, щоб отримати дату в форматі "рік-місяць-день".

Це мав би виглядати так:

```Elixir
current_date = DateTime.utc_now |> to_date
IO.puts(current_date)
```

Як результат, ми отримаємо щось подібне до `2021-09-29`.

## Глибокий дайв

Варто зазначити, що функція `DateTime.utc_now` повертає значення у всесвітньому часі (UTC). Якщо ви хочете отримати поточну дату у локальному часі, вам потрібно буде додатково використати модуль `Calendar` та метод `to_local_time`.

Наприклад, якщо ви хочете отримати поточну дату у часовій зоні Києва, ви можете використати наступний код:

```Elixir
current_local_time = DateTime.utc_now |> Calendar.to_local_time("Europe/Kiev") |> to_date
IO.puts(current_local_time)
```

Результат буде виглядати приблизно так: `2021-09-29`.

## Дивіться також

- Офіційна документація Elixir щодо роботи з датою та часом: https://elixir-lang.org/getting-started/datetime.html
- Стаття про роботу зі зміною часу в Elixir: https://medium.com/@jeffweiss/converting-dates-across-timezones-in-elixir-4b6b9ae2a54d
- Розширена бібліотека для роботи з датою та часом у Elixir: https://github.com/lau/calendar