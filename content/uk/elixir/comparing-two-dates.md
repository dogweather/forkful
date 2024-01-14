---
title:    "Elixir: Порівняння двох дат."
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Чому

У цій статті ми розглянемо порівняння двох дат у Elixir. Іноді ця задача може стати досить складною, але ми покажемо вам, як швидко і просто вирішити це завдання.

## Як

Для початку, нам потрібно імпортувати модуль DateTime, щоби мати доступ до функцій, які дозволяють порівнювати дати.

```Elixir
import DateTime
```

Тепер, створимо дві змінні, якими будуть наші дати, і присвоїмо їм значення.

```Elixir
date1 = DateTime.new(2020, 10, 15)
date2 = DateTime.new(2020, 11, 20)
```

Далі, ми можемо використовувати функції `<`, `>`, `==` для порівняння дат.

```Elixir
date1 < date2 # поверне true
date1 > date2 # поверне false
date1 == date2 # поверне false
```

Ми також можемо використовувати функцію `compare` для отримання числового результату порівняння. Якщо перша дата менша, повернеться -1, якщо більша - повернеться 1, а якщо дати рівні - повернеться 0.

```Elixir
DateTime.compare(date1, date2) # поверне -1
```

## Глибокий аналіз

Коли порівнюється дві дати в Elixir, враховується не тільки рік, місяць і день, але і час та часовий пояс. Це означає, що дві дати можуть бути різними, навіть якщо вони мають рівні значення. Наприклад, якщо ми використовуємо функцію `==` для порівняння дат з різними часовими поясами, результат може бути непередбачуваним.

```Elixir
date1 = DateTime.new(2020, 10, 15, hour: 12, timezone: "Europe/Kiev")
date2 = DateTime.new(2020, 10, 15, hour: 12, timezone: "America/New_York")
date1 == date2 # поверне false, хоча дати насправді рівні
```

Тому важливо враховувати часові пояси при порівнянні дат.

## Дивіться також

- Documetation for DateTime module: https://hexdocs.pm/elixir/DateTime.html
- ElixirCast episode about working with dates and times: https://elixircasts.io/dates-and-times-in-elixir