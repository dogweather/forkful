---
title:    "Elixir: Перетворення дати в рядок"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

# Чому

Перетворення дати в ряд може бути корисним для відображення дати у зрозумілому форматі для користувачів або для подальшої обробки дати в програмі.

## Як це зробити

Бібліотека Elixir має вбудовану функцію `DateTime.to_string` для перетворення дати у ряд. Перш за все, створіть об'єкт `DateTime` і викличте функцію `to_string` на ньому:

```Elixir
date = DateTime.utc_now()
DateTime.to_string(date)
```

Результат буде виглядати наступним чином: `"2020-10-25T13:25:01.211867Z"`

Також можна використовувати додаткові параметри для налаштування формату виведення дати. Наприклад, якщо потрібно отримати дату в форматі `DD/MM/YYYY`, можна використати функцію `to_string!` з додатковим параметром `{format, "DD/MM/YYYY"}`:

```Elixir
DateTime.to_string!(date, {format, "DD/MM/YYYY"})
```

Результат буде виглядати так: `"25/10/2020"`

## Глибоке занурення

Для більш гнучкого управління форматуванням дати, можна використовувати модуль `DateTime.Format`:

```Elixir
format = "%A, %-d %B %Y г., %H:%M:%S"
DateTime.Format.format(format, date)
```

Результат буде виглядати наступним чином: `"Sunday, 25 October 2020 г., 13:25:01"`

Також можна створювати власні шаблони форматування з використанням метасимволів, наприклад `%Y` для поточного року.

# Дивись також

- [Elixir DateTime модуль](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir DateTime.Format модуль](https://hexdocs.pm/elixir/DateTime.Format.html)
- [Стаття про форматування дати в Elixir](https://www.davidverhasselt.com/formatting-dates-in-elixir/)
- [Відеоурок про роботу з датою та часом в Elixir](https://www.youtube.com/watch?v=np8Fc8nIMJE)