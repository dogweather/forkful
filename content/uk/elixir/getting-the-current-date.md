---
title:                "Отримання поточної дати"
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Що і чому?

Отримання поточної дати - це процес, за допомогою якого ми визначаємо дату і час в даний момент. Програмісти використовують це для роботи з часовими штампами, програмування згідно з розкладом і т.д.

## Як це зробити:

Отримання поточної дати в Elixir виконується з допомогою модуля `DateTime`. Нижче подано приклад коду:

```Elixir
DateTime.utc_now()
```

Виконання цього коду дасть наступний результат:

```Elixir
#DateTime<2022-04-02 16:39:17.033Z>
```

## Поглиблений погляд:

1. Стосовно історичного контексту: модуль `DateTime` в Elixir був упроваджений з версії 1.3 для покращення роботи з датою та часом.

2. Щодо альтернатив: також можна отримати поточну дату використовуючи Erlang, на якій будується Elixir. Ось як це можна зробити:

    ```Elixir
    :calendar.local_time()
    ```

3. Про технічні деталі: `DateTime.utc_now()` використовує координоване всесвітнє часове позначення (Coordinated Universal Time, UTC), яке є стандартом обчислення часу у всьому світі.

## Дивись також:

- [Документація Elixir для DateTime](https://hexdocs.pm/elixir/DateTime.html)
- [Документація Erlang для calendar](http://erlang.org/doc/man/calendar.html)