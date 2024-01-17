---
title:                "Отримання поточної дати"
html_title:           "Elixir: Отримання поточної дати"
simple_title:         "Отримання поточної дати"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

Що це таке та чому це важливо?
Отримання поточної дати є важливою складовою частиною багатьох програмних проектів. Це дає можливість звертатись до поточної дати та часу для виконання різних функцій, таких як сортування даних або встановлення часових меж для інших процесів.

Як це зробити:
```
Elixir.DateTime.utc_now()
```

```
=> #DateTime<2020-09-12 14:33:01Z>
```

Глибоке копання:
Отримання поточної дати в Elixir базується на стандартній функції DateTime, що присутня у більшості мов програмування. Також існують альтернативні способи отримання дати у Elixir, такі як використання бібліотеки Timex або реалізування власної функції, яка буде повертати поточний час залежно від потреб проекту.

Дивіться також:
- Документація по DateTime у Elixir (https://hexdocs.pm/elixir/DateTime.html)
- Бібліотека Timex у Elixir (https://github.com/bitwalker/timex)
- Реалізація роботи з датами у проекті Ecto (https://hexdocs.pm/ecto/Ecto.DateTime.html)