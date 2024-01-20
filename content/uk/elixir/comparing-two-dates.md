---
title:                "Порівняння двох дат"
html_title:           "Clojure: Порівняння двох дат"
simple_title:         "Порівняння двох дат"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Що і чому?

Порівняння двох дат, це дія, коли ми визначаємо яка з двох дат раніша або пізніша. Програмісти роблять це, наприклад, для зорієнтуватися в часі, посортувати події або встановити таймер.

## Як до цього приступити:

```Elixir
# Створюємо дві дати у форматі { рік, місяць, день }
data1 = {2022, 1, 20}
data2 = {2022, 2, 28}

# Використаємо функцію compare/2 з модуля Date для порівняння дат
rezultat = Date.compare(data1, data2)

# Функція compare повертає:
# :lt, якщо перша дата раніша за другу
# :eq, якщо дати однакові
# :gt, якщо перша дата пізніша за другу

IO.puts rezultat  # Вивід: :lt
```

## Поглиблений матеріал:

1. Історичний контекст: Як і багато інших основних функціональних можливостей, порівняння дат було внесено в Elixir від самого початку. Це основний компонент для роботи з датами і часом.
2. Альтернативи: Інша популярна альтернатива - функція `Date.diff/2`, яку можна використовувати для визначення різниці в днях між двома датами.
3. Деталі реалізації: Функція `Date.compare/2` порівнює дати через конвертацію кожної дати до числа днів з часу, коли була встановлена Unix епоха (1 січня 1970 року), і потім порівнює ці числа.

## Додатково:

- [Elixir School: Dates and times](https://elixirschool.com/en/lessons/basics/date_time/)
- [Official Elixir Documentation: Date](https://hexdocs.pm/elixir/Date.html)
- [Pragmatic Studio: Working with Dates and Times in Elixir](https://pragmaticstudio.com/tutorials/elixir-dates-times-and-time-zones)