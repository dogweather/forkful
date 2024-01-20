---
title:                "Обчислення дати в майбутньому або минулому"
html_title:           "Elixir: Обчислення дати в майбутньому або минулому"
simple_title:         "Обчислення дати в майбутньому або минулому"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Що і чому?

Вирахування дати в майбутньому або минулому - це процес знаходження дати, відхиленої від певної дати на фіксований часовий интервал. Програмісти роблять це, щоб моделювати події в часі.

## Як це зробити:

Мовою програмування Elixir ви можете скористатися модулем `Date`.

```Elixir
# Додавання днів
iex> future_date = Date.add(~D[2022-05-20], 5)
# Виведення: ~D[2022-05-25]

# Віднімання днів
iex> past_date = Date.add(~D[2022-05-20], -5)
# Виведення: ~D[2022-05-15]
```

Просто додайте або відніміть кількість днів від існуючої дати, щоб отримати нову дату.

## Занурення у деталі

Коли ми працюємо з датами, маємо врахувати різницю в часових поясах і форматі дат. Історично, контролювати це було складно, але Elixir полегшує цю задачу.

Існують інші способи робити це, наприклад, використовуючи Erlang. Це виглядає так: 

```Erlang
% Додавання днів
> {{Y, M, D}} = erlang:date(). 
> NewDate = erlang:date_to_gregorian_days({Y, M, D}) + 5.
> erlang:gregorian_days_to_date(NewDate).
 
% Віднімання днів
> {{Y, M, D}} = erlang:date(). 
> NewDate = erlang:date_to_gregorian_days({Y, M, D}) - 5.
> erlang:gregorian_days_to_date(NewDate). 
```

Але, Elixir є більш передовим та зручним у використанні.

## Читайте також:

1. [Date documentation in Elixir](https://hexdocs.pm/elixir/Date.html)