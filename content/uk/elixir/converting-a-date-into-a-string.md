---
title:                "Перетворення дати в рядок"
html_title:           "Lua: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Що та навіщо?

Перетворення дати на рядок - це процес, під час якого ми змінюємо формат дати з числового на текстовий. Розробники роблять це, щоб забезпечити зручне переглядання та обробку дат в програмах.

## Як це робиться:

Для перетворення дати на рядок в Elixir використовують функцію `DateTime.to_string`.

```Elixir
date = DateTime.utc_now()
IO.puts DateTime.to_string(date)
```
Виходом цих команд буде поточна дата та час у виді рядка, такі як "2022-03-06 10:25:47.973800Z".

## Поглиблений огляд

### Історичний контекст

Elixir надає багато інструментів для роботи з датами. Проте функція `DateTime.to_string` була впроваджена в одній з останніх версій Elixir, щоб додати гнучкість при роботі з датами та уникнути необхідності користування сторонніми бібліотеками.

### Альтернативи

Якщо ви хочете змінити формат дати, використовуйте модуль `Calendar.strftime` з Elixir 1.11. 

```Elixir
date = DateTime.utc_now()
IO.puts Calendar.strftime(date, "%A %d %B %Y")
```

### Деталі реалізації

Функція `DateTime.to_string` використовує `Calendar.ISO` для перетворення дати на її ISO-8601 строковий еквівалент.

## Дивіться також:

- [Документація Elixir](https://hexdocs.pm/elixir/DateTime.html#to_string/1) - Докладно про DateTime.to_string
- [Підручник з Elixir про дату та час](https://elixir-lang.org/getting-started/io-and-the-filesystem.html#date-and-time) - Огляд щодо роботи з датою та часом в Elixir
- [Керівництво по Elixir strftime](https://hexdocs.pm/elixir/Calendar.html#strftime/3) - Деталі про форматування дати по заданому шаблону.