---
title:                "Elixir: Перетворення дати в рядок"
simple_title:         "Перетворення дати в рядок"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Для чого

Конвертація дат в рядки є важливим кроком в роботі з датами в програмуванні. Наприклад, це може бути корисним при виведенні дати у вигляді тексту на екран або збереженні дати у файлі з розширенням CSV.

## Як

Для перетворення дати в рядок у Elixir, ми можемо застосувати функцію `DateTime.to_string/2`. Ця функція приймає два аргументи: дату, яку ми хочемо конвертувати, та формат рядка, в який ми хочемо перетворити дату.

```elixir
date = DateTime.utc_now()
DateTime.to_string(date, "{YYYY}-{MM}-{DD}")
```

В даному прикладі ми використовуємо `{YYYY}-{MM}-{DD}` формат для виведення дати у вигляді `РІК-МІСЯЦЬ-ДЕНЬ`.

 Деякі корисні формати рядків для дати включають `{MM}/{DD}/{YY}`, `{DD}/{MM}/{YY}`, `{DD} {Month}",` та `{Month} {DD}`, де `Month` виводиться у вигляді тексту (наприклад, "Січень", "Лютий" тощо).

## Глибока погруження

Під капотом функції `DateTime.to_string/2`, використовується модуль `DateTime.Format`, який містить кілька вбудованих форматів для дати. Ви також можете використовувати свої власні формати і вказати їх у другому аргументі функції.

```elixir
format = "%d %B %Y"
DateTime.to_string(date, format)
```

Зверніть увагу, що у форматі `%B` перший символ перетворюється до верхнього регістру (наприклад, "січень" стає "Січень").

## Дивись також

- [Функція `DateTime.to_string/2` документація](https://hexdocs.pm/elixir/DateTime.html#to_string/2)
- [Модуль `DateTime.Format` документація](https://hexdocs.pm/elixir/DateTime.Format.html)
- [Експериментуйте зі своїми власними форматами для дати у Elixir](https://hexdocs.pm/elixir/DateTime.Format.html#module-format-examples)