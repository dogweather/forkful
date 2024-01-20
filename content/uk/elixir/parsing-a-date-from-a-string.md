---
title:                "Аналіз дати з рядка"
html_title:           "C++: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Роздбираємо дату з рядка в Elixir (поточна версія)

## Що та навіщо?

Розбір дати з рядка, або парсинг, - це процес отримання дати з текстового рядка. Програмісти роблять це, коли їм потрібно маніпулювати датами або порівнювати їх у своєму коді.

## Як це робиться:
```Elixir
date_string = "2022-02-28" 
{:ok, date} = Date.from_iso8601(date_string)  
IO.inspect(date)
```
Виведення:
```elixir
~D[2022-02-28]
```
## Поглиблений матеріал.

1. **Історичний контекст**: Еліксир був створений у 2011 році, а модуль `Date` був доданий у версії 1.3 (2016 рік) щоб полегшити обробку дати й часу.
   
2. **Альтернативи**: CSV, XML та інші формати також часто містять дати у рядках. Для конвертації таких даних часто використовуються специфічні бібліотеки.

3. **Деталі реалізації**: Еліксир використовує `Calendar.ISO` модулі за замовчуванням при парсингу дат з рядків. Цей модуль належи до стандартної бібліотеки Elixir, але може бути замінений іншим.

## Додатково.
- Докладніше про Date.from_iso8601: [https://hexdocs.pm/elixir/Date.html#from_iso8601/1](https://hexdocs.pm/elixir/Date.html#from_iso8601/1)
- Інший, більш гнучкий спосіб розбору дати та часу в Elixir: [https://hexdocs.pm/elixir/DateTime.html#strptime/2](https://hexdocs.pm/elixir/DateTime.html#strptime/2)
- Інформація про інші формати дати та часу: [https://en.wikipedia.org/wiki/ISO_8601](https://en.wikipedia.org/wiki/ISO_8601)