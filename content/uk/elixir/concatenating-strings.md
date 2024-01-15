---
title:                "Об'єднання рядків"
html_title:           "Elixir: Об'єднання рядків"
simple_title:         "Об'єднання рядків"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Чому

Розділення рядків є важливою частиною програмування, особливо в Еліксир. Це дозволяє створювати текстові повідомлення, надсилати дані до баз даних та взаємодіяти з користувачами. 

## Як це зробити

Один з способів злиття рядків - використання оператора `<>`. Давайте подивимось на приклад коду, як це працює в Еліксир:

```elixir
message = "Привіт"
name = "Олександр"
full_message = message <> ", " <> name

IO.puts full_message
```

Вивід: `Привіт, Олександр`

Також, можна використовувати функцію `String.concat/1`, яка приймає список рядків і повертає новий рядок. Ось приклад цього підходу:

```elixir
words = ["Привіт", "Олександр"]
full_message = String.concat(words)

IO.puts full_message
```

Вивід: `ПривітОлександр`

## Глибока занурення 

В Еліксирі є ще багато різних способів злиття рядків, наприклад, використання оператора `++` або функції `IO.puts`. Для більш детальної інформації, перегляньте [офіційну документацію](https://elixir-lang.org/getting-started/basic-types.html#strings). 

## Дивіться також

- [Офіційна документація з рядків Від Elixir](https://elixir-lang.org/getting-started/basic-types.html#strings)
- [Розширений посібник з Еліксиром](https://elixir-lang.org/getting-started/basic-types.html#strings)