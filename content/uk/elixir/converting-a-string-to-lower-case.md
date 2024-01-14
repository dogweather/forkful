---
title:                "Elixir: Перетворення рядка на малий регістр"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Чому

Перетворення рядка на нижній розділ (lower case) є важливою технікою в програмуванні ексира, оскільки дозволяє взаємодіяти з рядками в більш ефективний спосіб. Це особливо корисно, коли ви хочете порівняти рядки чи знайти певну підстроку в рядку.

## Як

```elixir
"hello world" |> String.downcase

# вивід: "hello world"
```

```elixir
"Programming with Elixir" |> String.downcase

# вивід: "programming with elixir"
```

Коли ми використовуємо оператор "|" (pipe), ексир автоматично передає результат попереднього виразу як перший аргумент в наступний вираз. Таким чином, ми можемо ланцюжити декілька методів разом для перетворення рядка на нижній розділ.

## Глибокий занурення

Окрім методу `String.downcase`, існує ще декілька способів перетворення рядка на нижній розділ, таких як `String.downcase!/1`, `String.to_lower/1` та `String.to_lower/2`. Відмінністю між ними є те, що перші два методи повертають новий рядок, тоді як останній змінює оригінальний рядок і повертає його. Крім того, метод `String.downcase!/1` може викинути помилку, якщо рядок містить невалідні символи.

## Дивіться також

- [Офіційна документація ексира про методи роботи з рядками](https://hexdocs.pm/elixir/String.html)
- [Відео українською про роботу з рядками в ексирі](https://www.youtube.com/watch?v=4hrAs-V3kI8)
- [Блог ексир програміста Андрія Боднаренка](https://andre.github.io/programming-in-elixir/)