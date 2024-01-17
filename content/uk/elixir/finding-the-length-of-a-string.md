---
title:                "Знаходження довжини рядка."
html_title:           "Elixir: Знаходження довжини рядка."
simple_title:         "Знаходження довжини рядка."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Що & Чому?

Знаходження довжини рядка - це процес обчислення кількості символів у рядку. Це важлива функція для багатьох програмістів, оскільки дозволяє визначити розмір і обробку текстових даних.

## Як: 

```Elixir
IO.puts(String.length("Привіт, світе!"))
```
Вивід: 14

```Elixir
IO.puts(String.length(""))
```
Вивід: 0

```Elixir
IO.puts(String.length("    "))
```
Вивід: 4

## Глибоке погруження:

Довжину рядка можна знайти за допомогою вбудованої функції String.length(). Це було поперше впроваджено у мові програмування Elixir в 2011 році. Існують також альтернативи для знаходження довжини рядка, такі як рекурсивна функція або використання бібліотеки Enum.

## Дивіться також:

- [Документація Elixir для String.length()](https://hexdocs.pm/elixir/String.html#length/1)
- [Стаття на тему довжини рядка за допомогою рекурсії](https://elixirforum.com/t/getting-the-length-of-a-string/325)
- [Стаття про бібліотеку для обробки текстових даних Enum](https://hackr.io/blog/javascript-string-length)