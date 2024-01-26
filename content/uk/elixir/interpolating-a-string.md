---
title:                "Інтерполяція рядків"
date:                  2024-01-20T17:50:34.130133-07:00
model:                 gpt-4-1106-preview
simple_title:         "Інтерполяція рядків"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Інтерполяція рядків – це вставлення значень змінних прямо у текстовий рядок. Програмісти використовують її для зручності форматування та збирання динамічного тексту.

## How to:

```elixir
name = "Світлана"
age = 28

# Інтерполяція за допомогою #{}.
greeting = "Привіт, мене звати #{name} і мені #{age} роки."

IO.puts greeting
```

Вивід:
```
Привіт, мене звати Світлана і мені 28 роки.
```

## Deep Dive

Інтерполяція рядків у Elixir з'явилась разом із самою мовою, оскільки це стандартний функціонал більшості сучасних мов програмування. Це нащадок механізмів, які були в Perl і Ruby.

Крім інтерполяції за допомогою `#{}`, можна використовувати функції форматування, як-от `String.replace/4` та модуль `StringIO` для комплексніших задач. При інтерполяції Elixir виконує код всередині `#{}` та замінює його на результат. Внутрішньо, це відмінний спосіб збільшити читабельність і зберегти іммутабельність даних.

## See Also

- [Elixir's String module](https://hexdocs.pm/elixir/String.html)
- [Elixir School on Strings](https://elixirschool.com/en/lessons/basics/strings/)
- [The Elixir programming language website](https://elixir-lang.org/)
