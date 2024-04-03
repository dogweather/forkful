---
date: 2024-01-20 17:50:34.130133-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:44:48.699512-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0406\u043D\u0442\u0435\u0440\u043F\u043E\u043B\u044F\u0446\u0456\u044F \u0440\
  \u044F\u0434\u043A\u0456\u0432"
weight: 8
---

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
