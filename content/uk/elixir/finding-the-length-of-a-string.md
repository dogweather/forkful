---
title:                "Знаходження довжини рядка"
html_title:           "Elixir: Знаходження довжини рядка"
simple_title:         "Знаходження довжини рядка"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Чому

У пошуках довжини рядка багато практичних використань, таких як робота з текстовими даними або перевірка коректності введених даних.

## Як

Почати пошук довжини рядка в Elixir дуже просто. Використовуйте вбудовану функцію `String.length` та передайте їй рядок, довжину якого потрібно знайти, як аргумент. Наприклад:

```Elixir
string = "Привіт, світе!"
String.length(string) # виведе 13
```

Якщо ж ви хочете знайти довжину рядка, який містить не тільки українські літери, але й символи з різних мов, то використовуйте функцію `Codepoint.length`. Наприклад:

```Elixir
string = "привет, мир | Hello, world | こんにちは、世界"
Codepoint.length(string) # виведе 34
```

## Глибоке дослідження

Щоб краще розібратися, як працюють ці функції, давайте подивимося на прикладі більш детально. У Elixir, рядки є списками символів, тому функції `String.length` та `Codepoint.length` фактично підраховують кількість елементів у цьому списку. Наприклад:

```Elixir
string = "Hello"
String.length(string) # виведе 5
```

Список складається з символів `H`, `e`, `l`, `l`, `o`. Також варто зазначити, що Elixir використовує UTF-8 для кодування символів, тому функція `String.length` може коректно визначати довжину рядка з символами з різних мов.

## Дивіться також

- [Офіційна документація Elixir про рядки](https://hexdocs.pm/elixir/String.html)
- [Стаття про функцію Codepoint.length на ElixirForum](https://elixirforum.com/t/difference-between-string-length-codepoint-length/5458)