---
title:                "Капіталізація рядка"
html_title:           "Elixir: Капіталізація рядка"
simple_title:         "Капіталізація рядка"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Чому

Наш мова завжди використовуємо різні формати для найкращого представлення інформації, і іноді нам потрібно змінити регістри слів в рядку. Наприклад, якщо нам потрібно вивести назву товару на екрані з великої літери або створити унікальне ідентифікатор, який має бути написаний лише з малих літер. Це може здатися дрібною задачею, але з Elixir це можна зробити дуже просто.

## Які крапки в спекаті

Почнемо з стандартного методу - використання модуля `String` з функцією `capitalize`. Нижче приведений приклад коду, який допоможе вам зрозуміти принцип роботи цієї функції:

```elixir
iex> String.capitalize("elixir")
"Elixir"
```

Як бачимо, функція `capitalize` просто виконує перше слово в рядку з великої літери, а всі інші - з малих.

Але що якщо нам потрібно змінити регістр не тільки першої літери, а всіх слів в рядку? Тут нам на допомогу приходить функція `titlecase`:

```elixir
iex> String.titlecase("elixir programming language")
"Elixir Programming Language"
```

Як бачимо, функція `titlecase` змінює регістр всіх слів в рядку, а також правильно обробляє розрізняння слів з апострофами.

## Глибше копання

Якщо ви більш досвідчений Elixir розробник, вам може бути цікаво дізнатися, що насправді відбувається за лаштунками при використанні `String.capitalize` і `String.titlecase` функцій.

Кожна з цих функцій базується на регулярних виразах, які перевіряють рядок на наявність слів та обробляють їх відповідним чином. Можете переглянути вихідний код цих функцій на офіційному сайті Elixir: [String.capitalize](https://hexdocs.pm/elixir/String.html#capitalize/1) та [String.titlecase](https://hexdocs.pm/elixir/String.html#titlecase/1).

## Див. також

- [String module documentation](https://hexdocs.pm/elixir/String.html)
- [Regular expressions in Elixir](https://elixirschool.com/lessons/basics/pattern-matching/)
- [Capitalization (збалансованосереднювання) у програмуванні за допомогою Elixir](https://hackernoon.com/capitalization-in-programming-with-elixir-781bbb40f83a)