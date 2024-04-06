---
date: 2024-01-20 17:38:12.057739-07:00
description: "How to: (\u042F\u043A \u0437\u0440\u043E\u0431\u0438\u0442\u0438:) ."
lastmod: '2024-04-05T21:53:48.947968-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u041F\u0435\u0440\u0435\u0442\u0432\u043E\u0440\u0435\u043D\u043D\u044F \u0440\
  \u044F\u0434\u043A\u0430 \u0443 \u043D\u0438\u0436\u043D\u0456\u0439 \u0440\u0435\
  \u0433\u0456\u0441\u0442\u0440"
weight: 4
---

## How to: (Як зробити:)
```elixir
# Використовуйте функцию String.downcase/1:
original = "Привіт, Світ!"
lower_case = String.downcase(original)

IO.puts(lower_case)  # output: привіт, світ!
```

```elixir
# Також працює з текстом що містить латиницю та інші символи:
mixed_language = "Elixir & Еліксир!"
lower_mixed = String.downcase(mixed_language)

IO.puts(lower_mixed)  # output: elixir & еліксир!
```

## Deep Dive (Поглиблений Розбір)
Історично, зменшення величини букв використовувалось для порівняння та сортування тексту в системах, які мали відмінності в регістрах, що важливо у багатомовніх середовищах, як у Elixir. Elixir використовує Unicode, тому String.downcase/1 може працювати з широким спектром мов.

Є й інші способи зміни регістру у Elixir, як наприклад `:binary` модуль, але `String.downcase/1` є більш універсальним і підтримує Unicode. 

Ця функція втілює нюанси врахування мовних правил зниження регистру. Наприклад, у турецькій літера 'I' при зменшенні перетворюється на 'ı', що відрізняється від більшості інших мов, де вона стає 'i'.

## See Also (Дивіться Також)
- [Elixir Documentation for String](https://hexdocs.pm/elixir/String.html)
- [Unicode Case Mapping](https://unicode.org/reports/tr21/)
- [Elixir Forum](https://elixirforum.com/) – знайдіть допомогу та обговорення на теми, пов’язані з Elixir.
