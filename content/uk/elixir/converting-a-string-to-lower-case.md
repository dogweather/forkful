---
title:                "Перетворення рядка у нижній регістр"
date:                  2024-01-20T17:38:12.057739-07:00
model:                 gpt-4-1106-preview
simple_title:         "Перетворення рядка у нижній регістр"

category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (Що та Чому?)
Зміна регістру рядка на нижній: процес перетворення усіх символів рядка у нижній регістр. Програмісти роблять це для уніфікації даних, наприклад, при порівнянні тексту без урахування регістру.

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
