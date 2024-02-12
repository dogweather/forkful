---
title:                "Видалення символів за візерунком"
aliases:
- uk/elixir/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:28.059879-07:00
model:                 gpt-4-1106-preview
simple_title:         "Видалення символів за візерунком"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
## Що і Чому?

У програмуванні видалення символів за патерном є процесом фільтрації тексту. Ми це робимо, щоб очистити дані, видалити непотрібне або змінити формат вхідних даних.

## How to:
## Як це зробити:

```elixir
# Видалення всіх цифр з рядка:
string = "Годинник показує 12:45"
clean_string = String.replace(string, ~r/\d/, "")
IO.puts(clean_string)
# Вивід: "Годинник показує :"

# Видалення конкретних символів, наприклад, голосних:
vowels = ~r/[аеєиіїоуюяАЕЄИІЇОУЮЯ]/
phrase = "Привіт, світ!"
sanitized_phrase = String.replace(phrase, vowels, "")
IO.puts(sanitized_phrase)
# Вивід: "Првт, свт!"
```

## Deep Dive:
## Поглиблений Розгляд:

Перші спроби маніпуляції з рядками в програмуванні виникли ще із введенням перших комп'ютерів. Регулярні вирази, які сьогодні є стандартом для визначення патернів, запропонували в 1950-их. В Elixir, String модуль використовує регулярні вирази (RegExp) для здійснення складних операцій із текстом, включаючи видалення символів.

Альтернативно можна використовувати `String.graphemes/1` чи рекурсію для видалення символів без RegExp, але це може бути менш ефективно.

Нюанс Elixir в тому, що він працює поверх Erlang VM, тому операції з рядками оптимізовані для високої проїздності і конкурентності. При видаленні символів важливо розуміти роботу з binary і pattern matching, які є основою роботи із текстовими даними в Elixir.

## See Also:
## Див. також:

- [Elixir String Module](https://hexdocs.pm/elixir/String.html)
- [Regular Expressions in Elixir](https://hexdocs.pm/elixir/Regex.html)
- [Erlang Efficiency Guide](http://erlang.org/doc/efficiency_guide/binaryhandling.html)
