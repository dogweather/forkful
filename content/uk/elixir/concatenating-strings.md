---
title:                "З'єднання рядків."
html_title:           "Elixir: З'єднання рядків."
simple_title:         "З'єднання рядків."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## Що & Чому?
Конкатенація рядків - це процес об'єднання двох або більше рядків в один. Це часто використовується програмістами для створення більш складних рядків, які містять комбінацію тексту та змінних даних.

## Як:
```Elixir
string1 = "Привіт"
string2 = "всім!"
results = string1 <> " " <> string2

IO.puts results
```

Вихід: "Привіт всім!"

## Глибше:
Конкатенація рядків вже використовувалася у багатьох програмувальних мовах, таких як C та Java. У Elixir це зроблено за допомогою оператора `<>`. Іншим варіантом є використання функції `#{}` для підстановки значень змінних в середину рядка. Також існують спеціалізовані бібліотеки для роботи з рядками, які можуть бути корисними в більш складних випадках.

## Дивись також:
- [Офіційна документація з оператора <>](https://hexdocs.pm/elixir/Kernel.html#<>/2)
- [Стаття про роботу з рядками в Elixir](https://medium.com/@ejnguyen/tutorial-how-to-work-with-strings-in-elixir-1b2a93a21b82)