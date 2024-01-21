---
title:                "Виведення налагоджувальної інформації"
date:                  2024-01-20T17:52:17.008638-07:00
model:                 gpt-4-1106-preview
simple_title:         "Виведення налагоджувальної інформації"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке друк виводу для налагодження та навіщо це програмістам? Друк виводу для налагодження допомагає зрозуміти, що відбувається у вашій програмі, виводячи значення змінних та іншу інформацію в консоль. Це незамінний інструмент для виявлення помилок.

## How to:
```elixir
# Kernel.inspect/2 дозволяє бачити внутрішнє представлення даних.
list = [1, 2, 3]
IO.puts(Kernel.inspect(list))

# Якщо вам треба швидко вивести значення змінної:
value = 42
IO.puts("The value is: #{value}")
```
Sample output:
```
[1, 2, 3]
The value is: 42
```

## Deep Dive
В Elixir, Kernel.inspect/2 перетворює будь-яку структуру даних на рядок, зручний для читання, що дуже корисно для налагодження. Це альтернатива вбудованим інструментам налагодження, як :debugger у Erlang, який може бути надто складним для простих задач. Друк виводу дозволяє швидко інспектувати значення "на ходу".

## See Also
- Офіційна документація по `IO.inspect/2`: [https://hexdocs.pm/elixir/IO.html#inspect/2](https://hexdocs.pm/elixir/IO.html#inspect/2)
- Книга "Programming Elixir" by Dave Thomas: Searching for the "Debugging" section might give more conceptual depth on Elixir tools for printing and debugging.