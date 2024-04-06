---
date: 2024-01-20 17:52:17.008638-07:00
description: 'How to: Sample output.'
lastmod: '2024-04-05T21:53:48.969755-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0412\u0438\u0432\u0435\u0434\u0435\u043D\u043D\u044F \u043D\u0430\u043B\u0430\
  \u0433\u043E\u0434\u0436\u0443\u0432\u0430\u043B\u044C\u043D\u043E\u0457 \u0456\u043D\
  \u0444\u043E\u0440\u043C\u0430\u0446\u0456\u0457"
weight: 33
---

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
