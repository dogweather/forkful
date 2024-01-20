---
title:                "Генерація випадкових чисел"
html_title:           "Java: Генерація випадкових чисел"
simple_title:         "Генерація випадкових чисел"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Що і навіщо?

В розробці програмного забезпечення згенерувати випадкове число - це створити число, яке вибирається або сформовано всліпу або без якогось сенсовного порядку, що може бути передбачено. Ми це робимо для відтворюваності тестів, моделювання випадкових процесів, створення унікальних ідентифікаторів і так далі.

## Як це робити:

В Elixir можна згенерувати випадкові числа в діапазоні за допомогою методу `rand/1` і `trunc/1`. Ось як:

```Elixir
IO.inspect trunc(:rand.uniform() * 100)
```

Цей код повертає випадкове число в діапазоні від 0 до 99. 

## Пірнемо глибше

У минулому, Elixir користувався Erlang-овою функцією `:random.uniform/0` для генерації випадкових чисел, але тепер він перейшов на `:rand.uniform/0`, отримавши покращення у випадковості та швидкості. 

Альтернативи включають використання інших бібліотек Elixir, таких як `exs1024`, для більшого контролю над генерацією випадкових чисел.

"Під капотом" Elixir генерує псевдовипадкові числа, що означає, що вони генеруються з використанням детермінованого алгоритму, але його трудно передбачити.

## Дивіться також

1. [Elixir's Random module](https://hexdocs.pm/elixir/Kernel.html#rand/1)
2. [Random number generation in Erlang](http://erlang.org/doc/man/rand.html)
3. [Exs1024 library](https://hex.pm/packages/exs1024)
4. [Understanding random numbers in Elixir](https://blog.notnot.services/understanding-random-numbers-in-elixir/)