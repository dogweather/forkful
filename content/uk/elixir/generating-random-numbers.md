---
title:                "Генерація випадкових чисел"
date:                  2024-01-20T17:49:03.043458-07:00
model:                 gpt-4-1106-preview
simple_title:         "Генерація випадкових чисел"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
Що таке генерація випадкових чисел? Просто кажучи, це процес створення чисел, які непередбачувані. Навіщо це програмістам? Від тестування до ігор та криптографії, випадкові числа — важливий компонент багатьох алгоритмів.

## How to:
```elixir
# Використання модуля :rand для генерації випадкового числа
random_number = :rand.uniform()
IO.puts("Випадкове число між 1 та 1: #{random_number}")

# Якщо вам треба випадкове число в певному діапазоні
range_random_number = :rand.uniform(100)
IO.puts("Випадкове число між 1 та 100: #{range_random_number}")
```

Приклад виводу:
```
Випадкове число між 1 та 1: 0.4435846175942928
Випадкове число між 1 та 100: 42
```

## Deep Dive
Генерація випадкових чисел має довгу історію в комп'ютерній науці, а їх якість та надійність варіюються. В Elixir ми використовуємо модуль `:rand`, який взагалі є прив'язкою до генератора випадкових чисел у Erlang. Цей модуль пропонує добрі характеристики та розподіл, але не призначений для криптографічного використання. Для крипто застосунків краще використати `:crypto.strong_rand_bytes/1`, який генерує більш безпечні випадкові числа.

## See Also
- [Erlang :rand module documentation](http://erlang.org/doc/man/rand.html)
- [Erlang :crypto module documentation](http://erlang.org/doc/man/crypto.html)
- [HexDocs Elixir Kernel module docs](https://hexdocs.pm/elixir/Kernel.html)