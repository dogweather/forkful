---
title:                "Читання аргументів командного рядка"
date:                  2024-01-20T17:56:13.427266-07:00
model:                 gpt-4-1106-preview
simple_title:         "Читання аргументів командного рядка"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Що це таке і навіщо?

Читання аргументів командного рядка – це метод взаємодії з програмою, що дозволяє користувачу передавати параметри під час її запуску. Програмісти використовують це для забезпечення гнучкості і настроювання поведінки програми без зміни коду.

## Як це зробити:

```elixir
# Створіть файл example.exs і додайте:
defmodule CLI do
  def main(args) do
    args |> Enum.each(&IO.puts/1)
  end
end

# Викликайте з командного рядка так:
# elixir example.exs Hello "How are you?"
CLI.main(System.argv())

# Вихід буде:
# Hello
# How are you?
```

## У подробицях:

Elixir використовує `System.argv()` для отримання аргументів командного рядка, подібно до інших мов, як-от Ruby чи Python. В історичному контексті, техніка походить ще з часів ранніх операційних систем, де командний рядок був основним способом взаємодії з комп'ютером. Хоча існують альтернативи, такі як подача даних через середовище чи файли конфігурації, аргументи командного рядка залишаються популярними через свою простоту та безпосередність. В Elixir ця функціональність проста, але важливо пам’ятати, що аргументи завжди являють собою рядки, отже, їх може знадобитися перетворити в інший тип, якщо потрібно.

## Дивись також:

- [Elixir документацію про System.argv()](https://hexdocs.pm/elixir/System.html#argv/0)
- [StackOverflow - як краще читати аргументи командного рядка в Elixir](https://stackoverflow.com/questions/tagged/elixir+command-line-arguments)
