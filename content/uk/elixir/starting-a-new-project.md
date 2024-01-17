---
title:                "Початок нового проекту"
html_title:           "Elixir: Початок нового проекту"
simple_title:         "Початок нового проекту"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Що & Чому?
Створення нового проекту - це створення нової програми або додатку в мові програмування Elixir. Програмісти роблять це, щоб реалізувати нові ідеї, вирішити проблеми або покращити наявний код.

## Як це зробити:
```Elixir
defmodule HelloWorld do
  def hello do
    IO.puts("Привіт світ!")
  end
end

HelloWorld.hello()
```

Використовуючи ключове слово `defmodule`, ми створюємо модуль з ім'ям `HelloWorld`. В середині цього модуля ми використовуємо ключове слово `def` для створення функції `hello`, яка просто виводить рядок `Привіт світ!`. Нарешті, ми викликаємо цю функцію за допомогою `()`.

## Глибокий занурення:
Elixir був створений в 2011 році і базується на байт-коді Erlang. Це функціональна мова програмування, яка підтримує паралельні та конкурентні обчислення. Для створення нового проекту, ви також можете використовувати інші мови програмування, такі як Ruby або Java.

## Дивись також:
- [Офіційна документація Elixir](https://hexdocs.pm/elixir)
- [Elixir School](https://elixirschool.com/uk)
- [Проектний шаблон Elixir](https://github.com/elixir-lang/elixir/blob/HEAD/lib/mix/lib/mix/templates/gen_app/README.md)