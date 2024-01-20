---
title:                "Великі букви в рядку"
html_title:           "Elixir: Великі букви в рядку"
simple_title:         "Великі букви в рядку"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Що і Для чого?

Стилізація рядка до верхнього регістру - це процес перетворення всіх маленьких літер у рядку в великі. Програмісти роблять це для забезпечення коректності користувацького введення або для поліпшення читабельності виводу.

## Як це зробити:

```elixir
defmodule Capitalize do
  def upcase_string(str) when is_binary(str) do
    String.upcase(str)
  end
end

IO.puts Capitalize.upcase_string("hello")
```

Виходом цього коду буде `"HELLO"`.

## Поглиблений аналіз

1. Історичний контекст: Функція `String.upcase/1` була введена в Elixir від самого початку. Важливо відзначити, що ця функція враховує Unicode.
2. Альтернативи: Ви також можете використовувати `:string.to_upper/1` з Erlang, але вона не враховує Unicode.
3. Деталі реалізації: `String.upcase/1` працює, проходячи по кожному символу в рядку і змінюючи його в верхній регістр, якщо він є літерою в нижньому регістрі.

## Дивись також

1. Офіційну документацію Elixir про `String.upcase/1`: [https://hexdocs.pm/elixir/String.html#upcase/2](https://hexdocs.pm/elixir/String.html#upcase/2)
2. Пост у блозі про роботу з рядками в Elixir: [https://www.jungledisk.com/blog/2017/06/05/string-handling-in-elixir](https://www.jungledisk.com/blog/2017/06/05/string-handling-in-elixir)