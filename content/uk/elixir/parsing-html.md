---
title:                "Парсинг HTML"
date:                  2024-01-20T15:31:24.218096-07:00
html_title:           "Arduino: Парсинг HTML"
simple_title:         "Парсинг HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Що це таке та навіщо?

Парсинг HTML - це процес, під час якого програма читає та інтерпретує HTML код, щоб знайти та витягти потрібні дані. Програмісти роблять це для автоматизації збору інформації з веб-сторінок.

## Як це робити:

У Elixir для парсингу HTML можна використати бібліотеку Floki. Вона працює на основі синтаксичного аналізатора html5ever, запозиченого з Rust. Ось приклад простого парсингу:

```elixir
# Підключаемо залежності
{:floki, "~> 0.30.0"},

# Код
defmodule HTMLParser do
  def parse_html(html) do
    { :ok, document } = Floki.parse_document(html)
    Floki.find(document, "h1")
    |> Enum.map(&Floki.text/1)
  end
end

# Використаня функції
html_content = "<html><body><h1>Hello, World!</h1></body></html>"
headlines = HTMLParser.parse_html(html_content)
IO.inspect(headlines) # Виведе ["Hello, World!"]
```

## Поглиблене занурення:

Парсинг HTML існує стільки, скільки і сам HTML. Раніше розробники використовували регулярні вирази, але це не найкраща практика через складність HTML. Тому з'явилися спеціалізовані бібліотеки як Floki в Elixir, Beautiful Soup у Python і Nokogiri у Ruby.

Альтернативою Floki може бути бібліотека Meeseeks, яка також базується на html5ever. Вибір залежить від переваг у швидкості, підтримці чи зручності API.

Деталі імплементації Floki включають використання CSS селекторів для пошуку елементів, що робить її зручною для людей, які вже знайомі з фронтенд розробкою.

## Дивіться також:

- Офіційний репозиторій бібліотеки Floki на GitHub: [https://github.com/philss/floki](https://github.com/philss/floki)
- Документація по Elixir: [https://elixir-lang.org/docs.html](https://elixir-lang.org/docs.html)
- Html5ever, Rust HTML парсинг бібліотека: [https://github.com/servo/html5ever](https://github.com/servo/html5ever)
- Meeseeks Elixir бібліотека: [https://hex.pm/packages/meeseeks](https://hex.pm/packages/meeseeks)