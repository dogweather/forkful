---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:59:40.703505-07:00
description: "\u041A\u0430\u043A \u044D\u0442\u043E \u0441\u0434\u0435\u043B\u0430\
  \u0442\u044C: \u0412 Elixir \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0430\
  \u043D\u0430\u043B\u0438\u0437\u0438\u0440\u043E\u0432\u0430\u0442\u044C HTML \u0441\
  \ \u043F\u043E\u043C\u043E\u0449\u044C\u044E \u0431\u0438\u0431\u043B\u0438\u043E\
  \u0442\u0435\u043A\u0438 Floki. \u0412\u043E\u0442 \u043F\u0440\u0438\u043C\u0435\
  \u0440 \u043A\u043E\u0434\u0430."
lastmod: '2024-03-13T22:44:44.423406-06:00'
model: gpt-4-0125-preview
summary: "\u0412 Elixir \u0432\u044B \u043C\u043E\u0436\u0435\u0442\u0435 \u0430\u043D\
  \u0430\u043B\u0438\u0437\u0438\u0440\u043E\u0432\u0430\u0442\u044C HTML \u0441 \u043F\
  \u043E\u043C\u043E\u0449\u044C\u044E \u0431\u0438\u0431\u043B\u0438\u043E\u0442\u0435\
  \u043A\u0438 Floki."
title: "\u0420\u0430\u0437\u0431\u043E\u0440 HTML"
weight: 43
---

## Как это сделать:
В Elixir вы можете анализировать HTML с помощью библиотеки Floki. Вот пример кода:

```elixir
# Сначала добавьте Floki в зависимости mix.exs
{:floki, "~> 0.30.0"}

# Затем, в вашем коде

defmodule HTMLParser do
  alias Floki

  def parse_html(html) do
    {:ok, document} = Floki.parse(html)
    titles = Floki.find(document, "h1")
    IO.inspect(titles, label: "Заголовки")
  end
end

# Использование
html_content = "<html><body><h1>Привет, Elixir!</h1></body></html>"
HTMLParser.parse_html(html_content)

# Пример вывода
Заголовки: [{"h1", [], ["Привет, Elixir!"]}]
```

## Подробнее
Исторически анализ HTML в таких языках, как Python или JavaScript, был более распространен, но одновременные возможности Elixir и его масштабируемость делают его сильной альтернативой для современных веб-задач. Библиотека Floki использует быстрый C-парсер fast_html для скорости, давая вам лучшее из обоих миров: одновременность Elixir и производительность компилируемого языка.

По сравнению с другими инструментами, такими как BeautifulSoup в Python, Floki менее многословен и более функционален по стилю - это хорошо сочетается с идеалами Elixir. К тому же, у вас есть вся мощь экосистемы Erlang для обеспечения отказоустойчивости и распределения, если вы думаете масштабно.

## Смотрите также
- [Floki на Hex](https://hex.pm/packages/floki) - Официальная документация Floki.
- [HTML5ever](https://github.com/servo/html5ever) - Парсер HTML на Rust, который лежит в основе fast_html.
