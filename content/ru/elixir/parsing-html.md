---
title:                "Разбор HTML"
date:                  2024-01-28T23:59:40.703505-07:00
model:                 gpt-4-0125-preview
simple_title:         "Разбор HTML"

category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ru/elixir/parsing-html.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Что и зачем?

Анализ HTML подразумевает просмотр кода HTML для программатического извлечения данных или деталей. Программисты делают это для задач, таких как веб-скрейпинг, извлечение данных или автоматизация взаимодействия с веб-сайтами.

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
