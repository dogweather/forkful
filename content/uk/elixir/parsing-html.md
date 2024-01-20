---
title:                "Розбір HTML"
html_title:           "Arduino: Розбір HTML"
simple_title:         "Розбір HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Що і чому?
Парсинг HTML - це процес витягування специфічної інформаціј з HTML документів, це важливо, бо дозволяє програмістам автоматизувати видобування даних або маніпулювати семантичним змістом веб-сайтів.

## Як це робити:
Ось невеликий приклад програми, яка парсить HTML за допомогою бібліотеки Floki в Elixir:

```Elixir
defmodule HtmlParser do
  require Logger
  def parse(url) do
    case :httpc.request(url) do
      {:ok, {_, _, body}} ->
        body
        |> Floki.find("title")
        |> Floki.raw_html
        |> Logger.info()

      {:error, reason} ->
        {:error, reason}
    end
  end
end
```
Після запуску це може вивести такий результат:

```Elixir
:title "[Elixir] Парсинг HTML з Floki - Проєкт кодування"
```

## Занурюємося глибше:
Парсинг HTML був важливим фактором ще з самого початку вебу. XML і JSON поступово стали більш популярними для міжсерверного обміну даними, але HTML все ще є нормою для структуризації і представлення контенту на веб-сторінках.

Альтернативами в Elixir для розбору HTML є, наприклад, бібліотеки: html_sax_parser і html5ever.

Що стосується Floki, він використовує під капотом mochiweb для утиліти html_scan, яка насправді розбиває HTML на частини. Після цього Floki бере ці частини, перетворює їх в свою власну внутрішню структуру даних і працює з нею.

## Див собі і тут:
1. [Floki on Hex](https://hex.pm/packages/floki)
3. [The Beauty of Elixir: Building a HTML parser using Elixir’s metaprogramming](https://itnext.io/building-a-html-parser-using-elixirs-metaprogramming-9ee0a3fd0ca1)