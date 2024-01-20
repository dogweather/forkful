---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Przetwarzanie HTML (parsing) to proces analizy kodu strony internetowej, aby zrozumieć jej strukturę i zawartość. Programiści robią to, aby skutecznie pobrać, manipulować lub interpretować dane z webowych dokumentów.

## Jak to zrobić:

Elixir z pomocą biblioteki `floki` umożliwia łatwe parsowanie HTML. Instalujemy ją poprzez dodanie do naszego mix.exs:

```Elixir
defp deps do
  [
    {:floki, "~> 0.26"}
  ]
end
```

Następnie, możemy zacząć parsować HTML:

```Elixir
html = "<div><p>Hej, jestem Elixir!</p></div>"
{"div", [], ["<p>Hej, jestem Elixir!</p>"]} = Floki.parse_document(html) |> hd()
```

Powyższy kod zwróci krotkę z elementem, atrybutami i zawartością.

## Głębsze zrozumienie

Parsowanie HTML ma swoje korzenie w przeszłości, zaczęło się na początku rozwoju internetu. Można to zrobić na różne sposoby - przez wyrażenia regularne, SAX, DOM, i wiele innych metod.

Alternatywą dla Elixir jest wykorzystanie innych języków takich jak Python, Ruby czy JavaScript, które oferują różne biblioteki do parsowania HTML.

Szczegółowym aspektem implementacji parsowania HTML w Elixir jest sposób, w jaki używamy funkcji z Floki, zazwyczaj zwracających wynik jako monadę, co jest typowe dla funkcjonalnych języków programowania.

## Zobacz również

1. Dokumentacja Floki: https://hexdocs.pm/floki/readme.html
2. Wprowadzenie do Elixir: http://elixir-lang.github.com/
3. Jak parsować HTML w Pythonie: https://www.crummy.com/software/BeautifulSoup/bs4/doc/