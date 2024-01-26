---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:31:02.072462-07:00
html_title:           "Bash: Przetwarzanie HTML"
simple_title:         "Przetwarzanie HTML"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Parsing HTML to proces wydobywania danych ze struktur dokumentów HTML. Programiści robią to, by uzyskać informacje, manipulować treścią, czy automatyzować interakcje z witrynami internetowymi.

## How to: (Jak to zrobić?)
W Elixirze do parsowania HTML możemy użyć pakietu `Floki`. Oto jak łatwo zacząć:

```elixir
# Dodaj Floki do mix.exs jako zależność
defp deps do
  [{:floki, "~> 0.30.0"}]
end

# Uruchom `mix deps.get`, aby zainstalować Floki
```

Przykład użycia Floki do znalezienia tytułu strony:
```elixir
html = "<html><head><title>Super Strona</title></head><body></body></html>"
{:ok, document} = Floki.parse_document(html)
title = Floki.find(document, "title")
|> Floki.text()

IO.puts title  # Wydrukuje "Super Strona"
```

## Deep Dive (Dogłębna analiza)
Floki bazuje na „mochei” - silniku XPath napisanym w Elixirze. W przeszłości częściej używano `:erlsom` czy `:xmerl`, ale te biblioteki miały skomplikowane API i obsługiwały tylko XML. Floki zmieniło grę, oferując prostszy interfejs i skupienie na HTML. Ma również elastyczne selektory CSS pozwalające na łatwe znajdowanie elementów.

Inną opcją jest wykorzystanie `meeseeks`, inspirującego się „beautifulsoup” z Pythona, zapewniające jeszcze większą moc przetwarzania.

Gdybyśmy robili to natywnie, musielibyśmy opierać się na wyrażeniach regularnych i ręcznym parsowaniu - a to ciężka i błędu praca.

## See Also (Zobacz również)
- [Floki na Hex.pm](https://hex.pm/packages/floki)
- [Dokumentacja Floki](https://hexdocs.pm/floki)
- [Meeseeks na Hex.pm](https://hex.pm/packages/meeseeks)
- [Porównanie parserów HTML w Elixirze](https://elixirforum.com/t/comparing-html-parsers-nokogiri-mochei-floki-etc/11904)
