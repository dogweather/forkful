---
title:                "Elixir: Analizowanie html."
simple_title:         "Analizowanie html."
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli jesteś programistą Elixira, na pewno wiesz, że jedną z najważniejszych rzeczy jest analizowanie danych. A co jeśli część tych danych jest ukryta w HTML? Na szczęście, dzięki Elixirowi, możemy łatwo parsować HTML i wyciągać potrzebne nam informacje. Dlaczego więc warto poznać podstawy parsowania HTML? Dowiesz się tego w tym artykule.

## Jak to zrobić

Aby móc analizować HTML w Elixirze, potrzebna jest nam biblioteka Floki. Możesz ją zainstalować, wykonując poniższą komendę w terminalu:

```Elixir
mix deps.get floki
```

Następnie, musimy pobrać dane z witryny internetowej, którą chcemy przeanalizować. W tym przykładzie, pobierzemy stronę główną Google i wyświetlimy tytuł strony:

```Elixir
url = "https://www.google.com/"
html = HTTPoison.get!(url).body
Floki.find(html, "title") |> Floki.text
```

Powyższy kod pobiera dane z witryny i przekazuje je do funkcji "find" z biblioteki Floki. W argumencie funkcji podajemy nazwę tagu, który chcemy znaleźć. Następnie, używamy funkcji "text" aby pobrać tekst znajdujący się wewnątrz tagu. W tym przypadku jest to tytuł strony Google.

Możemy również wyświetlić linki z danej strony przy użyciu funkcji "to_string" oraz "attrs":

```Elixir
Floki.find(html, "a") |> Floki.enum_map(fn(el) -> Floki.to_string(el |> Floki.attrs).href end)
```
Powyższy kod zwróci listę wszystkich linków z głównej strony Google.

## Głębsza analiza

Podstawowe przykłady pokazały nam jak wykorzystać bibliotekę Floki do prostego parsowania HTML. Jednak, ta biblioteka oferuje wiele innych funkcji, które mogą uprościć naszą pracę. Możemy na przykład użyć funkcji "first" aby znaleźć pierwszy element pasujący do danego warunku:

```Elixir
Floki.find(html, "div", [class: "result"]) |> Floki.first |> Floki.children
```

Powyższy kod zwróci dzieci pierwszego elementu div z klasą "result", czyli wszystkie elementy znajdujące się wewnątrz tego diva.

Inną przydatną funkcją jest "match?" która pozwala nam sprawdzić, czy dany element pasuje do danego wzorca:

```Elixir
Floki.find(html, "input", [id: "username"]) |> Floki.match?({:attrs, [{"type", "text"}]})
```

Ten przykład zwróci true jeśli element input z id "username" ma atrybut "type" ustawiony na "text".

Podsumowując, biblioteka Floki daje nam wiele możliwości analizowania i wykorzystywania danych z HTML. Wystarczy tylko poznać jej funkcje i wykorzystać je w odpowiedni sposób.

## Zobacz też

- [Dokumentacja biblioteki Floki](https://hexdocs.pm/floki/readme.html)
- [Przykładowe aplikacje wykorzystujące parsing HTML w Elixirze](https://github.com/topics/elixir-html-parsing)