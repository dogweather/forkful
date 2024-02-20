---
date: 2024-01-20 17:59:34.471348-07:00
description: "Wysy\u0142anie \u017C\u0105dania HTTP to spos\xF3b, by program m\xF3\
  g\u0142 komunikowa\u0107 si\u0119 z zewn\u0119trznymi serwerami \u2013 pobiera\u0107\
  \ dane, wysy\u0142a\u0107 formularze czy autoryzowa\u0107\u2026"
lastmod: 2024-02-19 22:04:54.218196
model: gpt-4-1106-preview
summary: "Wysy\u0142anie \u017C\u0105dania HTTP to spos\xF3b, by program m\xF3g\u0142\
  \ komunikowa\u0107 si\u0119 z zewn\u0119trznymi serwerami \u2013 pobiera\u0107 dane,\
  \ wysy\u0142a\u0107 formularze czy autoryzowa\u0107\u2026"
title: "Wysy\u0142anie \u017C\u0105dania HTTP"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Wysyłanie żądania HTTP to sposób, by program mógł komunikować się z zewnętrznymi serwerami – pobierać dane, wysyłać formularze czy autoryzować użytkowników. Programiści robią to, aby ich aplikacje internetowe mogły wymieniać informacje z innymi serwisami i przetwarzać je.

## Jak to zrobić:
Elixir używa różnych pakietów do wysyłania żądań HTTP, ale popularnym wyborem jest `HTTPoison`. Oto jak możesz wysłać proste żądanie GET:

```elixir
# Dodaj HTTPoison do swojego projektu dodając do `mix.exs`:
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Potem uruchom `mix deps.get` aby pobrać zależności.

# Przykład żądania GET:
defmodule ExampleClient do
  def get_example_page do
    HTTPoison.get("http://example.com")
  end
end

# Wywołanie i przykładowa odpowiedź:
{status_code, response_body} = ExampleClient.get_example_page()
IO.inspect({status_code, String.trim(response_body)})
```

Powinieneś zobaczyć odpowiedź serwera jako kod statusu wraz z treścią, np. `{200, "<html>...</html>"}`.

## Dogłębna analiza

HTTPoison bazuje na bibliotece `hackney` i jest powszechnie stosowany w społeczności Elixir ze względu na swoją prostotę i elastyczność. Istnieje od czasów Elixir 1.0, dając stabilne API dla wielu projektów. Alternatywy to `Tesla`, który oferuje middleware i większą konfigurowalność, oraz `finch`, skupiający się na wydajności przy użyciu HTTP/2.

Wysyłająć żądania HTTP, ważne jest też zrozumienie metody HTTP, której używasz. GET jest przeznaczony do pobierania danych, POST do ich wysyłania, PUT do aktualizacji, a DELETE do ich usuwania. Odpowiedź serwera zawiera standardowy kod statusu HTTP oraz, w większości przypadków, treść wiadomości, która może być w różnych formatach, takich jak HTML, JSON czy XML.

## Zobacz także

- [HTTPoison documentation](https://hexdocs.pm/httpoison/HTTPoison.html)
- [Elixir Forum](https://elixirforum.com/) - for community discussions and questions related to Elixir programming and HTTP requests.
