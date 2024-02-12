---
title:                "Pobieranie strony internetowej"
aliases:
- /pl/elixir/downloading-a-web-page/
date:                  2024-01-20T17:44:00.255806-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pobieranie strony internetowej"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Pobieranie strony internetowej to proces ściągania jej treści, by móc przetwarzać lokalnie. Programiści robią to, aby analizować dane, monitorować zmiany lub pobierać informacje.

## Jak to zrobić:
Poniżej znajdziesz kod w języku Elixir, który pobiera zawartość strony internetowej:

```elixir
defmodule PageDownloader do
  require HTTPoison

  def download(url) when is_binary(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: code}} ->
        {:error, "Failed with status code: #{code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end
end
```

Przykładowe użycie:

```elixir
PageDownloader.download("http://example.com")
# Wynik: {:ok, "... zawartość strony ..."}
```

## Deep Dive
Pobieranie stron zaczęło się, gdy internet stał się dostępny dla mas; HTML i HTTP wyrosły jako standardy. Elixir, korzystając z potężnej HTTPoison biblioteki na bazie Erlanga, robi to dobrze i mało kłopotliwie. Alternatywy to, między innymi, `:httpc` z BEAM, `Tesla` czy `Floki` do analizy HTML po pobraniu. Wybór wynika z potrzeb projektu - HTTPoison jest świetny dla wielu zadań.

## Zobacz też:
- [HTTPoison GitHub](https://github.com/edgurgel/httpoison)
- [Tesla GitHub](https://github.com/teamon/tesla)
- [Dokumentacja Elixir](https://elixir-lang.org/docs.html)
