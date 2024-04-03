---
date: 2024-01-20 17:44:00.255806-07:00
description: "Jak to zrobi\u0107: Poni\u017Cej znajdziesz kod w j\u0119zyku Elixir,\
  \ kt\xF3ry pobiera zawarto\u015B\u0107 strony internetowej."
lastmod: '2024-03-13T22:44:35.040866-06:00'
model: gpt-4-1106-preview
summary: "Poni\u017Cej znajdziesz kod w j\u0119zyku Elixir, kt\xF3ry pobiera zawarto\u015B\
  \u0107 strony internetowej."
title: Pobieranie strony internetowej
weight: 42
---

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
