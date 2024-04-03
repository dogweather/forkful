---
date: 2024-01-20 17:43:45.385316-07:00
description: "Come fare: Elixir rende il download delle pagine web un gioco da ragazzi\
  \ con la libreria HTTPoison. Installala aggiungendo `{:httpoison, \"~> 1.8\"}` al\
  \ tuo\u2026"
lastmod: '2024-03-13T22:44:43.082548-06:00'
model: gpt-4-1106-preview
summary: Elixir rende il download delle pagine web un gioco da ragazzi con la libreria
  HTTPoison.
title: Scaricare una pagina web
weight: 42
---

## Come fare:
Elixir rende il download delle pagine web un gioco da ragazzi con la libreria HTTPoison. Installala aggiungendo `{:httpoison, "~> 1.8"}` al tuo `mix.exs` e esegui `mix deps.get`.

```elixir
defmodule PageDownloader do
  def fetch(url) when is_binary(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:error, "Failed with status code: #{status_code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "Failed with error: #{reason}"}
    end
  end
end

# Uso
{:ok, content} = PageDownloader.fetch("https://www.example.com")
IO.puts(content)
```

## Approfondimento:
Il download di pagine web in Elixir si appoggia al potente HTTPoison, che a sua volta si basa su Hackney, un cliente HTTP scritto in Erlang. In alternativa a HTTPoison, ci sono altre library come Tesla o Finch, ognuna con i suoi vantaggi e filosofie di progetto. La scelta tra queste dipende da fattori come la concorrenza, la configurazione e l'elaborazione delle richieste HTTP, oltre alle preferenze personali.

## Vedere anche:
- HTTPoison documentation: https://hexdocs.pm/httpoison/
- Tesla GitHub repository: https://github.com/teamon/tesla
- Finch GitHub repository: https://github.com/sneako/finch
- Elixir School per lezioni su Elixir: https://elixirschool.com/
