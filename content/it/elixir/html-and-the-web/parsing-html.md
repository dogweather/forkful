---
date: 2024-01-20 15:31:09.248361-07:00
description: "How to: (Come fare:) La storia del parsing HTML \xE8 legata all'evoluzione\
  \ del web. All'inizio, l'HTML era semplice e si poteva gestire con espressioni\u2026"
lastmod: '2024-04-05T22:50:56.930503-06:00'
model: unknown
summary: "(Come fare:) La storia del parsing HTML \xE8 legata all'evoluzione del web."
title: Analisi dell'HTML
weight: 43
---

## How to: (Come fare:)
```elixir
# Installa Floki con: mix deps.get {:floki, "~> 0.30.0"}
defmodule HtmlParser do
  require Logger

  def fetch_and_parse(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{body: body}} ->
        body
        |> Floki.parse()
        |> Floki.find("a")
        |> Enum.map(&({"Link testo:", Floki.text(&1), "URL:", Floki.attribute("href", &1)}))
      {:error, %HTTPoison.Error{reason: reason}} ->
        Logger.error("Errore: #{reason}")
    end
  end
end

# Esempio di utilizzo:
parsed_links = HtmlParser.fetch_and_parse("https://elixir-lang.org")
IO.inspect(parsed_links)
```

Output campione:
```elixir
[
  {"Link testo:", "Learn more", "URL:", "https://elixir-lang.org/learning"},
  {"Link testo:", "Installation", "URL:", "https://elixir-lang.org/install.html"},
  ...
]
```

## Deep Dive (Approfondimento)
La storia del parsing HTML è legata all'evoluzione del web. All'inizio, l'HTML era semplice e si poteva gestire con espressioni regolari. Man mano che HTML si è evoluto, il parsing è diventato più complesso. Librerie come Floki in Elixir sfruttano il parsing basato su alberi, che è più adatto a HTML complesso. Floki si basa su mochiweb per gestire gli alberi HTML.

Alternative come `meeseeks` offrono anche parsing basato su query simili a jQuery. L'implementazione di Floki è progettata pensando alla concorrenza. Elixir, con il suo modello di attori (processi leggeri isolati), riesce a parallelizzare facilmente il parsing di grandi volumi di HTML, il che è un vantaggio significativo in termini di prestazioni e scalabilità.

## See Also (Vedi Anche)
- [Floki GitHub](https://github.com/philss/floki)
- [HTTPoison GitHub](https://github.com/edgurgel/httpoison)
- [Elixir Official Docs](https://elixir-lang.org/docs.html)
