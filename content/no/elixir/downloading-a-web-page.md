---
title:                "Laste ned en nettside"
html_title:           "Elixir: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor?

Nedlasting av en nettside er prosessen med å hente data fra en nettside til din lokale maskin. Programmører gjør dette for å analysere, monitorere, lagre eller manipulere nettinnhold.

## Hvordan gjøre:

Først, vi trenger å legge til `httpoison` og `floki` i vår `mix.exs` fil:

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"},
    {:floki, "~> 0.30"}
  ]
end
```

Kjør deretter `mix deps.get` for å hente avhengighetene. Nå er vi klare til å laste ned en nettside:

```elixir
defmodule WebpageDownloader do
  require HTTPoison
  
  def download(url) do
    case HTTPoison.get(url) do
      {:ok, response} -> 
        response.body
      {:error, reason} -> 
        IO.inspect(reason)
    end
  end
end
```

Kall på denne funksjonen som så:

```elixir
WebpageDownloader.download("https://www.example.com")
```

Du vil motta HTML-koden til nettsiden som en streng.

## Dypdykk

Historisk sett ble nedlasting av nettsider gjort ved å programmere HTTP forespørsler manuelt. Selv om Elixir gir deg muligheten til å gjøre dette, er det anbefalt å bruke bibliotek som HTTPoison for enkelthet og tilbud om bedre feilhåndtering.

Alternativer til nedlasting av webinnhold inkluderer webscraping og web crawling, men disse teknikkene handler ofte om datahenting på et større skala. 

Når det gjelder implementeringsdetaljer, er koden ovenfor ganske enkel. Den bruker HTTPoison-biblioteket for å sende en HTTP GET forespørsel til den oppgitte URL-en og returnerer svaret som en streng. Hvis det oppstår en feil, returneres feilmeldingen som en streng i stedet.

## Se Også:

  - [HTTPoison Dokumentasjon](https://hexdocs.pm/httpoison/HTTPoison.html)
  
  - [Floki Dokumentasjon](https://hexdocs.pm/floki/readme.html)

  - [Elixir School: Tutorials on Elixir](https://elixirschool.com/en/)

  - [Learn You Some Erlang For Great Good (A background on Erlang which Elixir is built upon)](https://learnyousomeerlang.com/)