---
date: 2024-01-20 17:44:03.914534-07:00
description: 'Hvordan: Eksempelutskrift.'
lastmod: '2024-04-05T21:53:41.421918-06:00'
model: gpt-4-1106-preview
summary: ''
title: Nedlasting av en nettside
weight: 42
---

## Hvordan:
```elixir
defmodule PageDownloader do
  require HTTPoison

  def download(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:error, "Failed to download. Status code: #{status_code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, "Failed to download. Reason: #{reason}"}
    end
  end
end

# Bruk PageDownloader for å laste ned en nettside
{:ok, body} = PageDownloader.download("http://example.com")
IO.puts(body)
```

Eksempelutskrift:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</head>
...
</html>
```

## Dypdykk
Å laste ned nettsider er ikke noe nytt; det har vært en del av web-programmering siden internettets barndom. I Elixir brukes ofte HTTP-klientbiblioteker som HTTPoison eller Tesla for å utføre oppgaven. Disse bibliotekene forenkler prosessen ved å håndtere HTTP-forespørsler for deg. Alternativt kan man bruke lavnivå biblioteker som Erlangs :httpc. Når du laster ned en nettside, er det viktig å respektere robots.txt-filer og eventuelle API-grenser for å unngå å overbelaste serverne eller bryte med tjenestevilkår.

## Se Også
- HTTPoison GitHub-side: https://github.com/edgurgel/httpoison
- Tesla GitHub-side: https://github.com/teamon/tesla
- Elixir’s offisielle dokumentasjon: https://elixir-lang.org/docs.html
- Web scraping guide med Elixir: https://www.scrapingbee.com/blog/web-scraping-elixir/
