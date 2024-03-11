---
date: 2024-01-20 17:44:03.914534-07:00
description: "\xC5 laste ned en nettside betyr \xE5 hente HTML-koden fra en webserver\
  \ for \xE5 lagre eller behandle den lokalt. Programmerere gj\xF8r dette for \xE5\
  \ skrape data, teste\u2026"
lastmod: '2024-03-11T00:14:13.968250-06:00'
model: gpt-4-1106-preview
summary: "\xC5 laste ned en nettside betyr \xE5 hente HTML-koden fra en webserver\
  \ for \xE5 lagre eller behandle den lokalt. Programmerere gj\xF8r dette for \xE5\
  \ skrape data, teste\u2026"
title: Nedlasting av en nettside
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å laste ned en nettside betyr å hente HTML-koden fra en webserver for å lagre eller behandle den lokalt. Programmerere gjør dette for å skrape data, teste webtjenester eller automatisere oppgaver som er avhengige av innholdet på en nettside.

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
