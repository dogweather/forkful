---
title:                "Een webpagina downloaden"
aliases: - /nl/elixir/downloading-a-web-page.md
date:                  2024-01-28T21:58:55.981178-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een webpagina downloaden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/elixir/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Een webpagina downloaden betekent het ophalen van de inhoud ervan via het internet—essentieel, wat je browser doet. Programmeurs doen dit om gegevensextractie, tests of interactie met webservices zonder GUI te automatiseren.

## Hoe:
Elixir, met zijn krachtige HTTP-clientbibliotheken, maakt deze taak eenvoudig. Hier is hoe met `HTTPoison`:

```elixir
# Voeg eerst HTTPoison toe aan je mix.exs afhankelijkheden:
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Voer mix deps.get uit om de nieuwe afhankelijkheid op te halen

# Laten we nu een webpagina downloaden:
defmodule PageDownloader do
  def download(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:error, "Ontvangen statuscode: #{status_code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end
end

# Voorbeeld gebruik:
{:ok, inhoud} = PageDownloader.download("http://example.com")
```

Zorg ervoor dat je de potentiële fouten afhandelt om crashes te voorkomen!

## Diepgaand
Elixir's benadering van webinteracties wordt aangedreven door de robuuste netwerkmogelijkheden van Erlang. `HTTPoison` is een populaire bibliotheek gebouwd op `hackney`, maar het is niet de enige speler. Er is ook `Tesla`, dat een meer modulaire aanpak biedt met ondersteuning voor middleware.

Historisch gezien was het downloaden van webinhoud handmatiger, met het opstellen van HTTP-verzoeken via sockets. Elixir-bibliotheken abstraheren deze details, zodat je je kunt concentreren op je applicatielogica.

Bij het downloaden van webpagina's heb je te maken met asynchrone operaties en verschillende HTTP-protocollen, die Elixir sierlijk afhandelt vanwege zijn concurrency-model en fouttolerante ontwerp. Bovendien is het kritisch om tekst- en binaire gegevens correct te behandelen—zorg ervoor dat je codering en de mogelijkheid van binaire gegevens in webinhoud overweegt.

## Zie Ook
- [`HTTPoison` documentatie](https://hexdocs.pm/httpoison)
- [`Tesla` bibliotheek op Hex](https://hex.pm/packages/tesla)
- [Elixir School's gids over OTP-concurrentie](https://elixirschool.com/en/lessons/advanced/otp-concurrency/)
- [Erlang's `hackney` bibliotheek](https://github.com/benoitc/hackney)
