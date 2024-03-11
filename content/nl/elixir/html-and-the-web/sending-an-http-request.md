---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:27.086770-07:00
description: "Een HTTP-verzoek verzenden is hoe jouw programma data van het web vraagt,\
  \ een beetje zoals je een bibliothecaris om een boek zou vragen. Programmeurs doen\u2026"
lastmod: '2024-03-11T00:14:24.271983-06:00'
model: gpt-4-0125-preview
summary: "Een HTTP-verzoek verzenden is hoe jouw programma data van het web vraagt,\
  \ een beetje zoals je een bibliothecaris om een boek zou vragen. Programmeurs doen\u2026"
title: Een HTTP-verzoek verzenden
---

{{< edit_this_page >}}

## Wat & Waarom?
Een HTTP-verzoek verzenden is hoe jouw programma data van het web vraagt, een beetje zoals je een bibliothecaris om een boek zou vragen. Programmeurs doen dit om externe data op te halen, te verzenden of te manipuleren, van het krijgen van het weer tot het plaatsen van tweets.

## Hoe:
Gebruik de `HTTPoison` bibliotheek van Elixir. Het is netjes, eenvoudig en klaart de klus.

1. Voeg HTTPoison toe aan je `mix.exs` van je project:

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

2. Voer `mix deps.get` uit in je terminal om de afhankelijkheid op te halen.

3. Nu ben je klaar om een GET-verzoek te verzenden:

```elixir
case HTTPoison.get("https://jsonplaceholder.typicode.com/posts/1") do
  {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
    IO.inspect(body) # je hebt je data!
  {:error, %HTTPoison.Error{reason: reason}} ->
    IO.inspect(reason) # fout afhandelen
end
```

Voorbeeldoutput: een JSON-string van postgegevens van de placeholder-API.

## Diepgaand
Historisch gezien zou je `:httpc` gebruiken dat wordt geleverd met Erlang/OTP of Elixir's `HTTPotion`. HTTPoison is nu populairder, met een schonere syntax en gebouwd op Hackney, een robuuste HTTP-client voor Erlang.

Alternatieven voor HTTPoison zijn onder andere Tesla – een flexibele HTTP-client met ondersteuning voor middleware, en Mint – een glanzende, laagniveau HTTP-client.

Wat implementatie betreft, handelen deze bibliotheken zaken af als connection pooling, SSL en keep-alive, lastige zaken die essentieel zijn voor efficiënte HTTP-verzoeken. Ze gedragen zich als vriendelijke bibliothecarissen die zich bezighouden met het lastige werk, zodat je niet zelf door de stapels hoeft te kruipen.

## Zie Ook
- [HTTPoison GitHub](https://github.com/edgurgel/httpoison) – voor alle details en updates.
- [HexDocs voor HTTPoison](https://hexdocs.pm/httpoison) – de plek voor uitgebreide documentatie.
- [Elixir Forum](https://elixirforum.com) – om te chatten met de gemeenschap.
