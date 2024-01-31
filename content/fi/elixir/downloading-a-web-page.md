---
title:                "Verkkosivun lataaminen"
date:                  2024-01-20T17:43:52.714286-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Web-sivun lataaminen tarkoittaa webbisivun sisällön hakemista internetistä. Ohjelmoijat tekevät tämän datan keräämiseksi, prosessointia varten ja automaatioita luodakseen.

## How to: (Miten tehdään:)
Elixirin HTTP-kirjastot, kuten HTTPoison, helpottavat web-sivujen lataamista. Tässä yksinkertainen esimerkki:

```elixir
# Lisää HTTPoison riippuvuus mix.exs-tiedostoon
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Sitten haemme web-sivun sisällön
def download_page(url) do
  case HTTPoison.get(url) do
    {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
      body
    {:error, %HTTPoison.Error{reason: reason}} ->
      "Error: #{reason}"
  end
end

# Käyttö
IO.puts download_page("https://example.com")
```

Tämä tulostaisi https://example.com web-sivun HTML-koodin.

## Deep Dive (Syvä sukellus):
Historiallisesti web-sivujen lataaminen on toteutettu monilla eri tavoilla, esimerkiksi CURL-komennolla tai eri ohjelmointikieleille tehdyillä kirjastoilla. Elixirissä HTTPoison on suosittu vaihtoehto sen yksinkertaisuuden ja tehokkuuden ansiosta. Muut vaihtoehdot voisivat olla Tesla tai HTTPotion. Web-sivun lataamisen yksityiskohtiin kuuluu HTTP-pyyntöjen hallinta ja vastauksena saadun datan käsittely.

## See Also (Katso myös):
- HTTPoison GitHub-repositorio: https://github.com/edgurgel/httpoison
- Elixirin virallinen dokumentaatio HTTP-asiakkaista: https://hexdocs.pm/elixir/1.13/HTTPClient.html
- Tietoa CURLista ja esimerkkejä sen käytöstä: https://curl.se/docs/
