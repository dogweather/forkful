---
date: 2024-01-20 17:59:32.395435-07:00
description: "How to: Elixiriss\xE4 HTTP-pyynt\xF6jen tekeminen k\xE4y usein `HTTPoison`-kirjaston\
  \ kautta. T\xE4ss\xE4 helppo esimerkki GET-pyynn\xF6st\xE4."
lastmod: '2024-03-13T22:44:56.223651-06:00'
model: gpt-4-1106-preview
summary: "Elixiriss\xE4 HTTP-pyynt\xF6jen tekeminen k\xE4y usein `HTTPoison`-kirjaston\
  \ kautta."
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen"
weight: 44
---

## How to:
Elixirissä HTTP-pyyntöjen tekeminen käy usein `HTTPoison`-kirjaston kautta. Tässä helppo esimerkki GET-pyynnöstä:

```Elixir
# ENSIKSI: Jos ei jo asennettu, lisää `HTTPoison` riippuvuuteen mix.exs-tiedostoosi:
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end

# Sitten, pyynnön tekeminen:
defmodule HTTPExample do
  def fetch(url) do
    HTTPoison.get(url)
  end
end

# Otetaan vastaan vastaus ja tulostetaan se:
{:ok, response} = HTTPExample.fetch("http://httpbin.org/get")
IO.inspect(response.status_code) # Tulostaa 200
IO.puts(response.body)           # Tulostaa pyydetyn resurssin raakadatan
```

Sample output:

```
200
{
  "args": {}, 
  "headers": {
    "Host": "httpbin.org"
    ...
  }, 
  ...
}
```

## Deep Dive
Elixirin suosio nousi erityisesti rinnakkaistamisen ja vikasietoisuuden vuoksi, mikä sopii hyvin moderneihin verkkosovelluksiin. Historiallisesti katsottuna, HTTP-pyyntöjen lähettäminen tapahtui Elixirissä matalamman tason kirjastojen, kuten `:httpc` (Erlangin HTTP client), kautta. `HTTPoison` on korkean tason kirjasto, joka käyttää `hackney`-kirjastoa ja tarjoaa käyttäjäystävällisen syntaksin Elixir-kehittäjille.

Vaihtoehtoisia kirjastoja `HTTPoison`ille ovat esimerkiksi `Tesla`, joka on modulaarinen HTTP-asiakaskirjasto middleware-tuella, sekä `Finch`, joka on keskittynyt suorituskykyyn Elixirin OTP-prinsiippejä noudattaen.

Lähetettäessä HTTP-pyyntöä tärkeää on ymmärtää eri pyyntötyypit (GET, POST, PUT, DELETE jne.), statuskoodit ja oikean HTTP-headerin määrittäminen.

## See Also
- [HTTPoison GitHub](https://github.com/edgurgel/httpoison)
- [Tesla GitHub](https://github.com/teamon/tesla)
- [Finch GitHub](https://github.com/sneako/finch)
