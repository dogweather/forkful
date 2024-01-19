---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi & Miten?
HTTP-pyynnön lähettäminen tarkoittaa palvelimelle lähetettävää pyyntöä tiedon saamiseksi tai päivittämiseksi. Ohjelmoijat tekevät sen vuorovaikutuksessa palvelinten kanssa esimerkiksi verkkosivustoja tai mikropalveluja luodessaan.

## Näin se tapahtuu:
Elixirissä HTTP-pyyntöjen lähettäminen on suoraviivaista `HTTPoison`-kirjaston kanssa. Harkitse esimerkkiä:

```elixir
defmodule Kysely do
  def req(url) do
    HTTPoison.get!(url)
  end
end

IO.inspect(Kysely.req("http://example.com"))
```

Yllä oleva koodi lähettää GET-pyynnön `http://example.com` -osoitteeseen ja tulostaa vastauksen. Output saattaa näyttää tältä:

```elixir
%HTTPoison.Response{
  body: "<html>...",
  headers: [{"Content-Type", "text/html"}, ...],
  request_url: "http://example.com",
  status_code: 200
}
```

## Syvempi syöksy
HTTP-pyynnöt ovat olleet olennainen osa verkkosovellusten toimintaa niiden alkuperästä lähtien. Ne ovat keino, jolla clientit, kuten selain, kommunikoivat palvelimen kanssa.

Elixirissä on vaihtoehtoja HTTP-pyyntöjen lähettämiseen, mukaan lukien `ibrowse` ja `gun`. Kuitenkin `HTTPoison` on erityisen suosittu sen yksinkertaisen käyttöliittymän ja monipuolisten toimintojen, kuten streamingin ja hienosäätöisten pyyntöjen ansiosta.

HTTP-pyyntöjen lähettämisen ajatuksena elixirissä on, että HTTPoison.get!:n kutsu palauttaa `HTTPoison.Response` tai `HTTPoison.Error` -rakenteen. Tämä mahdollistaa virheiden käsittelyn riippuen siitä, onko pyyntö onnistunut.

## Katso myös 
1. HTTPoison käyttöönotto: https://hexdocs.pm/httpoison/readme.html
2. HTTP-pyyntöjen perusteet: https://developer.mozilla.org/fi/docs/Web/HTTP/Methods
3. Elixirin virallinen dokumentaatio: https://elixir-lang.org/learning.html

Huomaathan, että tämä on pintapuolinen katsaus HTTP-pyyntöjen lähettämiseen Elixirissä. Jatkuvan kehityksen ja oppimisen tueksi suositellaan aiheesta lisätutkimusta ja harjoittelua.