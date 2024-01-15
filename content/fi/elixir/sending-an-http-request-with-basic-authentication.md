---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
html_title:           "Elixir: HTTP-pyynnön lähettäminen perusautentikoinnilla"
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

Elixir on dynaaminen, toimintajärjestelmääriippumaton ja funktionaalinen ohjelmointikieli, joka on suunniteltu skaalautuvuuteen ja resilienssiin. Siksi Elixirin avulla voidaan luoda tehokkaita ja luotettavia HTTP-asiakasohjelmistoja, kuten tilintarkastuksia, varmenteita ja kirjautumisia käyttäen.

## Miten

```elixir
iex> client = HTTPBasicAuth.client("username", "password")
{:ok, %HTTPBasicAuth.Client{username: "username", password: "password"}}
```

Käytä ensin HTTPBasicAuth-kirjastoa luodaksesi asiakkaan ja anna sille käyttäjänimi ja salasana. Tämän jälkeen voit lähettää HTTP-pyynnön seuraavasti:

```elixir
iex> HTTPBasicAuth.get("https://example.com", client)
{:ok, %HTTPotion.Response{status_code: 200, body: "Success"}}
```

HTTP-asiakasohjelmiston lähettämä pyyntö sisältää sisäänrakennetun HTTPBasicAuth-salauksen, joka varmistaa, että vain oikeutetut käyttäjät voivat käyttää pyyntöä. Tämä tekee HTTP-pyyntöjen lähettämisestä turvallisen ja luotettavan.

## Deep Dive

HTTPBasicAuth-kirjaston avulla voit myös vaihtaa käyttäjänimeä tai salasanaa, mikä tekee Elixirista erittäin joustavan ja helppokäyttöisen kielen. Kirjaston avulla voit myös helposti mukauttaa muita autentikointimenetelmiä, kuten Digest, OAuth ja JWT. Seuraavassa on esimerkki siitä, miten voit vaihtaa salasanaa:

```elixir
iex> updated_client = HTTPBasicAuth.update_password(client, "new_password")
```

Tällä tavalla voit helposti päivittää käyttäjän salasanan ilman, että sinun tarvitsee luoda uusi asiakas jokaiselle päivitykselle.

## Katso myös

- [Virallinen Elixir-kotisivu](https://elixir-lang.org/)
- [HTTPBasicAuth-kirjaston dokumentaatio](https://hexdocs.pm/http_basic_auth/HTTPBasicAuth.html)
- [HTTP-pyyntöjen lähettäminen Elixirilla (englanniksi)](https://www.slideshare.net/keathley/painless-http-requests-with-elixir)