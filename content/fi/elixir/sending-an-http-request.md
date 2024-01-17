---
title:                "Lähettää http-pyyntö"
html_title:           "Elixir: Lähettää http-pyyntö"
simple_title:         "Lähettää http-pyyntö"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Lähettäminen HTTP-pyyntö on tapa kommunikoida Web-palvelimen kanssa Elixir-ohjelman kautta. Tällä tavalla voit pyytää tietoja palvelimelta tai lähettää sinne tietoja. Ohjelmoijat käyttävät tätä toimintoa luodakseen dynaamisia verkkosovelluksia ja integroida niitä eri palveluihin.

## Miten:

Koodiesimerkki osoittaa, kuinka voit lähettää HTTP-pyynnön Elixirillä käyttämällä `HTTPoison`-pakettia:

```Elixir
response = HTTPoison.get("https://example.com/api/data")
```

Tämä koodi lähettää GET-pyynnön osoitteeseen "https://example.com/api/data". Palvelimen vastaus tallennetaan `response`-muuttujaan ja sitä voidaan käsitellä haluamallasi tavalla.

## Syvemmälle:

Lähettäminen HTTP-pyyntö ei ole mitään uutta, sillä se on ollut osa Web-protokollia jo vuosia. Ennen Elixirillä tehtyjen pyyntöjen lähettämiseen käytettiin yleensä `HTTPClient`-pakettia. Nykyään `HTTPoison` on kuitenkin suositumpi vaihtoehto sen yksinkertaisemman käyttöliittymän ja paremman suorituskyvyn vuoksi.

## Katso myös:

- [`HTTPoison`-dokumentaatio](https://hexdocs.pm/httpoison/HTTPoison.html)
- [`HTTPClient`-dokumentaatio](http://httpclient.exirel.com/)
- [Elixirin virallinen verkkosivusto](https://elixir-lang.org/)