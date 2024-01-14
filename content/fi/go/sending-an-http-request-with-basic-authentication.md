---
title:                "Go: Http-pyynnön lähettäminen perusautentikoinnilla"
simple_title:         "Http-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Go:

# Miksi käyttää perusautentikointia HTTP-pyyntöihin?

HTTP-pyyntöjen lähettämistä käytetään usein kommunikointiin verkkosivustojen ja sovellusten välillä. Yksi tapa varmistaa, että vain tiettyjä käyttäjiä tai sovelluksia voi lähettää pyyntöjä on käyttää perusautentikointia. Tämä auttaa suojamaan rajapintoja ja estämään luvattomia käyttäjiä pääsemästä tärkeisiin tietoihin.

## Kuinka käyttää perusautentikointia HTTP-pyyntöihin

Perusautentikoinnin käyttämiseksi HTTP-pyyntöihin tarvitaan koodirivi, jossa määritellään käyttäjätunnus ja salasana. Esimerkiksi:

```Go
req.SetBasicAuth("käyttäjätunnus", "salasana")
```

Tässä koodissa määritellään käyttäjätunnus ja salasana osaksi HTTP-pyyntöä. Käyttäjätunnus ja salasana on tärkeää antaa oikein, jotta pyyntö voidaan lähettää onnistuneesti.

## Syvempää tietoa HTTP-pyynnön lähettämisestä perusautentikoinnilla

Perusautentikointi perustuu käyttäjätunnuksen ja salasanan antamiseen ja tarkistamiseen ennen kuin pyyntö hyväksytään. Tämä tapahtuu yleensä käyttäen Base64-koodausta, joka on menetelmä tekstin muuntamiseksi ASCII-muotoon.

HTTP-pyyntöjen lähettämisessä perusautentikoinnilla on tärkeää muistaa, että käyttäjätunnus ja salasana pitäisi aina lähettää turvallisella tavalla. Tämä tarkoittaa esimerkiksi HTTPS-yhteyden käyttämistä ja salauksen käyttämistä tietojen lähettämisessä.

# Katso myös

- [Go:n virallinen dokumentaatio HTTP-pyyntöjen lähettämisestä](https://golang.org/pkg/net/http/#Request.SetBasicAuth)
- [Perusautentikoinnin selitys ja esimerkkikoodia Go-kielellä](https://www.mysamplecode.com/2012/07/golang-http-client-setbasicauth-example.html)