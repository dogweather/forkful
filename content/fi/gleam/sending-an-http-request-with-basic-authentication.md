---
title:                "Perusautentikoinnin lähettäminen http-pyyntönä"
html_title:           "Gleam: Perusautentikoinnin lähettäminen http-pyyntönä"
simple_title:         "Perusautentikoinnin lähettäminen http-pyyntönä"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit lähettää HTTP-pyynnön perusautentikoinnilla?

HTTP-pyyntöjen lähettäminen perusautentikoinnilla on tärkeä osa monia web-sovelluksia, joissa käyttäjän täytyy todistaa olevansa oikea henkilö ennen kuin heille annetaan pääsy rajoitettuihin resursseihin. Tämä voi sisältää esimerkiksi salaisia tietokantoja, API-palveluita tai muita herkkiä tiedostoja.

## Miten

Gleamilla on helppo lähettää HTTP-pyyntöjä perusautentikoinnilla käyttäen `httpc` kirjastoa. Katso esimerkki alla:

```Gleam
import httpc

fn make_request() {
  let url = "https://api.example.com/user"
  let username = "käyttäjänimi"
  let password = "salasana"

  let response = httpc.request(
    method: "GET",
    url: url,
    headers: [
      ("Authorization", "Basic {base64_encode(username ++ ":" ++ password)}")
    ]
  )

  case response {
    Ok(httpc.Response(decoder)) -> {
      let body = decoder.read()
      // Tee jotain vastaukselle
    }
    Error(httpc.Error(err)) -> {
      // Käsittelyvirheet
    }
  }
}
```

Yllä oleva koodi lähettää GET-pyynnön annettuun URL-osoitteeseen ja ottaa käyttäjänimen ja salasanan käyttöön perusautentikointia varten. Vastauksena saatava data voi sitten käsitellä ja hyödyntää tarpeen mukaan. 

## Syvällinen tarkastelu

Perusautentikointi käyttää HTTP-pyynnön otsikkona `Authorization`-otsikkoa, jossa kerrotaan käyttäjän nimi ja salasana salausmuodossa. Tässä esimerkissä käytämme base64-koodausta, mutta autentikointimenetelmä voi vaihdella riippuen tarpeista ja käytetyistä palveluista. On tärkeää varmistaa, että käyttäjän salasana ei ole näkyvillä selkeästi käyttäjän koodissa.

## Katso myös

- [Gleam HTTP Client -kirjaston dokumentaatio](https://hexdocs.pm/gleam_httpc/)
- [HTTP-pyyntöjen lähettäminen Gleamilla](https://gleam.run/articles/http-requests)
- [HTTP-tilan koodauksen perusteet](https://gleam.run/articles/http-status-codes)