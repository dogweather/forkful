---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Gleam: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen on olennainen osa modernin web-sovelluksen toimintaa. Se mahdollistaa tietojen hakemisen ja lähettämisen eri web-palvelimien välillä sekä käyttäjän vuorovaikutuksen verkkosivustojen kanssa.

## Miten

```Gleam
import gleam/http

// Luo pyyntöobjekti ja aseta URL-osoite
let request = http
    .get("https://example.com/api")
    .header("Content-Type", "application/json")

// Lähetä pyyntö ja tallenna vastausmuuttujaan
let response = http.send(request)

// Hae vastauksen statuskoodi
let status = response.status

// Hae vastauksen sisältämä data
let data = response.body

// Tulosta vastaus
gleam/io.format("Statuskoodi on %d ja data on %s", [status, data])

```

Yllä olevassa koodiesimerkissä näkyy, miten Gleam-kielellä luodaan HTTP-pyyntö ja lähetetään se käyttäen gleam/http-pakettia. Pyyntöön voi lisätä otsikkotietoja ja vastauksesta voi hakea statuskoodin ja sisällön.

## Syvemmälle

HTTP-pyyntöjä lähetettäessä on tärkeää huomioida, että vastaanotetut tiedot eivät välttämättä ole turvallisia ja luotettavia. Tietoturvaan liittyvät riskit tulisi ottaa huomioon esimerkiksi lisäämällä tarkistuksia vastaanotetun datan muodolle ja alkuperälle.

Gleamissa on myös mahdollista lähettää asynkronisia HTTP-pyyntöjä käyttäen `http.async_send`-funktiota, joka palauttaa `Task`-objektin. Tätä voidaan hyödyntää esimerkiksi silloin, kun pyyntöihin ei ole heti tarvetta saada vastausta.

## Katso myös

- [Gleam-http: HTTP-pyyntöjen lähettämiseen tarkoitettu paketti](https://github.com/JoelW-S/gleam-http)
- [HTTP-koodit: Tietoa HTTP-pyyntöjen statuskoodeista](https://httpstatuses.com/)