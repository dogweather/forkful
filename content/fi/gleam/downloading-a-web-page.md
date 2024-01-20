---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Web-sivun lataaminen tarkoittaa tietokoneesi yhdistämistä www-palvelimeen kiinteän internet-yhteyden kautta nähdäksesi sen sisällön omassa selaimessasi. Ohjelmoijat tekevät tämän tutkiakseen, muokatakseen tai kerätäkseen tietoja verkkosivun koodista.

## Näin se tehdään:
Gleamilla web-sivun lataaminen on melko suoraviivaista. Seuraavassa on yksinkertainen koodiesimerkki:

```gleam
import gleam/http.{client, get}

fn download_page(url: String) {
  let Ok(response) = client.default()
    |> get("https://www.example.com")
    |> http.send()

  let body = response.body
  io.println(body)
}

fn main(args: List(String)) {
  download_page(args.0)
}
```

Kun suoritat tämän koodin, näet www.example.com-sivun HTML-lähtökoodin.

## Syvä sukellus
Web-sivun lataaminen ei ole uusi käytäntö; se on itse asiassa ollut mahdollista jo Internetin alkupäivistä lähtien. Tässä asiayhteydessä on tärkeää muistaa, että teet sen vastuullisesti ja kunnioitat verkkosivustojen käyttöehtoja ja yksityisyydensuojaa.

Vaihtoehtoisesti, voit kokeilla muita ohjelmointikieliä web-sivun lataamiseksi, esimerkiksi Pythonin `requests`-kirjasto tai JavaScriptin `axios`-kirjasto.

Gleamissa `http.send()` -metodi antaa vastauksen, johon sisältyy palvelimen lähettämä vastauskoodi, otsikot ja vastauksen runko. Rungon (body) tyyppi on bitstring, joka edustaa vastauksen raakatekstiä.

## Katso myös
Jos haluat tutkia Gleam-ohjelmointikieltä lisää, seuraavat resurssit voivat olla hyödyllisiä:
- [Gleam kotisivu](https://gleam.run/)
- [Gleam GitHub repo](https://github.com/gleam-lang/gleam)
- [HTTP-kirjaston dokumentointi](https://hexdocs.pm/gleam_http/readme.html)
- [Gleam-yhteisön Discord-chat](https://discord.gg/Fm8Pday2Jg)