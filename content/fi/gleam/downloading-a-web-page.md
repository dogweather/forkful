---
title:                "Gleam: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Miksi ladata verkkosivuja Gleam-ohjelmoinnilla?

Monet verkkosivustot tarjoavat hyödyllisiä tietoja, kuten uutisia, tuotetietoja tai tilastotietoja, joihin pääsy vaatii lataamista. Gleam-ohjelmointikieli tarjoaa yksinkertaisen ja tehokkaan tavan ladata verkkosivuja helposti ja nopeasti.

## Kuinka tehdä se Gleamilla?

[lataa verkkosivuion]

Gleamin `http` -kirjasto tarjoaa "GET"-toiminnon, joka pystyy lähettämään HTTP-pyynnön haluttuun verkkosivuun ja vastaanottamaan tiedot vastauksena. Voit käyttää tätä pyyntöä saadaksesi halutut tiedot verkkosivulta ja tallentaa ne muuttujaan käsiteltäväksi.

```Gleam
import http

let url = "https://example.com"
let response = http.get(url)
let data = response.body
```

Tässä koodiesimerkissä lataamme verkkosivun osoitteesta `https://example.com` ja tallennamme sen vastauksen muuttujaan, jotta voimme käsitellä sitä myöhemmin.

Kun olet tallentanut verkkosivun tiedot muuttujaan, voit käyttää sitä haluamallasi tavalla. Esimerkiksi voit tulostaa sivun sisällön Gleamin `io` -kirjastoa käyttäen.

```Gleam
import io

let content = data |> io.read_all()
io.print(content)
```

Tämä koodiesimerkki tulostaa verkkosivun sisällön konsoliin. Voit myös käsitellä tietoja ja eristää haluamiasi tietoja sivulta.

## Syvä sukellus verkkosivujen lataamiseen

Verkkosivujen lataamiseen liittyy myös muita asioita, kuten virheiden hallinta ja turvallisuus. Gleam-ohjelmointikieli tarjoaa joukon työkaluja, joita voit käyttää varmistaaksesi, että lataaminen on turvallista ja virheetön.

Voit esimerkiksi käyttää Gleamin `Result` -tyyppiä käsittelemään mahdollisia virheitä verkkosivujen lataamisen aikana. Tämä auttaa sinua hallitsemaan virheellisiä pyyntöjä ja varmistamaan, että sovelluksesi ei kaadu virheen sattuessa.

```Gleam
import http
import result

fn get_page(url) {
  http.get(url)
  |> result.from_result_error
}
```

Tässä koodiesimerkissä käytämme `get_page` -funktiota lataamaan verkkosivun ja käsittelemme mahdolliset virheet Gleamin `result` -kirjaston avulla.

# Katso myös

- [Gleam virallinen dokumentaatio](https://gleam.run/)
- [Gleam-ohjelmointikielen verkkosivut](https://gleam.run)
- [Gleamin `http` -kirjaston dokumentaatio](https://gleam.run/modules/http/)