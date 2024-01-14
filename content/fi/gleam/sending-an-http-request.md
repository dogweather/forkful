---
title:                "Gleam: Lähetetään http-pyyntö"
simple_title:         "Lähetetään http-pyyntö"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Miksi: Miksi lähettää HTTP-pyyntö?

HTTP-pyyntöjen lähettäminen on tärkeä osa web-ohjelmointia ja antaa mahdollisuuden kommunikoida muiden palvelinten ja sovellusten kanssa. Se voi olla tarpeellista esimerkiksi tietojen hakemiseksi, tallentamiseksi tai päivittämiseksi.

# Kuinka: Esimerkkejä Gleam-koodilla ja tulostuksella

Yksinkertaisin tapa lähettää HTTP-pyyntö Gleamilla on käyttää `gleam_httpc` kirjastoa. Alla on esimerkki, jossa lähetämme GET-pyynnön ja tulostamme vastauksen kehon:

```Gleam
import gleam/httpc

let response = httpc.get("https://www.example.com")

case response {
  Ok(resp) -> 
    let code = resp.status_code
    let body = resp.body
    body
      |> to_string
      |> IO.println
  Err(err) -> 
    IO.println("Virhe: ${err}")
}
```

Tämä koodi lähettää GET-pyynnön osoitteeseen `https://www.example.com` ja tulostaa vastauksen HTML-kehon konsoliin. Voit myös käyttää muita HTTP-metodeja, kuten POST, ja määrittää erilaisia lisäparametreja kuten otsikoita ja kehoa.

# Syväsukellus: Lisätietoja HTTP-pyyntöjen lähettämisestä

HTTP-pyyntöjen lähettämiseen liittyy useita eri osia, kuten ostoskorin lisääminen tai kirjautuminen sisään. Gleamissa voit käyttää `httpc.shopper`- ja `httpc.authentication`-moduuleja auttamaan näiden tapausten käsittelyssä.

Voit myös luoda omia HTTP-pyyntöjä ja määrittää haluamasi HTTP-metodit, päänimimerkit ja kehon. Tämä antaa sinulle enemmän joustavuutta ohjelmoinnissa.

# Katso myös

- [Gleam HTTP-kirjastodokumentaatio](https://gleam.run/modules/httpc/latest/)
- [HTTP-metodien vertailu](https://developer.mozilla.org/fi/docs/Web/HTTP/Methods)
- [Ohjelmoidaan HTTP-pyyntöjä Gleamilla](https://learnxinyminutes.com/docs/fi-fi/gleam/)