---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/gleam/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyynnön lähettäminen perustodentamisen kanssa on prosessi, jossa lähetetään tietoja palvelimelle käyttämällä salasanoja tai muita tunnistetietoja. Ohjelmoijat tekevät tämän ylläpitääkseen tietoturvaa ja suojatakseen luottamuksellisia tietoja.

## Miten:

Alla on esimerkki siitä, miten lähetetään HTTP-pyyntö perustodentamisen kanssa Gleam-ohjelmointikielellä:

```Gleam
let request = http.new_request("http://example.com")
 |> http.post(_, "body content")
 |> http.header("Authorization", "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==")
 |> http.make()
 
 case http.send(request) {
   Error(e) -> e
   Ok(response) -> response.body
 }
 ```
 
Tässä ohjelman tuloste:
 
 ```Gleam
 "Response body here..."
 ```
 
## Syvennys

HTTP-pyynnön lähettäminen perustodentamisella on ollut käytössä pitkään, ja sen historiallinen konteksti ulottuu verkkoprotokollien alkuun. Se on tärkeä arkaluonteisen datan lähettämisen menetelmä, mutta ei suositelluin vaihtoehto, koska sen tietoturva on verrattain heikko. Usein sen alternatiiveiksi ehdotetaan vahvempia todentamismenetelmiä, kuten token-pohjaista todentamista tai OAuth2:ta.

## Katso myös:

Lisätietoja ja muita lähteitä voi löytää seuraavista linkeistä:

3. [RFC7617- Basic Authentication Scheme](https://datatracker.ietf.org/doc/html/rfc7617)

Suosittelen tutustumaan näihin aiheisiin syvällisemmin etenkin, jos teet paljon verkko-ohjelmointia tai työskentelet arkaluonteisten tietojen kanssa.