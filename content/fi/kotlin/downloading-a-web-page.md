---
title:                "Verkkosivun lataaminen"
html_title:           "Kotlin: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Web-sivun lataaminen on prosessi, jossa ohjelmoija ottaa kopion verkkosivun sisällöstä ja tallentaa sen paikalliseen tietokoneeseensa. Tätä tehdään yleensä joko sisällön käyttämiseksi myöhemmin tai tiedon keräämiseksi analysointia varten.

## Miten:
```Kotlin
val url = "https://www.esimerkki.fi"
val conn = URL(url).openConnection() as HttpURLConnection
val input = conn.inputStream.bufferedReader().use { it.readText() }
println(input)
```
Tämä koodi lataa verkkosivun sisällön `url` muuttujassa määritetystä URL-osoitteesta käyttäen `HttpURLConnection` luokkaa ja tulostaa sen konsolille. Koodissa käytetään myös `bufferedReader` ja `use` metodeja helpottamaan lukuoperaatiota.

## Syvällisempi sukellus:
Verkkosivun lataamisella on pitkä historia internetin alkuaikojen jälkeen. Aiemmin sitä käytettiin lähinnä tiedonkeruuseen web-skrapingin avulla, mutta nykyään se on tärkeä osa monia sovelluksia, kuten verkkoselaimia ja mobiilisovelluksia.

On myös muita tapoja ladata verkkosivun sisältöä, kuten käyttämällä kirjastoja kuten `OkHttp` tai `HttpUrlConnection`, mutta Kotlinin `HttpURLConnection` on helppo tapa aloittaa ja soveltuu hyvin yksinkertaisten operaatioiden suorittamiseen.

Verkkosivun lataamisen toteutus riippuu käytetystä protokollasta, mutta perusperiaate on sama: luodaan yhteys verkkosivun osoitteeseen ja vastaanotetaan sen sisältö. Tämän jälkeen sisältöä voidaan käsitellä halutulla tavalla, esimerkiksi tallentaa paikallisesti tai analysoida.

## Katso myös:
- [Kotlin virallinen dokumentaatio](https://kotlinlang.org/docs/reference/)
- [HTTP ja Kotlin: Creating Data Requests With URlConnection](https://www.baeldung.com/kotlin-httpurlconnection)