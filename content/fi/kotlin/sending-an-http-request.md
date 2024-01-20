---
title:                "HTTP-pyynnön lähettäminen"
html_title:           "Bash: HTTP-pyynnön lähettäminen"
simple_title:         "HTTP-pyynnön lähettäminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTTP-pyynnön lähettäminen on prosessi, jossa tietokone lähettää pyynnön palvelimelle tietojen saamiseksi tai lähettämiseksi. Ohjelmoijat tekevät tämän kommunikoidakseen APIen tai muiden resurssien kanssa verkon yli.

## Miten toimii:

Lähetämme HTTP-pyynnön Ktor-kirjaston avulla Kotlin-koodissa. Seuraa alla olevia askel-askeleelta ohjeita.

```kotlin
import io.ktor.client.*
import io.ktor.client.request.*

suspend fun main() {
    val client = HttpClient()

    val response: String = client.get("https://www.example.com")

    println(response)
}
```

Edellä oleva ohjelma luo HTTP-asiakkaan, lähettään `GET`-pyynnön esimerkkisivulle ja tulostaa saadut vastaukset.

## Syvemmällä:

### Historiallinen konteksti:
HTTP-pyynnöt ovat olleet olemassa lähes yhtä kauan kuin internet itse. Ne ovat perusta, jolla web-sovellukset kommunikoivat keskenään.

### Vaihtoehdot:
Vaikka tässä artikkelissa keskitymme Ktor-kirjastoon, on olemassa monia muita kirjastoja, kuten OkHttp ja Fuel, jotka tarjoavat saman toiminnallisuuden.

### Toteutuksen yksityiskohdat:
HTTP-pyynnöt voivat olla erityyppisiä, kuten `GET`, `POST`, `PUT` ja `DELETE`. Ktor tukee kaikkia näitä pyyntöjä. Se muuntaa pyynnöt HTTP-protokollan mukaisiksi viesteiksi, jotka lähetetään verkon kautta.

## Katso myös:

- [Ktor Tutotrials](https://ktor.io/docs/welcome.html)
- [OkHttp](https://square.github.io/okhttp/)
- [Fuel](https://github.com/kittinunf/fuel)
- [HTTP Request Methods Explained](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)