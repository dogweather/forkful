---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Mikä & Miksi?
HTTP-pyyntö perustodennuksella on prosessi, jossa ohjelma lähettää verkkopyynnön samalla, kun se tarjoaa tunnistetiedot pääsynhallinnan tarkistamiseksi. Ohjelmoijat käyttävät tätä lähestymistapaa tiedonsiirron turvaamiseen ja pääsynhallinnan toteuttamiseen verkko-ohjelmissa.

# Näin tehdään:
Kotlinissa voimme käyttää Ktor-kirjastoa HTTP-pyyntöjen käsittelyyn.

```Kotlin
import io.ktor.client.*
import io.ktor.client.request.*
import io.ktor.client.features.auth.*
import io.ktor.client.features.auth.basic.*
import io.ktor.http.*

val client = HttpClient() {
    install(Auth) {
        basic {
            sendWithoutRequest = true
            credentials {
               BasicAuthCredentials(username = "username", password = "password")
           }
        }
    }
}

suspend fun sendRequest() {
    val response: String = client.get("https://api.verkkosivu.com/data");
    println(response)
}
```
Yllä oleva koodi luo HTTP-asiakkaan perustodennuksella ja lähettää GET-pyynnön.

# Syvennys:
Lähettäessään HTTP-pyynnön perustodennuksella, ohjelma liittää `Authorization`-otsakkeen HTTP-pyynnön otsakkeeseen. Otsakkeessa on käyttäjätunnus ja salasana, jotka on koodattu Base64-muotoon. Tällä menetelmällä on ollut keskeinen rooli web-todennuksessa sen jälkeen, kun se otettiin käyttöön HTTP/1.0:n myötä.

Vaihtoehtoisesti salasanan liittämisen sijaan voidaan käyttää muita todennusmenetelmiä, kuten OAuthia. On kuitenkin tärkeää huomata, että perustodennus ei tarjoa täydellistä turvaa, koska se lähettää todennustiedot selväkielisenä. Tämän korjaamiseksi perustodennus voidaan yhdistää SSL-salaukseen.

Ktor-kirjaston Basic-todennus toimii panemalla HttpClientin `install(Auth)`-funktioon ja asettamalla tarvittavat vakiotietoturvatiedot. Authentication-lisäosa lähettää perustodennuksen otsakkeen jokaisella pyynnöllä.

# Katso myös:
Käy läpi seuraavat linkit oppiaksesi lisää:

- Ktor HTTP Client: https://ktor.io/docs/http-client.html
- Basic Authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- OAuth: https://oauth.net/2/