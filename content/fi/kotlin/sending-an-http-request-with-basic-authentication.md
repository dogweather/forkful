---
title:                "Perusautentikointia käyttävän http-pyynnön lähettäminen"
html_title:           "Kotlin: Perusautentikointia käyttävän http-pyynnön lähettäminen"
simple_title:         "Perusautentikointia käyttävän http-pyynnön lähettäminen"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Ennalta määritetty kirjautuminen HTTP-pyyntöjen lähettämisessä

Kun lähetät HTTP-pyynnön, voit käyttää ennalta määritettyä kirjautumista varmistaaksesi, että vain oikeat käyttäjät pääsevät tietoihisi. Tämä tapahtuu lisäämällä kirjautumistiedot tiedustelun otsikkoon, jotta palvelin voi varmentaa käyttäjän.

Ohjelmoijat käyttävät ennalta määritettyä kirjautumista varmistaakseen, että vain tietyt käyttäjät voivat käyttää tiettyjä palveluja tai tietoja. Tämä auttaa suojaamaan tietosi ja varmistamaan, että vain valtuutetut käyttäjät voivat tehdä muutoksia.

## Miten tehdä se:

Kotlinissa voit lähettää HTTP-pyynnön ennalta määritetyn kirjautumisen avulla seuraavalla tavalla:

```
val url = URL("url_of_your_service")
val connection = url.openConnection() as HttpURLConnection
connection.setRequestMethod("GET")
connection.setRequestProperty("Authorization", "Basic " + Base64.getEncoder().encodeToString("username:password".toByteArray()))
val responseCode = connection.responseCode
println("Response Code: $responseCode")
```

Tämä koodi lähettää GET-pyynnön määriteltyyn URL-osoitteeseen ja sisältää kirjautumistiedot tietoturvaa varten. Voit myös käyttää muita HTTP-metodeja (kuten POST tai PUT) ja vaihtaa käyttäjätunnuksen ja salasanan tosielämän kirjautumistiedoiksi.

## Syväsukellus:

Ennen ennalta määritetyn kirjautumisen käyttöä, käytettiin usein Basic Authentication -protokollaa. Tämä protokolla kuljetti käyttäjän tunnistetiedot avoimesti, mikä aiheutti tietoturvauhkia. Nykyään käytetään yleensä turvallisempia menetelmiä, kuten OAuthia, joka salaa tunnistetiedot.

On myös olemassa muita tapoja lähettää HTTP-pyyntöä ennalta määritetyllä kirjautumisella, kuten käyttämällä Java HttpClientia tai OkHttp-kirjastoa.

Kotlin tarjoaa myös sisäänrakennetun Base64-koodauksen, joten sinun ei tarvitse asentaa muita kirjastoja koodin suorittamiseksi.

## Katso myös:

- [Kotlin handle HTTP Basic authentication with HTTPURLConnection and base 64 encoding](https://stackoverflow.com/questions/37587812/kotlin-handle-http-basic-authentication-with-httpurlconnection-and-base-64-encodi)
- [Introduction to HTTP Basic Authentication](https://medium.com/@rajanmaharjan/introduction-to-http-basic-authentication-c736958b9e5c)
- [Using OAuth2 in Kotlin](https://www.baeldung.com/kotlin/oauth2)