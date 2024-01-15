---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
html_title:           "Kotlin: HTTP-pyynnön lähettäminen perusautentikoinnilla"
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Miksi

Miksi joku haluaisi lähettää HTTP-pyynnön perusautentikoinnilla? Yksi yleinen syy tähän on, että halutaan varmistaa, että vain oikeutetut käyttäjät voivat käyttää tiettyä web-sovellusta tai palvelua.

## Miten

```Kotlin
// Lisätään tarvittavat importit
import java.net.URL
import java.net.HttpURLConnection
import java.nio.charset.StandardCharsets
import java.util.Base64

// Luodaan muuttujat pyynnön URL-osoitteelle ja käyttäjän tiedoille
val url = URL("https://example.com/api/users")
val username = "käyttäjänimi"
val password = "salasana"

// Luodaan HTTP-yhteys ja asetetaan siihen pyyntötyyppi ja perusautentikointi
val connection = url.openConnection() as HttpURLConnection
connection.setRequestMethod("GET")
val auth = username + ":" + password
val encodedAuth = Base64.getEncoder().encodeToString(auth.toByteArray(StandardCharsets.UTF_8))
connection.setRequestProperty("Authorization", "Basic " + encodedAuth)

// Tulostetaan vastauskoodi
println("Vastauskoodi: ${connection.responseCode}")

// Luetaan vastauksen sisältö
val response = connection.inputStream.bufferedReader().readText()
println("Vastaus: $response")
```

Koodiesimerkissä luodaan HTTP-yhteys ja siihen lisätään pyyntötyyppi (GET) ja perusautentikointi käyttäjän antamien tietojen perusteella. Tämän jälkeen lähetetään pyyntö ja tulostetaan vastauksen sisältö sekä vastauskoodi.

## Syväsukellus

Perusautentikointi toimii lähettämällä käyttäjänimi ja salasana Base64-koodattuna HTTP-pyynnön otsikkoon. Tämä tapa on turvallinen, mutta ei välttämättä riitä kaikissa tilanteissa. Parempi vaihtoehto on käyttää TLS/SSL-salausta, jolloin käyttäjätiedot eivät kulje selkeästi tietoverkkoa pitkin.

## Katso myös

- [Basic Authentication in Kotlin](https://www.baeldung.com/kotlin/http-request-basic-authentication) 
- [HTTPURLConnection class in Kotlin](https://developer.android.com/reference/java/net/HttpURLConnection) 
- [Base64 class in Kotlin](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)