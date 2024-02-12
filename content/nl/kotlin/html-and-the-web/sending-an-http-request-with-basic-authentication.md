---
title:                "Een HTTP-verzoek verzenden met basisauthenticatie"
aliases: - /nl/kotlin/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-28T22:08:06.885023-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden met basisauthenticatie"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/sending-an-http-request-with-basic-authentication.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Basisauthenticatie slingert een gebruikersnaam:wachtwoord combinatie op een HTTP-verzoek. Ontwikkelaars gebruiken het als een snelle en vuile manier om te bewijzen wie wat vraagt op het web.

## Hoe:

Kotlin handelt HTTP-verzoeken af met bibliotheken zoals `ktor` of `okhttp`. Laten we voor nu met `okhttp` doorgaan.

Pak eerst de bibliotheek in je build.gradle:

```groovy
dependencies {
    implementation("com.squareup.okhttp3:okhttp:4.9.0")
}
```

Tijd om te coderen:

```kotlin
import okhttp3.Credentials
import okhttp3.OkHttpClient
import okhttp3.Request
import java.io.IOException

fun main() {
    val client = OkHttpClient()

    val username = "admin"
    val password = "password123"
    de credentials = Credentials.basic(username, password)

    val request = Request.Builder()
        .url("http://example.com/resource")
        .header("Authorization", credentials)
        .build()

    client.newCall(request).execute().use { response ->
        if (!response.isSuccessful) gooi IOException("Onverwachte code $response")

        println(response.body!!.string())
    }
}
```

Druk op uitvoeren en kijk naar je console. Je zou de beveiligde bron moeten zien uitkomen.

## Diepe Duik

Vroeger was HTTP Basis Auth de standaard. Simpel: gewoon de `gebruikersnaam:wachtwoord` in base64 coderen en die in de kop zetten. Niet alleen veilig, vandaar dat HTTPS zich bij het feestje voegde.

Alternatieven? Zat. OAuth voor tokens, API-sleutels voor eenvoud, of digest-authenticatie voor een upgrade. Basisauthenticatie is goed om te beginnen of voor interne gereedschappen, maar niet voor het moderne, veiligheidsbewuste web.

Implementatiedetail: Vind het wiel niet opnieuw uit. Bibliotheken handelen codering en protocolnuances af. OkHttp gaat zelfs om met opnieuw proberen en verbindingen voor je. Onthoud, basisauthenticatie over HTTP is een no-go—gebruik altijd HTTPS om inloggegevens veilig in transit te houden.

## Zie Ook

- Officiële documentatie van OkHttp: [https://square.github.io/okhttp/](https://square.github.io/okhttp/)
- Kotlin-taalpagina (voor alles over Kotlin): [https://kotlinlang.org/](https://kotlinlang.org/)
- Leer meer over Basis Auth: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- Alternatieven voor Basis Auth zoals OAuth 2.0: [https://oauth.net/2/](https://oauth.net/2/)
