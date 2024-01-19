---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering handler enkelt sagt om å overføre data over internett på en sikker måte. Programmører gjør dette for å begrense tilgangen til sensitiv informasjon.

## Hvordan Gjør Man Det:
La oss se på hvordan du kan håndtere dette i Kotlin.

Du kan bruke biblioteket ktor for klientforespørselen. Hvis det ikke allerede er lagt til i prosjektet ditt, inkluder det med følgende:

```Kotlin
implementation 'io.ktor:ktor-client-core:1.6.3'
implementation 'io.ktor:ktor-client-cio:1.6.3'
implementation 'io.ktor:ktor-client-auth:1.6.3'
```

Kodeeksempel for å utføre autentiserte HTTP-forespørsler:

```Kotlin
import io.ktor.client.*
import io.ktor.client.features.auth.*
import io.ktor.client.features.auth.providers.*
import io.ktor.client.request.*
import io.ktor.client.statement.*

val client = HttpClient {
    install(Auth) {
        basic {
            sendWithoutRequest = true
            username = "brukernavn"
            password = "passord"
        }
    }
}

suspend fun main() {
    val response: HttpResponse = client.get("https://eksempel.com")
    println(response.readText())
}
```

## Dypdykk
Historisk sett har grunnleggende autentisering lenge vært en standardmetode for å sikre webapplikasjoner, selv om det har visse begrensninger, slik som at det ikke inneholder innebygd metode for å logge ut.

Det finnes flere alternativer til HTTP Basic Authentication, for eksempel Digest Access Authentication, OAuth eller JSON Web Tokens (JWT).

Angående implementeringsdetaljer brukes Base64-koding av brukernavn og passord som en del av autentiseringsprosessen i grunnleggende autentisering. Bemerk at Base64-koding ikke er en kryptografisk sikker mekanisme, den bare transformerer dataen på en måte som er designet for å hindre at den blir lurt av dataprotokoller.

## Se Også
Her er noen kilder for mer informasjon:

- Offisiell Kotlin dokumentasjon: https://kotlinlang.org/docs/home.html

- Ktor dokumentasjon for klienter med autentisering: https://ktor.io/docs/basic-auth-from-client.html

- HTTP Basic Authentication på MDN Web Docs: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication.