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

## Hvorfor

I dagens digitale verden er det ofte nødvendig å sende HTTP-forespørsler for å få tilgang til informasjon eller tjenester fra forskjellige nettsider og applikasjoner. Ved å bruke basic authentication kan man sikre at kun godkjente brukere har tilgang til disse ressursene, noe som er avgjørende for å opprettholde sikkerheten og personvernet til både brukeren og nettstedet. 

## Slik gjør du det

For å sende en HTTP-forespørsel med basic authentication i Kotlin, kan man bruke biblioteket `kotlin-clietnt` som er tilgjengelig via Maven eller Gradle. Først må man importere biblioteket i prosjektet sitt:

```Kotlin
import io.ktor.client.HttpClient
```

Deretter kan man opprette en ny instans av `HttpClient` og konfigurere den med basic authentication:

```Kotlin
val client = HttpClient {
    install(Auth) {
        basic {
            username = "brukernavn"
            password = "passord"
            sendWithoutRequest = true // Deprecated, should be used only for easy debugging or testing
        }
    }
}
```

Til slutt kan man definere og sende en HTTP-forespørsel ved å bruke det definerte klientobjektet:

```Kotlin
val response = client.get<String>("https://www.example.com")
println(response) // Skriver ut innholdet i responsen
```

Når koden kjøres, vil man motta en `200`-respons hvis basic authentication var vellykket, og man vil kunne få tilgang til ressursene på den gitte URL-en.

## Dypdykk

Basic authentication fungerer ved at brukerens legitimasjon blir kryptert og sendt sammen med HTTP-forespørselen i form av en Base64-kodet streng. Denne strengen består av brukernavnet og passordet separert med et kolon (`:`) og deretter kryptert til en sikker tekst som kun kan dekodes av den autoriserte serveren.

Ved å inkludere basic authentication i HTTP-forespørsler, sikrer man at kun brukere med riktige legitimasjonsopplysninger får tilgang til ressursene. Det er også mulig å bruke SSL-sertifikater for ekstra sikkerhet.

## Se også

- [Kotlin HttpClient dokumentasjon](https://ktor.io/clients/http-client/index.html)
- [HTTP basic authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)