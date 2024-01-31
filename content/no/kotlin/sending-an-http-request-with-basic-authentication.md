---
title:                "Å sende en HTTP-forespørsel med grunnleggende autentisering"
date:                  2024-01-20T18:02:08.927213-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel med grunnleggende autentisering"

category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering innebærer å inkludere brukernavn og passord for å få tilgang til en ressurs på en server. Vi gjør dette for å sikre at bare godkjente brukere kan hente eller manipulere data.

## Hvordan:
For å sende en HTTP-forespørsel med Basisgodkjenning i Kotlin, bruk biblioteket `khttp`. Slik ser koden ut:

```Kotlin
import khttp.*

fun main() {
  val url = "https://your-api.com/endpoint"
  val credentials = Base64.getEncoder().encodeToString("brukernavn:passord".toByteArray())
  val response = khttp.get(url, headers = mapOf("Authorization" to "Basic $credentials"))

  println(response.text)
}
```

Et eksempel på utdata kan være:

```
{ "response": "Dette er dataene du ba om." }
```

## Dypdykk:
Historisk sett har Basisgodkjenning vært en enkel metode for å kontrollere tilgang til webressurser. Nå er det ofte erstattet av mer robuste autentiseringsmetoder som OAuth. Alternativer til Basisgodkjenning inkluderer API-nøkler, tokens og klientsertifikater. Å inkludere brukernavn og passord direkte er enkelt, men er mindre sikkert siden det kan avsløres hvis forespørselen avlyttes. Basisgodkjenningen bruker et `Authorization`-hode med verdien `Basic` etterfulgt av en Base64-kodet streng av brukernavnet og passordet.

## Se Også:
- [khttp documentation](https://khttp.readthedocs.io/en/latest/)
- [HTTP autentisering: Grunnleggende og Fordøyelse (MDN)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
