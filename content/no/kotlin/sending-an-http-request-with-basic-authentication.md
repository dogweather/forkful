---
title:                "Å sende en http forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Å sende en http forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http forespørsel med grunnleggende autentisering"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Sending av en HTTP-forespørsel med grunnleggende autentisering er en måte for en programmerer å få tilgang til et beskyttet nettsted eller nettverk. Dette er viktig for sikkerhet og personvern, da det betyr at bare de med riktig autentiseringsinformasjon kan få tilgang til sensitive data eller tjenester. 

## Hvordan:
Eksempel på Kotlin-kode for å sende en HTTP-forespørsel med grunnleggende autentisering og hvordan resultatet vil se ut:

```Kotlin
// Importerer nødvendige biblioteker 
import java.net.HttpURLConnection 
import java.net.URL 

// Definerer autentiseringsinformasjon 
val username = "brukernavn"
val password = "passord" 

// Oppretter en URL-objekt for nettstedet du vil få tilgang til 
val url = URL("https://nettsted.com") 

// Oppretter en HTTP-tilkobling 
val connection = url.openConnection() as HttpURLConnection 

// Setter autentiseringsmetoden til grunnleggende 
connection.setRequestProperty ("Authorization", "Basic " + 
        Base64.getMimeEncoder().encodeToString((username + ":" + password).toByteArray())) 

// Sender tilkoblingen 
connection.connect() 

// Skriver ut svaret fra nettstedet 
println(connection.responseMessage) 

// Lukker tilkoblingen 
connection.disconnect() 
```

Eksempel på resultat:
```
OK
```

## Dykk dypere:
- Historisk kontekst: Grunnleggende autentisering har vært en vanlig måte å autentisere brukere på siden starten av internett og er fortsatt i bruk i dag.
- Alternativer: Det finnes andre autentiseringsmetoder, som for eksempel OAuth eller HMAC, som tilbyr mer sikkerhet enn grunnleggende autentisering.
- Implementasjonsdetaljer: I eksempelet bruker vi Base64-koding for å sikre at autentiseringsinformasjonen blir sendt på en sikker måte.

## Se også:
- Dokumentasjon for HTTP-forespørsler med Kotlin: https://kotlinlang.org/docs/reference/using-gradle.html#http-client
- Eksempler på autentisering i Kotlin: https://www.baeldung.com/kotlin-http-basic-authentication