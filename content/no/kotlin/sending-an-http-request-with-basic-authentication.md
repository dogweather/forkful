---
title:                "Kotlin: Send en http-forespørsel med grunnleggende autentisering"
simple_title:         "Send en http-forespørsel med grunnleggende autentisering"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor
HTTP-forespørsler med grunnleggende autentisering er en vanlig oppgave når man utvikler applikasjoner som kommuniserer med API-er. Dette er en effektiv og enkel måte å sikre at brukere har rettigheter til å hente eller sende data til en server. Ved å forstå hvordan man sender en HTTP-forespørsel med grunnleggende autentisering, vil du kunne forbedre sikkerheten og funksjonaliteten til applikasjonene dine.

## Hvordan
For å kunne sende en HTTP-forespørsel med grunnleggende autentisering, må du først opprette en instans av HttpURLConnection-objektet. Deretter må du sette metoden til "GET" eller "POST", avhengig av behov. Deretter setter du følgende to egenskaper:

```Kotlin
con.setRequestProperty("Authorization", "Basic " + Base64.getEncoder().encodeToString("username:password".toByteArray()))
con.setRequestProperty("Content-Type","application/json; utf-8")
```

Her bruker vi Base64-koding for å kryptere brukernavn og passord, som er en vanlig metode for grunnleggende autentisering. Vi setter også innholdstypen for forespørselen til JSON.

Nå kan vi sende forespørselen ved å bruke "getInputStream()" og lese responsen fra serveren:

```Kotlin
var statusCode = con.responseCode
var response = ""
if(statusCode == 200) {
    response = con.inputStream.bufferedReader().use{
        it.readText()
    }
}
```

Vi kan også legge til parametere i forespørselen ved å bruke "setRequestProperty()" og sende data i form av JSON ved å bruke "setDoOutput(true)" og "DataOutputStream". Etter å ha sendt forespørselen kan vi lese responsen fra serveren på samme måte.

## Dypdykk
Når du sender en HTTP-forespørsel med grunnleggende autentisering, må du sørge for at forbindelsen er sikker. Dette er spesielt viktig hvis du sender sensitiv informasjon som brukernavn og passord. Du bør også vurdere å bruke HTTPS i stedet for HTTP for å sikre at forbindelsen ikke kan bli avlyttet.

Det er viktig å også håndtere eventuelle feil som kan oppstå underveis i prosessen. Dette kan inkludere feil i nettverksforbindelsen, ugyldig eller manglende tilgang eller feil i forespørselen.

Sørg også for å ha god sikkerhetspraksis når det gjelder lagring og behandling av brukernavn og passord. Dette kan inkludere kryptering og sikker lagring i en database.

## Se også
- [Kotlin Dokumentasjon: HttpURLConnection](https://kotlinlang.org/api/latest/jvm/stdlib/java.net.-http-url-connection/index.html)
- [Android Developers Dokumentasjon: Sending HTTP Requests](https://developer.android.com/training/basics/network-ops/connecting.html)
- [Baeldung Tutorial: Basic Authentication with Java](https://www.baeldung.com/java-basic-authentication)