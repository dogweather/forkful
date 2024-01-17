---
title:                "Sending en http-forespørsel"
html_title:           "Kotlin: Sending en http-forespørsel"
simple_title:         "Sending en http-forespørsel"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

Velkommen til en rask introduksjon til å sende HTTP forespørsler i Kotlin! I denne artikkelen vil vi se på hva det betyr å sende en HTTP forespørsel, hvorfor programmerere gjør det og hvordan du gjør det i Kotlin.

## Hva & Hvorfor?

En HTTP forespørsel er en måte å kommunisere med en annen datamaskin på nettverket, vanligvis en fjern webserver. Programmører gjør det for å få tilgang til data eller tjenester som er lagret på disse serverne. Det kan også være for å sende data til en annen server for behandling. Kort sagt, det er grunnlaget for kommunikasjon på internett.

## Hvordan:

Her er et enkelt eksempel på hvordan du sender en HTTP forespørsel i Kotlin:

```Kotlin
val url = "http://www.example.com" // velg en nettside å kommunisere med
val client = OkHttpClient() // opprett en klient for å sende forespørselen
val request = Request.Builder()
            .url(url)
            .build() // lag en forespørsel med ønsket URL
val response = client.newCall(request).execute() // send forespørselen og lagre responsen
println(response.body()?.string()) // skriv ut innholdet i responsen
```

Output vil være innholdet på nettsiden du har angitt i URL-en.

## Dypdykk:

HTTP-protokollen ble utviklet på 1990-tallet for å standardisere kommunikasjonen mellom servere og klienter på internett. Alternativene til å sende HTTP forespørsler inkluderer FTP, som er en eldre protokoll som er begrenset til filoverføring. I dag er det også alternative protokoller som SPDY og HTTP/2 som tilbyr raskere og mer effektiv kommunikasjon.

Implementeringen av HTTP forespørsler i Kotlin krever en HTTP-klient, hvorav OkHttp er en populær og pålitelig. Alternativt, hvis du bruker Kotlin på Android, kan du også bruke Androids egne API-er for å sende HTTP forespørsler.

## Se også:

For mer informasjon om å sende HTTP forespørsler i Kotlin, kan du sjekke ut Kotlin sine offisielle dokumenter: https://kotlinlang.org/docs/reference/native/networking.html eller OkHttp-dokumentasjonen: https://square.github.io/okhttp/