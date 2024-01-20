---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

En HTTP-forespørsel er teknisk sett en henvendelse fra en klient til en server over Internett. Programmere bruker de til å hente, sende, eller oppdatere data på serveren. 

## Hvordan 

Her er en grunnleggende kode for å sende en GET HTTP-forespørsel i Kotlin med hjelp av ktor klientbiblioteket:

```Kotlin
import io.ktor.client.*
import io.ktor.client.request.*

suspend fun main() {
    val client = HttpClient()
    val response: String = client.get("http://example.com")
    println(response)
    client.close()
}
```
I denne koden, henter vi svar som tekst fra "http://example.com". Du vil se HTTP-responsen printet ut i konsollen din.

## Dypere Inn 

Historisk sett, ble HTTP (Hyper Tekst Transfer Protocol) utviklet for å gjøre datautveksling mellom klienter og servere mer strukturert og pålitelig. I dag, har det blitt en standard protokoll for webbasert kommunikasjon.

Alternativene til HTTP-forespørsler inkluderer bruken av WebSockets for real-time kommunikasjon, og GraphQL for mer effektiv dataforespørsel. 

Når det kommer til implementeringen, kan du i Kotlin bruke ulike HTTP-klientbiblioteker som OkHttp, Fuel, Ktor, osv. Valget avhenger av prosjektets behov og personlige preferanser.

## Se Også

- Offisielle ktor dokumentasjonen: [ktor.io/clients/http-client.html](https://ktor.io/clients/http-client.html)
- OkHttp GitHub side: [github.com/square/okhttp](https://github.com/square/okhttp)
- Fuel GitHub side: [github.com/kittinunf/fuel](https://github.com/kittinunf/fuel)

Lær mer om de ulike alternativene for å sende HTTP forespørsler i Kotlin, og velg den som passer best til dine behov!