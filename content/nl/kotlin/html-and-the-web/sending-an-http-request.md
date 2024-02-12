---
title:                "Een HTTP-verzoek verzenden"
aliases:
- /nl/kotlin/sending-an-http-request/
date:                  2024-01-28T22:07:32.209352-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een HTTP-verzoek verzenden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/sending-an-http-request.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Een HTTP-verzoek verzenden is als een webserver vragen om iets te doen of je iets te geven. Programmeurs doen dit om te interacteren met webservices, gegevens op te halen, formulieren in te dienen of te communiceren met API's.

## Hoe:

Kotlin maakt HTTP-verzoeken eenvoudig. Hier is een basisvoorbeeld met `khttp`, een gebruiksvriendelijke bibliotheek:

```Kotlin
import khttp.get

fun main() {
    val response = get("https://api.github.com/users/octocat/orgs")
    println(response.text)
}
```

Uitvoer:

```Kotlin
[{"login":"octo-org","id":583231,"url":"https://api.github.com/orgs/octo-org", ...}]
```

Voor meer robuuste behoeften, hier is een fragment dat `ktor`, een Kotlin-framework, gebruikt om asynchroon gegevens op te halen:

```Kotlin
import io.ktor.client.*
import io.ktor.client.engine.cio.*
import io.ktor.client.request.*

suspend fun main() {
    val client = HttpClient(CIO)
    val response: String = client.get("https://api.github.com/users/octocat/orgs")
    println(response)
    client.close()
}
```

Uitvoer vergelijkbaar met het eerste voorbeeld.

## Diepere Duik

De `khttp` bibliotheek is een handig hulpmiddel, gemodelleerd naar Python's `requests`. Het is geweldig voor snelle scripts, maar is niet actief onderhouden. `ktor` is een nieuwer, actief project van JetBrains, ontworpen met coroutines voor asynchrone operaties. Het is bedoeld voor schaalbare apps. Beide handelen HTTP-verzoeken af, maar dienen verschillende gebruikssituaties.

Historisch gezien werden HTTP-verzoeken in Kotlin gedaan met Java-bibliotheken zoals `HttpURLConnection` of Apache's `HttpClient`. Deze zijn nog steeds geldig, maar zijn uitgebreider en missen de taalfeatures van Kotlin.

Wat implementatie betreft, vergeet niet om veelvoorkomende HTTP-fouten te behandelen en de responscode te lezen. Je zult ook `try-catch` willen gebruiken voor netwerkuitzonderingen en moet mogelijk werken met headers en queryparameters.

## Zie Ook

- Ktor Documentatie: https://ktor.io/
- khttp GitHub Repository: https://github.com/jkcclemens/khttp (Let op de onderhoudsstatus)
- Kotlin HTTP-oproepen met HttpURLConnection: https://kotlinlang.org/api/latest/jvm/stdlib/java.net/-http-u-r-l-connection/
