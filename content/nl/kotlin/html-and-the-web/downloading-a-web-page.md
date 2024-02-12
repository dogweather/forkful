---
title:                "Een webpagina downloaden"
aliases:
- /nl/kotlin/downloading-a-web-page.md
date:                  2024-01-28T21:59:25.519893-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een webpagina downloaden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/kotlin/downloading-a-web-page.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het downloaden van een webpagina betekent het ophalen van de HTML van een gegeven URL om lokaal te bekijken of te gebruiken. Programmeurs doen dit voor zaken zoals webscraping, offline lezen of geautomatiseerd testen.

## Hoe:
Laten we aan de slag gaan met Kotlin's `HttpURLConnection` om snel een webpagina te grijpen. We gebruiken ook coroutines voor soepele achtergrondprocessen. Hier is een introductie:

```kotlin
import java.net.HttpURLConnection
import java.net.URL
import kotlinx.coroutines.*

fun main() = runBlocking {
    val url = "http://example.com"
    val resultaat = withContext(Dispatchers.IO) {
        downloadWebpagina(url)
    }
    println(resultaat)
}

fun downloadWebpagina(urlAdres: String): String {
    val url = URL(urlAdres)
    val verbinding = url.openConnection() as HttpURLConnection
    try {
        verbinding.connect()
        return verbinding.inputStream.bufferedReader().use { it.readText() }
    } finally {
        verbinding.disconnect()
    }
}
```

Voorbeelduitvoer:

```
<!doctype html>
<html>
<head>
    <title>Voorbeeld Domein</title>
...
</html>
```
Mooi, hè? Je hebt de HTML van de webpagina!

## Diep Duiken
Het downloaden van webpagina's is zo oud als het web zelf. In de jaren '90 gebruikten mensen opdrachtregeltools zoals `wget` en `curl`. Ze zijn er nog steeds, maar wanneer je meer controle wilt of webinhoud ophalen wilt integreren in een app, codeer je het.

In Kotlin zou je Java's `HttpURLConnection` kunnen gebruiken of bibliotheken zoals OkHttp of Ktor voor een krachtige benadering met meer functies. Het bovenstaande voorbeeld is basic; in het echte leven zou je denken aan foutafhandeling, omleidingen en prestaties. Misschien toevoegingen van opnieuw proberen of een time-out? En je kunt het omgaan met verschillende karaktercoderingen en inhoudstypes niet vergeten.

Je zou ook nadenken over threading. We zouden de hoofdthread niet willen laten hangen terwijl we een gigantische pagina ophalen, toch? Vandaar, coroutines - ze laten je app responsief blijven, op de achtergrond ophalen zonder te zweten.

## Zie Ook
- **OkHttp**: https://square.github.io/okhttp/
- **Ktor Client**: https://ktor.io/docs/client.html
- **Kotlin Coroutines**: https://kotlinlang.org/docs/coroutines-overview.html
- **Java HttpURLConnection**: https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html

Dat is het in het kort—haal de pagina op, wees slim erover, en respecteer altijd de data en de bron ervan. Gelukkig coderen!
