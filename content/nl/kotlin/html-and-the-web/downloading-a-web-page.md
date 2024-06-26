---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:59:25.519893-07:00
description: "Hoe: Laten we aan de slag gaan met Kotlin's `HttpURLConnection` om snel\
  \ een webpagina te grijpen. We gebruiken ook coroutines voor soepele\u2026"
lastmod: '2024-03-13T22:44:50.766388-06:00'
model: gpt-4-0125-preview
summary: Laten we aan de slag gaan met Kotlin's `HttpURLConnection` om snel een webpagina
  te grijpen.
title: Een webpagina downloaden
weight: 42
---

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
