---
date: 2024-01-20 17:44:27.188031-07:00
description: 'Come si fa: Utilizziamo `URL` e `HttpURLConnection` di Kotlin per scaricare
  il contenuto di una pagina web.'
lastmod: '2024-03-13T22:44:43.390108-06:00'
model: gpt-4-1106-preview
summary: Utilizziamo `URL` e `HttpURLConnection` di Kotlin per scaricare il contenuto
  di una pagina web.
title: Scaricare una pagina web
weight: 42
---

## Come si fa:
Utilizziamo `URL` e `HttpURLConnection` di Kotlin per scaricare il contenuto di una pagina web:

```kotlin
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.HttpURLConnection
import java.net.URL

fun downloadWebPage(url: String): String {
    val urlObj = URL(url)
    with(urlObj.openConnection() as HttpURLConnection) {
        requestMethod = "GET"
        
        BufferedReader(InputStreamReader(inputStream)).use {
            val response = it.readText()
            println(response)
            return response
        }
    }
}

fun main() {
    val webPageContent = downloadWebPage("http://example.com")
    println(webPageContent)
}
```
Quando esegui `main`, vedrai il contenuto della pagina web nel terminale.

## Approfondimento
Nel passato, programmi come `wget` e `curl` erano tipicamente usati per scaricare pagine web. Oggi, in Kotlin, `URL` e `HttpURLConnection` offrono un approccio diretto. Potresti anche usare librerie esterne come `OkHttp` per maggiori funzionalità e una sintassi più semplice.

Dettagli d'implementazione:
- `HttpURLConnection` permette di impostare metodo di richiesta e headers.
- `BufferedReader` e `InputStreamReader` convertono il flusso binario in testo.
- `InputStreamReader` usa il charset di default, che potrebbe non essere corretto per tutte le pagine.

Alternative:
- `OkHttp` è più moderno e facile da usare: gestisce meglio i timeout e gli errori.
- `jsoup` può essere utilizzato per il parsing del HTML se hai la necessità di estrarre dati specifici.

## Vedi Anche
- [OkHttp](https://square.github.io/okhttp/)
- [jsoup: Java HTML Parser](https://jsoup.org/)
- [Kotlin Documentation: Making HTTP Requests](https://kotlinlang.org/docs/java-interop.html#making-http-requests)
