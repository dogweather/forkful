---
title:                "Kotlin: Inviare una richiesta http"
simple_title:         "Inviare una richiesta http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Perché

Molte volte durante lo sviluppo di un'applicazione, potresti dover comunicare con un server esterno per ottenere o inviare dati. In questi casi, è necessario inviare una richiesta HTTP.

## Come fare

Per inviare una richiesta HTTP in Kotlin, puoi utilizzare la libreria standard `java.net.URL` e `java.net.HttpURLConnection`. Ecco un semplice esempio di codice per inviare una richiesta GET e ottenere una risposta:

```Kotlin
import java.net.URL
import java.net.HttpURLConnection

val url = URL("https://example.com/api/endpoint")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"

val responseCode = connection.responseCode
println("response code: $responseCode")

val response = connection.inputStream.bufferedReader().use { it.readText() }
println("response body: $response")
```

Questo codice crea un oggetto `URL` con l'indirizzo del server e una `HttpURLConnection` per gestire la connessione. Viene quindi impostato il metodo di richiesta sulla GET e viene inviata la richiesta al server. Infine, il codice ottiene il codice di risposta e il corpo della risposta utilizzando il metodo `inputStream` della connessione.

## Approfondimento

Ci sono molti parametri ed opzioni disponibili per personalizzare una richiesta HTTP, ad esempio puoi impostare degli header o inviare dei parametri con una richiesta POST. Si consiglia di leggere la documentazione ufficiale di `HttpURLConnection` per saperne di più.

## Vedi anche

- [Documentazione ufficiale di HttpUrlConnection](https://developer.android.com/reference/java/net/HttpURLConnection)
- [Tutorial su come inviare richieste HTTP in Kotlin](https://www.raywenderlich.com/230-an-introduction-to-kotlin-request-with-httplib)
- [Esempi di richieste HTTP in Kotlin](https://kotlinlang.org/docs/reference/basic-syntax.html#http-requests)