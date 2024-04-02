---
date: 2024-01-20 18:00:17.164851-07:00
description: "Mandare una richiesta HTTP significa comunicare con un server web: invii\
  \ una richiesta, lui ti risponde. Lo facciamo per prelevare dati, inviare form,\u2026"
lastmod: '2024-03-13T22:44:43.388287-06:00'
model: gpt-4-1106-preview
summary: "Mandare una richiesta HTTP significa comunicare con un server web: invii\
  \ una richiesta, lui ti risponde. Lo facciamo per prelevare dati, inviare form,\u2026"
title: Inviare una richiesta http
weight: 44
---

## What & Why?
Mandare una richiesta HTTP significa comunicare con un server web: invii una richiesta, lui ti risponde. Lo facciamo per prelevare dati, inviare form, interagire con API o accedere a servizi web.

## How to:
In Kotlin, inviare una richiesta HTTP è semplice. Ecco un esempio con `HttpURLConnection`:

```Kotlin
import java.net.HttpURLConnection
import java.net.URL

fun sendGetRequest() {
    val url = URL("http://example.com")
    with(url.openConnection() as HttpURLConnection) {
        requestMethod = "GET" // Tipo di richiesta

        inputStream.bufferedReader().use {
            val response = it.readText()
            println(response)
        }
    }
}

fun main() {
    sendGetRequest()
}
```

Questo stamperà la risposta del server.

Per POST, cambia `requestMethod` e scrivi nel `outputStream`:

```Kotlin
fun sendPostRequest() {
    val url = URL("http://example.com")
    val params = "param1=value1&param2=value2"

    with(url.openConnection() as HttpURLConnection) {
        requestMethod = "POST"
        doOutput = true

        outputStream.bufferedWriter().use {
            it.write(params)
        }

        inputStream.bufferedReader().use {
            val response = it.readText()
            println(response)
        }
    }
}

fun main() {
    sendPostRequest()
}
```

Assicurati di gestire le eccezioni dove necessario.

## Deep Dive
Le richieste HTTP risalgono agli inizi del web. Oggi, abbiamo standard come REST e GraphQL che usano HTTP.

Alternative a `HttpURLConnection` includono librerie esterne come OkHttp o Retrofit, che offrono un'esperienza più pulita e strutturata. Ad esempio, OkHttp gestisce meglio il pooling delle connessioni e i timeout.

Per Retrofit:

```Kotlin
import retrofit2.Retrofit
import retrofit2.converter.gson.GsonConverterFactory
import retrofit2.http.GET

interface ApiService {
    @GET("data")
    suspend fun fetchData(): MyData
}

fun main() {
    val retrofit = Retrofit.Builder()
        .baseUrl("http://example.com")
        .addConverterFactory(GsonConverterFactory.create())
        .build()
    
    val service = retrofit.create(ApiService::class.java)
    
    runBlocking {
        val data = service.fetchData()
        println(data)
    }
}
```

I dettagli di implementazione cambiano in base alle esigenze: sincrono vs. asincrono, trattamento degli errori, strategie di deserializzazione dei dati, ecc.

## See Also
- Documentazione ufficiale Kotlin: https://kotlinlang.org/docs/home.html
- OkHttp: https://square.github.io/okhttp/
- Retrofit: https://square.github.io/retrofit/
- Tutorial su HTTP in Kotlin: https://www.baeldung.com/kotlin-http-requests
