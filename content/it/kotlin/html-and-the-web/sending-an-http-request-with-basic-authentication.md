---
title:                "Inviare una richiesta http con autenticazione di base"
aliases: - /it/kotlin/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:14.407826-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http con autenticazione di base"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
Inviare una richiesta HTTP con autenticazione di base significa inserire username e password per accedere a risorse protette su un server. I programmatori usano questo metodo per interagire con API che richiedono un livello di sicurezza.

## How to:
In Kotlin, possiamo utilizzare `HttpURLConnection` per inviare richieste HTTP con autenticazione di base.

```kotlin
import java.net.HttpURLConnection
import java.net.URL
import java.util.Base64

fun main() {
    val url = URL("http://tuo.server.com/api")
    val username = "tuonomeutente"
    val password = "tuapassword"

    with(url.openConnection() as HttpURLConnection) {
        val creds = "$username:$password"
        val auth = Base64.getEncoder().encodeToString(creds.toByteArray())
        setRequestProperty("Authorization", "Basic $auth")

        println("Response Code: $responseCode")
        println("Response Message: $responseMessage")
    }
}
```
Output potrebbe essere simile a:

```
Response Code: 200
Response Message: OK
```

## Deep Dive
L'autenticazione di base HTTP è uno dei metodi più semplici: codifica username e password in Base64 e le inserisce nell'header della richiesta HTTP. Nato negli anni '90, non è il più sicuro perché può essere facilmente decodificato. Alternative moderne includono OAuth e JWT che forniscono maggiore sicurezza attraverso token.

`HttpURLConnection` è parte del JDK standard, ma librerìe come OkHttp o Retrofit rendono la codifica meno verbosa e più gestibile. Ecco un esempio con OkHttp:

```kotlin
val client = OkHttpClient()
val credentials = Credentials.basic("username", "password")
val request = Request.Builder()
    .url("http://your.server.com/api")
    .header("Authorization", credentials)
    .build()

client.newCall(request).execute().use { response ->
    println(response.code)
    println(response.message)
}
```

## See Also
- [RFC 7617 'The 'Basic' HTTP Authentication Scheme'](https://tools.ietf.org/html/rfc7617)
- [OkHttp Library](https://square.github.io/okhttp/)
- [Retrofit Library by Square](https://square.github.io/retrofit/)
