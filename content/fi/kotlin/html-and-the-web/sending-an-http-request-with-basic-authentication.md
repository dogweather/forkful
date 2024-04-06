---
date: 2024-01-20 18:01:51.988751-07:00
description: "How to - Kuinka tehd\xE4: Perusautentikointi on yksinkertainen HTTP:n\
  \ autentikointimekanismi, joka kehitettiin internetin alkuaikoina. Se ei ole erityisen\u2026"
lastmod: '2024-04-05T22:51:10.688944-06:00'
model: gpt-4-1106-preview
summary: Perusautentikointi on yksinkertainen HTTP:n autentikointimekanismi, joka
  kehitettiin internetin alkuaikoina.
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
weight: 45
---

## How to - Kuinka tehdä:
```Kotlin
import java.net.HttpURLConnection
import java.net.URL
import java.util.Base64

fun sendGetRequestWithBasicAuth(url: String, username: String, password: String) {
    val connection = URL(url).openConnection() as HttpURLConnection

    val auth = Base64.getEncoder().encodeToString("$username:$password".toByteArray())
    connection.requestProperty["Authorization"] = "Basic $auth"

    connection.connect()

    val responseCode = connection.responseCode
    val responseMessage = if (responseCode == HttpURLConnection.HTTP_OK) {
        connection.inputStream.bufferedReader().readText()
    } else {
        "Error $responseCode: ${connection.responseMessage}"
    }

    println("Response: $responseMessage")
}

fun main() {
    val url = "http://example.com/api/data"
    val username = "user123"
    val password = "pass456"

    sendGetRequestWithBasicAuth(url, username, password)
}
```

Sample output:
```
Response: { "data": "Confidential data..." }
```

## Deep Dive - Syväsukellus:
Perusautentikointi on yksinkertainen HTTP:n autentikointimekanismi, joka kehitettiin internetin alkuaikoina. Se ei ole erityisen turvallinen, sillä tunnistetiedot lähetetään BASE64-koodattuna, mikä on helposti purettavissa. Siksi suositellaan käyttämään vahvempia menetelmiä, kuten OAuth 2.0, kun mahdollista. Javassa HttpURLConnection on yksi tapa lähettää HTTP-pyynnöt, mutta Kotlinissa voi myös käyttää kotlinx.coroutines ja okhttp kolmannen osapuolen kirjastoja, jotka tukevat asynkronista suoritusta ja ovat kohtuullisen helppokäyttöisiä.

## See Also - Katso Myös:
- [RFC 7617 'The 'Basic' HTTP Authentication Scheme'](https://tools.ietf.org/html/rfc7617)
- [OkHttp Library](https://square.github.io/okhttp/)
- [Kotlin Coroutines](https://kotlinlang.org/docs/reference/coroutines-overview.html)
