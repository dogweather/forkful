---
date: 2024-01-20 18:02:10.296949-07:00
description: "Jak to zrobi\u0107: Uwierzytelnianie podstawowe (Basic Authentication)\
  \ to standardowy spos\xF3b na przekazanie nazwy u\u017Cytkownika i has\u0142a w\
  \ nag\u0142\xF3wkach HTTP.\u2026"
lastmod: '2024-04-05T21:53:36.801900-06:00'
model: gpt-4-1106-preview
summary: "Uwierzytelnianie podstawowe (Basic Authentication) to standardowy spos\xF3\
  b na przekazanie nazwy u\u017Cytkownika i has\u0142a w nag\u0142\xF3wkach HTTP."
title: "Wysy\u0142anie zapytania http z podstawow\u0105 autoryzacj\u0105"
weight: 45
---

## Jak to zrobić:
```kotlin
import java.net.HttpURLConnection
import java.net.URL
import java.util.Base64

fun sendGetRequestWithBasicAuth(url: String, username: String, password: String) {
    val urlConnection = URL(url).openConnection() as HttpURLConnection
    val credentials = "$username:$password"
    val encodedCredentials = Base64.getEncoder().encodeToString(credentials.toByteArray())
    urlConnection.requestProperty["Authorization"] = "Basic $encodedCredentials"

    try {
        val responseCode = urlConnection.responseCode
        if (responseCode == HttpURLConnection.HTTP_OK) {
            val response = urlConnection.inputStream.bufferedReader().readText()
            println("Response: $response")
        } else {
            println("GET request not worked. Response code: $responseCode")
        }
    } finally {
        urlConnection.disconnect()
    }
}

// Użycie funkcji
val testUrl = "https://example.com/api/data"
val username = "myUser"
val password = "myPass123"
sendGetRequestWithBasicAuth(testUrl, username, password)
```

## Deep Dive:
Uwierzytelnianie podstawowe (Basic Authentication) to standardowy sposób na przekazanie nazwy użytkownika i hasła w nagłówkach HTTP. Pochodzi z początków internetu, ale jest nadal popularne ze względu na swoją prostotę. Alternatywne metody uwierzytelniania obejmują OAuth i tokeny API, które oferują lepszą bezpieczeństwo, ale są bardziej skomplikowane w obsłudze.

W implementacji Kotlin używamy klasy `HttpURLConnection`, do której dołączamy zakodowane w Base64 dane uwierzytelniające. Pamiętajmy, że taki sposób przesyłania wrażliwych informacji jest bezpieczny tylko przy użyciu szyfrowanego połączenia (HTTPS), inaczej dane mogą być przechwycone w transmisji.

## Zobacz też:
- [Kotlin Documentation](https://kotlinlang.org/docs/home.html) - dokumentacja języka Kotlin.
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617) - specyfikacja uwierzytelniania podstawowego.
- [Mozilla Developer Network - HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication) - przewodnik po uwierzytelnianiu HTTP.
