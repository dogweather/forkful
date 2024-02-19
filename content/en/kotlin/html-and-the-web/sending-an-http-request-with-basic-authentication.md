---
aliases:
- /en/kotlin/sending-an-http-request-with-basic-authentication/
date: 2024-01-20 18:02:05.656700-07:00
description: Basic authentication slaps a username:password combo onto an HTTP request.
  Devs use it for a quick-and-dirty way to prove who's asking for what on the web.
lastmod: 2024-02-18 23:09:11.012480
model: gpt-4-1106-preview
summary: Basic authentication slaps a username:password combo onto an HTTP request.
  Devs use it for a quick-and-dirty way to prove who's asking for what on the web.
title: Sending an HTTP request with basic authentication
---

{{< edit_this_page >}}

## What & Why?

Basic authentication slaps a username:password combo onto an HTTP request. Devs use it for a quick-and-dirty way to prove who's asking for what on the web.

## How to:

Kotlin handles HTTP requests with libraries like `ktor` or `okhttp`. Let's roll with `okhttp` for now.

First, grab the library in your build.gradle:

```groovy
dependencies {
    implementation("com.squareup.okhttp3:okhttp:4.9.0")
}
```

Time to code:

```kotlin
import okhttp3.Credentials
import okhttp3.OkHttpClient
import okhttp3.Request
import java.io.IOException

fun main() {
    val client = OkHttpClient()

    val username = "admin"
    val password = "password123"
    val credentials = Credentials.basic(username, password)

    val request = Request.Builder()
        .url("http://example.com/resource")
        .header("Authorization", credentials)
        .build()

    client.newCall(request).execute().use { response ->
        if (!response.isSuccessful) throw IOException("Unexpected code $response")

        println(response.body!!.string())
    }
}
```

Hit run and watch your console. You should see the secured resource spill out.

## Deep Dive

Back in the day, HTTP Basic Auth was the go-to. Simple: just base64 the `username:password` and plop it in the header. Not secure alone, hence HTTPS joined the party.

Alternatives? Plenty. OAuth for tokens, API keys for simplicity, or digest authentication for an upgrade. Basic auth is good to start or for internal tools, but not for the modern, security-conscious web.

Implementation detail: Don't invent the wheel. Libraries handle encoding and protocol nuances. OkHttp even deals with retries and connections for you. Remember, basic auth over HTTP is a no-go—always use HTTPS to keep credentials safe in transit.

## See Also

- OkHttp's official documentation: [https://square.github.io/okhttp/](https://square.github.io/okhttp/)
- Kotlin language page (for all things Kotlin): [https://kotlinlang.org/](https://kotlinlang.org/)
- Learn more about Basic Auth: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)
- Alternatives to Basic Auth such as OAuth 2.0: [https://oauth.net/2/](https://oauth.net/2/)
