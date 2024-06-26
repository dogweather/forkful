---
date: 2024-01-20 17:59:59.071111-07:00
description: 'How to: Kotlin makes HTTP requests straightforward. Here''s a basic
  example using `khttp`, a user-friendly library.'
lastmod: '2024-03-13T22:45:00.045730-06:00'
model: gpt-4-1106-preview
summary: Kotlin makes HTTP requests straightforward.
title: Sending an HTTP request
weight: 44
---

## How to:
Kotlin makes HTTP requests straightforward. Here's a basic example using `khttp`, a user-friendly library:

```Kotlin
import khttp.get

fun main() {
    val response = get("https://api.github.com/users/octocat/orgs")
    println(response.text)
}
```

Output:

```Kotlin
[{"login":"octo-org","id":583231,"url":"https://api.github.com/orgs/octo-org", ...}]
```

For more robust needs, here's a snippet using `ktor`, a Kotlin framework, to asynchronously fetch data:

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

Output similar to the first example.

## Deep Dive
The `khttp` library is a convenient tool, modeled after Python's `requests`. It's great for quick scripts but hasn't been actively maintained. `ktor` is a newer, active project by JetBrains, designed with coroutines for asynchronous operations. It's meant for scalable apps. Both handle HTTP requests but serve different use cases.

Historically, HTTP requests in Kotlin were done with Java libraries like `HttpURLConnection` or Apache's `HttpClient`. These are still valid but are more verbose and lack Kotlin's language features.

As for implementation, remember to handle common HTTP errors and read the response code. You'll also want to use `try-catch` for network exceptions and might need to work with headers and query parameters.

## See Also
- Ktor Documentation: https://ktor.io/
- khttp GitHub Repository: https://github.com/jkcclemens/khttp (Note the maintenance status)
- Kotlin HTTP calls with HttpURLConnection: https://kotlinlang.org/api/latest/jvm/stdlib/java.net/-http-u-r-l-connection/
