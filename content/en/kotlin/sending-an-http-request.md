---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is a way for a program to communicate with a server, fetching or sending data. Programmers do this to interact with APIs, load media files, or submit form data.

## How to:

In Kotlin, we can use the Ktor library to send HTTP requests. It's simple and efficient. 

```kotlin
import io.ktor.client.*
import io.ktor.client.request.*

suspend fun main() {
    val client = HttpClient()

    val response: String = client.get("http://example.com")
    println(response) 
   
    client.close()
}
```
Executing the above code will print the HTML from "http://example.com" to your console. 

## Deep Dive 

Sending HTTP requests has been an integral part of programming since the advent of web services. Originally, this task was often handled using libraries that directly implemented the HTTP protocol. Nowadays, various third-party libraries such as Ktor in Kotlin make the process more standardized and user-friendly.

Alternatives to Ktor include OkHttp and Fuel. These libraries provide similar functionality but may vary in terms of verbosity and configuration options. The choice often depends on the specific project or personal preference.

When sending an HTTP request, you shouldn't have to worry about the underlying implementation details. Under the hood, the client creates a connection to the server, sends the HTTP request, waits for the response, and finally closes the connection. This process is abstracted in modern libraries like Ktor, leaving you to focus on the core functionality.

## See Also

For further knowledge on HTTP requests, check these links:

1. Official Ktor Documentation: [Ktor Docs](https://ktor.io/docs/http-client.html)
2. Comparison of HTTP Libraries in Kotlin: [Kotlin HTTP Libraries](https://blog.autsoft.hu/a-confusing-dependency-ktor-okhttp-and-retrofit/)
3. Understanding HTTP: [HTTP Basics](https://developer.mozilla.org/en-US/docs/Web/HTTP/Overview)

Please note that this article assumes a working knowledge of Kotlin. If you're new to Kotlin or need a refresher, it may be best to start here: [Kotlin Basics](https://kotlinlang.org/docs/kotlin-docs.pdf).