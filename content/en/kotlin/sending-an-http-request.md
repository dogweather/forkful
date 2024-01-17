---
title:                "Sending an http request"
html_title:           "Kotlin recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request is the process of making a request to a web server in order to retrieve information or perform an action. Programmers often use HTTP requests in their code to retrieve data from APIs, communicate with web services, or send data to a remote server for processing.

## How to:
Sending an HTTP request in Kotlin is a simple and straightforward process, thanks to the built-in library called "kotlinx.coroutines". Here's how you can send a GET request using this library:

```Kotlin
// Import the necessary libraries
import kotlinx.coroutines.*
import java.net.HttpURLConnection
import java.net.URL

// Set up a coroutine
val scope = CoroutineScope(Dispatchers.IO)
// Specify the URL for the request
val url = URL("https://jsonplaceholder.typicode.com/todos/1")
// Set up a connection to the URL
val connection: HttpURLConnection = url.openConnection() as HttpURLConnection
// Use a coroutine to send the request asynchronously
scope.launch {
    // Make sure the request was successful
    if (connection.responseCode == HttpURLConnection.HTTP_OK) {
        // Create an input stream to read the response data
        val inputStream = connection.inputStream
        // Read the data and print it to the console
        println(inputStream.bufferedReader().use { it.readText() })
    } else {
        // Request was not successful, handle the error
        println("Error: ${connection.responseCode}")
    }
}
```

Sample output:

`{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}`

## Deep Dive:
Historically, sending HTTP requests was a more complicated process, often requiring the use of external libraries or custom code. However, with the introduction of "kotlinx.coroutines" in Kotlin 1.3, sending HTTP requests has become much simpler and more efficient.

Other languages such as Java have their own libraries for sending HTTP requests, such as Apache HttpClient and OkHttp. However, these libraries often require a lot of boilerplate code and can be more difficult to use compared to the built-in support in Kotlin.

When sending an HTTP request in Kotlin, the "kotlinx.coroutines" library handles most of the low-level details, such as establishing and maintaining a connection, making it easier for the programmer to focus on the logic of their request.

## See Also:
- [Kotlinx Coroutines documentation](https://kotlinlang.org/docs/reference/coroutines-overview.html) for more information on using coroutines in Kotlin.
- [HTTP Requests in Java](https://www.baeldung.com/java-http-request) for an in-depth tutorial on sending HTTP requests in Java.
- [OkHttp documentation](https://square.github.io/okhttp/) for an alternative library for sending HTTP requests in Java.