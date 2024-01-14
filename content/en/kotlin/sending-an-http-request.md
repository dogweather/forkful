---
title:                "Kotlin recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why
HTTP requests are an essential aspect of modern programming. It allows us to communicate with other web servers and retrieve data or perform actions. Understanding how to send an HTTP request is a crucial skill for any developer looking to build web applications or integrate different systems. In this blog post, we will explore the basics of sending an HTTP request using Kotlin.

## How To
To send an HTTP request in Kotlin, we will be using the built-in `URL` and `HttpURLConnection` classes. First, we need to create a `URL` object, specifying the URL we want to send the request to. Then, we can use the `openConnection()` method to create a connection to that URL. Finally, we can set the request method, headers, and body before sending the request.

```
Kotlin
val url = URL("https://example.com/api/users")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "POST"
connection.setRequestProperty("Content-Type", "application/json")
val body = "{\"name\":\"John Doe\",\"email\":\"johndoe@example.com\"}"
val outputStream = OutputStreamWriter(connection.outputStream)
outputStream.write(body)
outputStream.flush()
val responseCode = connection.responseCode
println("Response Code: $responseCode")
```

## Deep Dive
There are several things to note when sending an HTTP request in Kotlin. First, the `URL` class allows us to specify the protocol, hostname, and path in one string. We can also specify query parameters in the URL if needed. Second, the `HttpURLConnection` class gives us access to various request methods such as GET, POST, PUT, and DELETE. We can also set headers, such as the content type, authorization, or custom headers. Finally, we can write the request body using the `outputStream` and read the response using the `inputStream` of the connection.

It is important to handle exceptions when sending HTTP requests. Some common exceptions to handle are `MalformedURLException`, `IOException`, and `ProtocolException`. Additionally, it is a good practice to close the connection and input/output streams in a `finally` block to avoid any memory leaks.

## See Also
- [Kotlin documentation on HttpURLConnection](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-http-url-connection/)
- [Official Java documentation on HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Tutorial on sending HTTP requests in Kotlin](https://www.baeldung.com/kotlin-http-request)

Sending an HTTP request using Kotlin is a fundamental skill that will come in handy in many projects. By understanding the basics and exploring more advanced features, you will be able to create robust and efficient code that can communicate with other systems and retrieve data.