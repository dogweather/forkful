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

## Why

Sending HTTP requests is a key feature of web development and allows developers to communicate with servers, retrieve data, and perform various actions. Knowing how to send HTTP requests enables developers to create dynamic and interactive web applications, making it an essential skill for any web developer.

## How To

Sending an HTTP request can be done using the built-in `URL` and `HttpURLConnection` classes in Kotlin. First, create a `URL` object with the desired URL as its parameter:

```Kotlin
val url = URL("https://www.example.com/api/users")
```

Next, open a connection to that URL using the `HttpURLConnection` class:

```Kotlin
val connection = url.openConnection() as HttpURLConnection
```

Specify the type of request and any necessary headers using the `RequestMethod` and `RequestProperty`:

```Kotlin
connection.requestMethod = "GET"
connection.setRequestProperty("Content-Type", "application/json")
```

If there is a request body, it can be added using the connection's `OutputStream`:

```Kotlin
val requestBody = "username=test&password=1234"
val outputStream = connection.outputStream
outputStream.write(requestBody.toByteArray())
```

To receive the response, use the connection's `inputStream`:

```Kotlin
val inputStream = connection.inputStream
```

Finally, read and parse the response data:

```Kotlin
val bufferedReader = BufferedReader(InputStreamReader(inputStream))
var response = ""
var inputLine: String?
while ((inputLine = bufferedReader.readLine()) != null) {
    response += inputLine
}
```

The `response` string will contain the data returned from the HTTP request. Here is a complete example of sending a GET request and parsing the response as JSON using the `Gson` library:

```Kotlin
val url = URL("https://www.example.com/api/users")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"
connection.setRequestProperty("Content-Type", "application/json")
val bufferedReader = BufferedReader(InputStreamReader(connection.inputStream))
var response = ""
var inputLine: String?
while ((inputLine = bufferedReader.readLine()) != null) {
    response += inputLine
}
val gson = Gson()
val userList: List<User> = gson.fromJson(response, object : TypeToken<List<User>>() {}.type)
for (user in userList) {
    println("Username: ${user.username}, Email: ${user.email}")
}
```

Sample output:

```
Username: john123, Email: john123@example.com
Username: sarah456, Email: sarah@example.com
```

## Deep Dive

While the above example provides a basic understanding of sending HTTP requests, there are various other options and libraries that can be used for more complex requests. For example, the `OkHttp` library provides a more user-friendly API for making HTTP requests, and the `Retrofit` library allows for creating interfaces to define the API endpoints and automatically handle the creation of requests and parsing of responses.

Additionally, HTTP requests can be asynchronous to prevent blocking the main thread and improve performance. This can be done using coroutines or callbacks.

It is also important to handle errors and exceptions when sending HTTP requests, as there is always a possibility of the request failing. Be sure to handle any potential errors and implement proper error handling and retry mechanisms.

## See Also

- Official Kotlin Documentation for sending HTTP requests: https://kotlinlang.org/docs/reference/http-client.html
- Official OkHttp Documentation: https://square.github.io/okhttp/
- Official Retrofit Documentation: https://square.github.io/retrofit/
- Tutorial on using coroutines for asynchronous HTTP requests in Kotlin: https://www.raywenderlich.com/772-async-http-clients-in-android-with-kotlin-coroutines-and-retrofit