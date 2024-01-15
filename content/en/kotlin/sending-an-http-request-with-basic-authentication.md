---
title:                "Sending an http request with basic authentication"
html_title:           "Kotlin recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Why

Sending HTTP requests with basic authentication is a common practice when working with web APIs. It allows for secure communication by requiring a username and password to access protected resources.

## How To

Sending an HTTP request with basic authentication in Kotlin is simple and straightforward. First, import the necessary packages for working with HTTP requests:
```
import java.net.HttpURLConnection
import java.net.URL
```
Next, create a URL object using the desired API endpoint:
```
val url = URL("https://example.com/api/endpoint")
```
Then, create a HttpURLConnection object and set the request method:
```
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"
```
To add basic authentication, set the "Authorization" header with the username and password encoded in Base64:
```
val username = "username"
val password = "password"
val encodedCredentials = Base64.getEncoder().encodeToString("$username:$password".toByteArray())
connection.setRequestProperty("Authorization", "Basic $encodedCredentials")
```
Finally, send the request and handle the response accordingly:
```
val responseCode = connection.responseCode
if(responseCode == HttpURLConnection.HTTP_OK){
    // Request was successful, do something with the response
    val inputStream = connection.inputStream
    // Process the JSON or data received
} else {
    // Request failed, handle the error
}

// Remember to close the connection when finished
connection.disconnect()
```

## Deep Dive

Basic authentication is a simple but effective way to restrict access to resources on a server. It works by requiring a username and password to be sent with each request, which is then encoded in Base64 format. This method should only be used when communicating over HTTPS, as the username and password are easily readable if sent over plain HTTP.

By sending the credentials in Base64 format, they are not transmitted in plain text and are therefore less susceptible to interception. However, it should be noted that Base64 encoding is not a form of encryption and can be easily decoded. Therefore, it is important to use additional security measures when handling sensitive information.

## See Also
- [Java.net API documentation for HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Base64 encoding in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-base64/)
- [Official documentation for basic authentication in HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)