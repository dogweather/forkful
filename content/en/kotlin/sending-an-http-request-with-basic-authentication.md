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

## What & Why?

Sending an HTTP request with basic authentication is the process of adding login credentials to the header of an HTTP request. This is often done by programmers to authenticate and authorize users to access specific resources on a server.

## How to:

To send an HTTP request with basic authentication in Kotlin, you can use the `URL` and `HttpURLConnection` classes. First, create a `URL` object with the desired URL, then open a connection using `.openConnection()` and cast it to `HttpURLConnection`. Next, set the request method to `GET` and add the authorization header using the `setRequestProperty` method. Finally, read the response by using the `getInputStream` method and printing it to the console.

```
val url = URL("https://example.com/api/resource")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"
connection.setRequestProperty("Authorization", "Basic base64EncodedCredentials")
val response = connection.inputStream.bufferedReader().readText()
println(response)
```

Sample output: `{"message": "Success!"}`

## Deep Dive:

Basic authentication has been around since the early days of the internet and is one of the simplest forms of authentication. It works by sending login credentials in plain text, which can be a security concern. Alternative methods, such as OAuth, have been developed to address this issue. One thing to note is that with basic authentication, the credentials are not encrypted, so it's important to only use it over HTTPS connections.

When implementing basic authentication, the credentials should be base64 encoded for added security. This is because base64 encoding is not an encryption method, but it does obscure the credentials from being seen in plain text.

## See Also:

- Official Kotlin documentation for `URL` and `HttpURLConnection` classes: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-u-r-l/ and https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-http-u-r-l-connection/
- More information about basic authentication and its vulnerabilities: https://www.owasp.org/index.php/Basic_Authentication
- Alternative authorization methods, such as OAuth: https://oauth.net/2/