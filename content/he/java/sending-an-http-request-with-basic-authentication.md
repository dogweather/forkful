---
title:                "שליחת בקשת HTTP עם אימות בסיסי"
html_title:           "Java: שליחת בקשת HTTP עם אימות בסיסי"
simple_title:         "שליחת בקשת HTTP עם אימות בסיסי"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Basic authentication is a way for programmers to secure their HTTP requests by requiring a username and password to access a web resource. It involves sending additional authentication headers along with the request to the server.

## What & Why?

Sending an HTTP request with basic authentication means including the username and password in the header of the request. This allows the server to verify the user's credentials before granting access. Programmers use basic authentication to ensure the security of their requests and to prevent unauthorized access to sensitive data.

## How to:

To send an HTTP request with basic authentication in Java, you can use the `HttpURLConnection` class. First, you need to create a `URL` object with the desired URL. Then, open a connection to the URL using `HttpURLConnection` and set the `Authorization` header with the username and password encoded in Base64.

```Java
URL url = new URL("https://example.com/api/resource");
HttpURLConnection connection = (HttpURLConnection) url.openConnection();
String encodedCredentials = Base64.getEncoder().encodeToString("username:password".getBytes());
connection.setRequestProperty("Authorization", "Basic " + encodedCredentials);
```

## Deep Dive

Basic authentication has been around since the early days of the internet and is still widely used today. However, it is not considered a secure method of authentication as the username and password are transmitted in plain text, making it vulnerable to attacks. Alternatives such as OAuth and JSON Web Tokens (JWTs) are now more commonly used for securing HTTP requests.

The implementation details of basic authentication may vary depending on the server and client implementation. The server needs to be able to decode the Base64 encoded credentials and validate them against a database or user directory. On the client-side, the developer needs to ensure that the username and password are properly encoded and included in the request header.

## See Also

- [Java HttpURLConnection class](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Base64 encoding in Java](https://docs.oracle.com/javase/8/docs/api/java/util/Base64.html)
- [OAuth and its use in API authentication](https://www.oauth.com/)