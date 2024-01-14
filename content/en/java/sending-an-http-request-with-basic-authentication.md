---
title:                "Java recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

Sending an HTTP request with basic authentication is a common practice in web development. This type of authentication allows users to access restricted resources by providing a username and password in the request headers. It adds an extra layer of security to web applications, making it a crucial aspect of modern web development.

## How To

To send an HTTP request with basic authentication in Java, we first need to create an instance of the `URL` class with the URL of the resource we want to access. Then, we can use the `openConnection()` method to open a connection to the URL. Finally, we can set the `Authorization` header on the connection using the `setRequestProperty()` method.

Let's take a look at a simple code example:

```Java
// Creating a URL object
URL url = new URL("https://example.com/api/resource");

// Opening a connection
HttpURLConnection connection = (HttpURLConnection) url.openConnection();

// Setting basic authentication header
String username = "user123";
String password = "pass456";
String auth = username + ":" + password;
byte[] encodedAuth = Base64.getEncoder().encode(auth.getBytes());
String authHeaderValue = "Basic " + new String(encodedAuth);
connection.setRequestProperty("Authorization", authHeaderValue);

// Sending the request
connection.setRequestMethod("GET");
int responseCode = connection.getResponseCode();
System.out.println("Response code: " + responseCode);
```

The output of the above code will be the response code of the request, which will indicate whether the request was successful or not.

## Deep Dive

When sending an HTTP request with basic authentication, the `Authorization` header is formed by combining the username and password, separating them with a colon (`:`), and encoding them using Base64. This means that the username and password are not transmitted in plain-text, adding a layer of security to the request.

It's important to note that basic authentication is not the most secure method of authentication, as the encoded credentials can be easily deciphered. Therefore, it's recommended to use other forms of authentication, such as OAuth, for sensitive information.

## See Also

- [Java URL class](https://docs.oracle.com/javase/8/docs/api/java/net/URL.html)
- [HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Basic access authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)