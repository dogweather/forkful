---
title:                "Sending an http request with basic authentication"
html_title:           "Java recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication is a way to securely access a web resource that requires authentication. Programmers use it to send a request to a server with a username and password, which is then checked by the server to grant access to the desired resource.

## How to:

To send an HTTP request with basic authentication in Java, you first need to create an instance of the URL class with your desired web address. Then, you can create a HttpURLConnection object using the URL's openConnection() method. Next, set the request method to "GET" and add the authentication information to the request header using the setRequestProperty() method. Finally, you can use the getResponseCode() method to retrieve the server's response and access the requested resource.

```Java
URL url = new URL("https://example.com/resource");
HttpURLConnection connection = (HttpURLConnection) url.openConnection();
connection.setRequestMethod("GET");
connection.setRequestProperty("Authorization", "Basic " + 
    Base64.getEncoder().encodeToString("username:password".getBytes()));
int responseCode = connection.getResponseCode();
// access the resource using the connection's input stream
```

Upon running the code, you'll receive a response code indicating the success or failure of the request, along with the desired resource if the request was successful.

## Deep Dive:

In the early days of the internet, authentication was not common, as most websites and resources were publicly accessible. However, with the rise of online transactions and the need to protect sensitive information, websites started implementing basic authentication as a means of ensuring only authorized users could access certain content.

While basic authentication is a simple and easy way to secure a web resource, it has some limitations. For one, it is not the most secure method, as the username and password are transmitted in plain text, making it vulnerable to interception. As a result, other forms of authentication, such as OAuth, have become more popular.

To implement basic authentication, the username and password are typically encoded using Base64 encoding and added to the request header. It is important to note that this is not a form of encryption and should not be used as a security measure to store or transmit sensitive information.

## See Also:

- [Java API documentation for URL class](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/net/URL.html)
- [Java API documentation for HttpURLConnection class](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/net/HttpURLConnection.html)
- [Base64 encoding in Java](https://www.baeldung.com/java-base64-encode-and-decode)