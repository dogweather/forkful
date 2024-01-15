---
title:                "Sending an http request"
html_title:           "Java recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests is an essential component of web development. It allows us to communicate with servers and retrieve data, making it an integral part of building dynamic and interactive websites.

## How To

Sending an HTTP request in Java can be done using the HttpURLConnection class. Here's an example of how to use it to send a GET request to a URL:

```Java
// Import the necessary packages
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

// Create a URL object with the desired URL
URL url = new URL("https://example.com");

// Open a connection to the URL
HttpURLConnection con = (HttpURLConnection) url.openConnection();
con.setRequestMethod("GET"); // Set the request method to GET

// Read the response from the server
BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
String inputLine;
StringBuilder response = new StringBuilder();
while ((inputLine = in.readLine()) != null) {
    response.append(inputLine);
}
in.close();

// Print the response
System.out.println(response.toString());
```

The above code makes use of the HttpURLConnection class to establish a connection, set the request method to GET, and read the response from the server. The response is then printed to the console. 

## Deep Dive

The HttpURLConnection class is a sub-class of the abstract class URLConnection, which provides a framework for working with URLs. It is part of the java.net package and is used for both client and server-side communication. The class provides a range of methods for sending different types of HTTP requests, such as GET, POST, PUT, DELETE, HEAD, OPTIONS, and TRACE. It also allows you to set request headers, set timeouts, and handle redirects.

Another useful class for sending HTTP requests in Java is the HttpClient class from the Apache HttpComponents library. It provides a more user-friendly and modern API for sending HTTP requests and supports features such as authentication, cookies, and caching.

When sending an HTTP request, it's essential to handle potential errors and exceptions that may occur. This includes connection timeouts, server errors, and network issues. It's also crucial to observe proper security practices, such as using HTTPS instead of HTTP for sensitive data.

## See Also
- [Oracle's HttpURLConnection class documentation](https://docs.oracle.com/javase/10/docs/api/java/net/HttpURLConnection.html)
- [Apache HttpComponents library documentation](https://hc.apache.org/)
- [Mozilla's HTTP request methods reference](https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods)