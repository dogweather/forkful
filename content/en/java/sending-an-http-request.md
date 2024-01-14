---
title:                "Java recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why
In the world of web development, sending HTTP requests is a fundamental practice. Whether it is retrieving data from a server or updating information, understanding how to send an HTTP request is essential for any programmer.

## How To
Sending an HTTP request in Java is a relatively straightforward process. First, we need to import the necessary classes from the Java.net package.

```Java
import java.net.HttpURLConnection;
import java.net.URL;
```

Next, we need to create an instance of the URL class and pass in the API endpoint as a String.

```Java
URL url = new URL("https://example.com/api");
```

We can then open a connection to this URL using the openConnection() method and casting it to a HttpURLConnection.

```Java
HttpURLConnection con = (HttpURLConnection) url.openConnection();
```

With the connection now established, we can set the request method, headers, and any necessary parameters.

```Java
con.setRequestMethod("GET");
con.setRequestProperty("Content-Type", "application/json");
con.setDoOutput(true); //only if we want to send parameters
```

Once all our settings are in place, we can finally send the request using the connect() and getResponseCode() methods.

```Java
con.connect();
int responseCode = con.getResponseCode();
```

The response code will tell us if the request was successful or if there were any errors. We can also read the response body using the getInputStream() method and process the data accordingly.

## Deep Dive
Behind the scenes, sending an HTTP request involves creating a TCP/IP socket connection, formatting the request using the correct protocols, and handling any errors or redirects that may occur. It is crucial to have a basic understanding of the HTTP protocol and its various request methods (GET, POST, PUT, DELETE) when working with HTTP requests.

Some other useful classes and methods in the Java.net package include URLConnection, URLDecoder and URLEncoder for handling URLs with special characters, and HttpsURLConnection for secure HTTPS connections.

## See Also
- [Oracle Java Network Programming Tutorial](https://docs.oracle.com/javase/tutorial/networking/urls/index.html)
- [How to Send HTTP Request in Java](https://www.baeldung.com/java-http-request)
- [Understanding HTTP Basics](https://www.digitalocean.com/community/tutorials/http-the-protocol-every-web-developer-must-know-part-1)