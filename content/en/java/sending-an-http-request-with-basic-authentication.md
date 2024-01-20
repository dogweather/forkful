---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request with basic authentication is a method of accessing web resources that requires credentials - a username and password. Its primary use is to protect sensitive data by making sure requests are legitimate, hence preventing unauthorized access.

## How to:

Here's how to send an HTTP request with basic authentication in Java using the `HttpURLConnection` class.

```java
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Base64;

public class Main {
    public static void main(String[] args) throws Exception {
        String userCredentials = "username:password";
        String basicAuth = "Basic " + new String(Base64.getEncoder().encode(userCredentials.getBytes()));

        URL url = new URL("http://example.com");
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();

        connection.setRequestProperty("Authorization", basicAuth);
        connection.setRequestMethod("GET");
        connection.setDoOutput(true);

        InputStream content = (InputStream)connection.getInputStream();
        BufferedReader in = new BufferedReader(new InputStreamReader(content));

        in.lines().forEach(System.out::println);
    }
}
```

This code sends a GET request to `http://example.com` using a username and password set in `userCredentials`. The server's response is then printed line by line.

## Deep Dive

The basic HTTP authentication we're using here is as old as the internet. First defined in 1999 by the Internet Engineering Task Force, it's made to be simple and fast for resources that don't need high-level security.

One alternative is Digest Authentication, which is a tad more secure as it involves hashing and doesn't send passwords in plaintext. However, it's slower due to the added complexity.

The `Base64.getEncoder().encode(userCredentials.getBytes())` line in the code means we're not sending our login details in plaintext. The encoder transforms the text to a format that can be reliably sent over networks.

Also, it's worth noting the downside of basic authentication: It's not as safe for high-security needs since the encoded username and password can be decoded quite easily. That's why for very secure needs, it's best to use stronger methods like OAuth, which also handles permissions between applications.

## See Also

- The Java `HttpURLConnection` Documentation: [https://docs.oracle.com/en/java/javase/11/docs/api/java.net/java/net/HttpURLConnection.html](https://docs.oracle.com/en/java/javase/11/docs/api/java.net/java/net/HttpURLConnection.html)
- Overview of HTTP Authentication Schemes: [https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- More authentication options (OAuth, Bearer JWT, etc.): [https://auth0.com/docs/authorization/flows](https://auth0.com/docs/authorization/flows)