---
date: 2024-01-20 18:02:01.895683-07:00
description: "Sending an HTTP request with basic authentication involves adding a\
  \ header with a username and password to access a protected resource. Programmers\
  \ use it\u2026"
lastmod: '2024-03-13T22:44:59.972195-06:00'
model: gpt-4-1106-preview
summary: "Sending an HTTP request with basic authentication involves adding a header\
  \ with a username and password to access a protected resource. Programmers use it\u2026"
title: Sending an HTTP request with basic authentication
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request with basic authentication involves adding a header with a username and password to access a protected resource. Programmers use it for simple authorization in web services when more advanced methods aren't necessary.

## How to:
Java makes it pretty straightforward to send HTTP requests with basic authentication using the `HttpURLConnection` class. Here's a quick example:

```java
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

public class BasicAuthRequest {

    public static void main(String[] args) {
        try {
            URL url = new URL("http://example.com/resource");
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            
            String userCredentials = "user:password";
            String basicAuth = "Basic " + new String(Base64.getEncoder().encode(userCredentials.getBytes(StandardCharsets.UTF_8)));
            connection.setRequestProperty("Authorization", basicAuth);

            int responseCode = connection.getResponseCode();
            System.out.println("Response Code: " + responseCode);

            if (responseCode == HttpURLConnection.HTTP_OK) {
                BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                String inputLine;
                StringBuilder response = new StringBuilder();

                while ((inputLine = in.readLine()) != null) {
                    response.append(inputLine);
                }
                in.close();

                System.out.println(response.toString());
            } else {
                System.out.println("GET request not worked");
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
Sample Output:
```
Response Code: 200
{ "message": "This is a response from a protected resource!" }
```

## Deep Dive
Basic authentication has been around since the early days of HTTP. It works by transmitting base64-encoded credentials in the header, making it simple but not very secure without HTTPS, as credentials can be easily decoded. 

Alternatives like OAuth add another layer of security by using tokens instead. Token-based authentication is preferred nowadays, particularly for RESTful APIs.

When implementing basic access authentication in Java, the recommended way since Java 11 is using the new `HttpClient` class. It's more versatile and supports HTTP/2 out of the box. Still, for basic requirements or legacy systems, `HttpURLConnection` remains a viable option.

## See Also
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [Oracle Java 11 HTTP Client API Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [Baeldung guide on Java HTTP requests](https://www.baeldung.com/java-http-request)
