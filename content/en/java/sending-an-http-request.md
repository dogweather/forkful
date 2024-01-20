---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request is a way of asking a server to send back data. Developers use this to access APIs, fetch data, or interact with other web services.

## How to:
Here's how to send a GET request to a server, using Java's `HttpClient`.

```Java
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.URI;

public class Main {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                    .uri(new URI("https://example.com"))
                    .GET()
                    .build();

        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

        System.out.println(response.body());
    }
}
```
In this code:
1. We create a `HttpClient`.
2. Next, we build an `HttpRequest` specifying the URL and the request method (GET).
3. Then, we send our request with `HttpResponse.BodyHandlers.ofString()` to get the body as a `String`.
4. At last, we print out the response body.

## Deep Dive
*Historical context*: Before Java 11, sending HTTP requests involved more hands-on work, dealing with `HttpURLConnection`. Java 11's `HttpClient` offers a more convenient API.

*Alternatives*: Apache HttpClient and OkHttp are alternatives to Java's built-in HttpClient, offering more customization options.

*Implementation details*: The `newHttpClient()` uses a builder pattern, so you can customize more if needed. The built HttpRequest and HttpResponse allow for different handling of request/response bodies - for file download, for example, you can specify it to be stored in a file directly with `HttpResponse.BodyHandlers.ofFile(Paths.get("file.txt"))`.

## See Also
1. [Oracle's `Java.net.http` package documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/package-summary.html)
2. [Baeldung's guide to HttpClient](https://www.baeldung.com/httpclient-guide)