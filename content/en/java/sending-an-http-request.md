---
title:                "Sending an HTTP request"
aliases:
- en/java/sending-an-http-request.md
date:                  2024-01-20T17:59:42.892380-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request means asking a server for data or action, like pulling up a webpage or sending a form. Programmers do it to interact with web services, APIs, and to make their apps play nice with others on the internet.

## How to:

Let's roll with Java 11's `HttpClient`, `HttpRequest`, and `HttpResponse` to shoot a GET request and snag some data:

```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class HttpRequestExample {
    public static void main(String[] args) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                              .uri(URI.create("http://example.com"))
                              .build();

        client.sendAsync(request, HttpResponse.BodyHandlers.ofString())
              .thenApply(HttpResponse::body)
              .thenAccept(System.out::println)
              .join();
    }
}
```

You run it, and voilà—server response, right in your console.

## Deep Dive

Before Java 11, sending an HTTP request was a more complex dance that often involved third-party libraries like Apache HttpClient. `HttpURLConnection` was also an option but felt like a dinosaur—cumbersome and less intuitive.

With Java 11, `HttpClient` steps in, streamlining the process with both synchronous `.send` and asynchronous `.sendAsync` methods. It's reactive and non-blocking—meaning you're not waiting around tapping your foot while it does its thing. This aligns with modern app efficiency needs where waiting equals wasted time.

Alternatives to Java's standard libraries? Libraries like OkHttp and Retrofit are still favorites when robust features and custom configurations are desired. And why not? They come with their own perks, like connection pooling and call conversion out of the box.

## See Also

Dive deeper into the Java HttpClient with the official Java docs:
- [HttpClient](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [HttpRequest](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpRequest.html)
- [HttpResponse](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpResponse.html)

Feeling adventurous? Explore OkHttp and Retrofit:
- [OkHttp](https://square.github.io/okhttp/)
- [Retrofit](https://square.github.io/retrofit/)
