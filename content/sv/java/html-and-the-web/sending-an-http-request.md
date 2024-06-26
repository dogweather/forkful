---
date: 2024-01-20 18:00:00.820896-07:00
description: "Hur g\xF6r man: Java's `HttpClient` g\xF6r det enkelt att skicka HTTP-beg\xE4\
  ran. H\xE4r \xE4r ett snabbt exempel f\xF6r att skicka en GET-beg\xE4ran och skriva\
  \ ut svaret."
lastmod: '2024-03-13T22:44:37.784339-06:00'
model: gpt-4-1106-preview
summary: "Java's `HttpClient` g\xF6r det enkelt att skicka HTTP-beg\xE4ran."
title: "Skicka en http-f\xF6rfr\xE5gan"
weight: 44
---

## Hur gör man:
Java's `HttpClient` gör det enkelt att skicka HTTP-begäran. Här är ett snabbt exempel för att skicka en GET-begäran och skriva ut svaret:

```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class SimpleHttpClient {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                .uri(new URI("http://example.com"))
                .GET()
                .build();
        
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

        System.out.println(response.statusCode());
        System.out.println(response.body());
    }
}
```

Kör koden och få svaret:

```
200
<!doctype html>...
```

## Djupdykning:
HTTP-begäran har funnits sedan webbens födelse, först specificerad i 1991. Nu används HTTP/2 och HTTP/3 för snabbare prestanda. Alternativ till Javas `HttpClient` inkluderar bibliotek som `Apache HttpClient` och `OkHttp`. Dessa erbjuder fler funktioner, men Java's inbyggda klient är tillräcklig för enkel användning. Implementeringsdetaljer är viktiga; ställ in timeouts, hantera olika HTTP-metoder, hantera omdirigeringar, och kontrollera svar för att bygga robusta applikationer.

## Se även:
- Oracle officiella dokumentation på `HttpClient`: [https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- Mozilla Developer Network på HTTP: [https://developer.mozilla.org/en-US/docs/Web/HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)
