---
date: 2024-01-20 17:59:51.506210-07:00
description: "Hvordan gj\xF8re det: Her er en kjapp kodesnutt for \xE5 sende en GET-foresp\xF8\
  rsel og skrive ut responsen med Java 11 `HttpClient`."
lastmod: '2024-03-13T22:44:40.663463-06:00'
model: gpt-4-1106-preview
summary: "Her er en kjapp kodesnutt for \xE5 sende en GET-foresp\xF8rsel og skrive\
  \ ut responsen med Java 11 `HttpClient`."
title: "\xC5 sende en HTTP-foresp\xF8rsel"
weight: 44
---

## Hvordan gjøre det:
Her er en kjapp kodesnutt for å sende en GET-forespørsel og skrive ut responsen med Java 11 `HttpClient`:

```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class HttpExample {
    public static void main(String[] args) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("https://httpbin.org/get"))
                .build();

        client.sendAsync(request, HttpResponse.BodyHandlers.ofString())
                .thenApply(HttpResponse::body)
                .thenAccept(System.out::println)
                .join();
    }
}
```

Kjører du dette, vil du se utdata som:

```
{
  "args": {}, 
  "headers": {
    "Accept-Encoding": "gzip,deflate",
    ...
  }, 
  "origin": "xxx.xxx.xxx.xxx", 
  "url": "https://httpbin.org/get"
}
```

## Dypdykk
Før Java 11, var det `HttpURLConnection` som gjaldt. Det var greit nok, men Java 11 introduserte `HttpClient`, som er lettere å bruke og mer moderne. 

Alternativer til Java-standardbiblioteket inkluderer biblioteker som Apache HttpClient, OkHttp, og Retrofit, alle som tilbyr mer funksjonalitet og fleksibilitet. 

Når du sender en HTTP-forespørsel, bygger `HttpClient` en request som kan være av type GET, POST, og andre HTTP-metoder. Dette går over nettverket til en server, som behandler forespørselen og sender tilbake et svar.

## Se Også
For mer om Java `HttpClient`:

- [Offisiell Java 11 HttpClient dokumentasjon](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- For biblioteker med ekstra kraft, sjekk ut:
  - [Apache HttpClient](https://hc.apache.org/httpcomponents-client-5.1.x/index.html)
  - [OkHttp](https://square.github.io/okhttp/)
  - [Retrofit](https://square.github.io/retrofit/)
