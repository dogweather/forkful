---
title:                "Å sende en HTTP-forespørsel"
aliases:
- /no/java/sending-an-http-request.md
date:                  2024-01-20T17:59:51.506210-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
I Java sender vi HTTP-forespørsler for å snakke med websider og tjenester. Det er essensielt for å hente data, sende informasjon og integrere med et utall av APIer på nett.

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
