---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:44.909930-07:00
description: "Een HTTP-verzoek versturen betekent een server vragen om gegevens of\
  \ acties, zoals het ophalen van een webpagina of het versturen van een formulier.\u2026"
lastmod: '2024-03-13T22:44:50.678460-06:00'
model: gpt-4-0125-preview
summary: "Een HTTP-verzoek versturen betekent een server vragen om gegevens of acties,\
  \ zoals het ophalen van een webpagina of het versturen van een formulier.\u2026"
title: Een HTTP-verzoek verzenden
---

{{< edit_this_page >}}

## Wat & Waarom?

Een HTTP-verzoek versturen betekent een server vragen om gegevens of acties, zoals het ophalen van een webpagina of het versturen van een formulier. Programmeurs doen dit om te interageren met webdiensten, API's, en om hun apps goed te laten samenwerken met anderen op het internet.

## Hoe:

Laten we aan de slag gaan met Java 11's `HttpClient`, `HttpRequest`, en `HttpResponse` om een GET-verzoek te doen en wat gegevens te bemachtigen:

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

Je draait het, en voilà—serverrespons, direct in je console.

## Diepere Duik

Voor Java 11 was het versturen van een HTTP-verzoek een complexere dans die vaak betrokkenheid van bibliotheken van derden zoals Apache HttpClient vereiste. `HttpURLConnection` was ook een optie maar voelde aan als een dinosaurus—omslachtig en minder intuïtief.

Met Java 11 komt `HttpClient`, die het proces stroomlijnt met zowel synchrone `.send` als asynchrone `.sendAsync` methoden. Het is reactief en niet-blokkerend—wat betekent dat je niet rondloopt te tappen met je voet terwijl het zijn ding doet. Dit sluit aan bij de efficiëntiebehoeften van moderne apps waar wachten gelijk staat aan verspilde tijd.

Alternatieven voor Java's standaardbibliotheken? Bibliotheken zoals OkHttp en Retrofit zijn nog steeds favorieten wanneer robuuste functies en aangepaste configuraties gewenst zijn. En waarom niet? Ze komen met hun eigen voordelen, zoals verbinding pooling en gespreksconversie uit de doos.

## Zie Ook

Duik dieper in de Java HttpClient met de officiële Java-documentatie:
- [HttpClient](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [HttpRequest](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpRequest.html)
- [HttpResponse](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpResponse.html)

Voel je avontuurlijk? Verken OkHttp en Retrofit:
- [OkHttp](https://square.github.io/okhttp/)
- [Retrofit](https://square.github.io/retrofit/)
