---
date: 2024-01-20 17:59:56.760946-07:00
description: "Envoyer une requ\xEAte HTTP, c'est demander \xE0 un serveur web de faire\
  \ quelque chose pour vous, comme r\xE9cup\xE9rer une page web. Les d\xE9veloppeurs\
  \ le font pour\u2026"
lastmod: '2024-03-13T22:44:57.635887-06:00'
model: gpt-4-1106-preview
summary: "Envoyer une requ\xEAte HTTP, c'est demander \xE0 un serveur web de faire\
  \ quelque chose pour vous, comme r\xE9cup\xE9rer une page web. Les d\xE9veloppeurs\
  \ le font pour\u2026"
title: "Envoi d'une requ\xEAte HTTP"
weight: 44
---

## What & Why?
Envoyer une requête HTTP, c'est demander à un serveur web de faire quelque chose pour vous, comme récupérer une page web. Les développeurs le font pour interagir avec des APIs, récupérer des données, soumettre des formulaires, et bien plus.

## How to:
Avec Java 11, c’est devenu très fluide. Vous pouvez utiliser le client HTTP intégré pour envoyer des requêtes et recevoir des réponses facilement. Voici comment faire :

```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class HttpExample {
    public static void main(String[] args) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("https://jsonplaceholder.typicode.com/todos/1"))
                .build();

        client.sendAsync(request, HttpResponse.BodyHandlers.ofString())
                .thenApply(HttpResponse::body)
                .thenAccept(System.out::println)
                .join();
    }
}
```

Sortie:
```json
{
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

## Deep Dive:
Avant Java 11, envoyer des requêtes HTTP était assez lourdeau. Il fallait jongler avec `HttpURLConnection` ou des bibliothèques tierces comme Apache HttpClient ou OkHttp. Mais dès Java 11, le client HTTP natif a facilité les choses avec une API plus intuitive.

Alternativement, des frameworks comme Spring ou des langages de script HTTP comme cURL existent toujours pour des cas d'usage spécifiques ou des préférences personnelles.

En termes de détails d'implémentation, il est important de savoir que le client HTTP de Java supporte HTTP/1.1 et HTTP/2, et qu'il peut gérer de manière asynchrone les requêtes grâce aux `CompletableFuture`.

## See Also:
- [La documentation officielle de HttpClient de Java 11](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [Guide sur les requêtes HTTP avec Java](https://www.baeldung.com/java-9-http-client)
