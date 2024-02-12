---
title:                "Envoi d'une requête HTTP"
aliases: - /fr/java/sending-an-http-request.md
date:                  2024-01-20T17:59:56.760946-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

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
