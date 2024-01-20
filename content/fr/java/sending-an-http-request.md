---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

L'envoi d’une requête HTTP, c'est demander des données à un serveur via le protocole HTTP. Les programmeurs l'utilisent pour interagir avec des services Web, récupérer des données, etc.

## Comment faire:

**Importation de bibliothèques nécessaires**

```Java
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.URI;
```

**Créer une requête HTTP GET**

```Java
HttpClient client = HttpClient.newHttpClient();
HttpRequest request = HttpRequest.newBuilder()
      .uri(URI.create("http://example.com"))
      .build();
```

**Envoyer la requête et obtenir la réponse**

```Java
HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
System.out.println(response.body());
```

Lors de l'exécution de ce code, vous devriez voir le contenu HTML de "http://example.com" dans votre console.

## Plongée Profonde

Historiquement, les requêtes HTTP étaient réalisées en Java à l'aide de `HttpURLConnection`. Cependant, depuis Java 11, le `HttpClient` plus moderne et complet est le mode privilégié.

En termes d'alternatives, vous pouvez également utiliser des bibliothèques tiers comme OkHttp ou Apache HttpClient. Chacune a ses propres avantages et inconvénients en termes de performance, de flexibilité et de facilité d'utilisation.

En ce qui concerne les détails de mise en œuvre, `HttpClient` offre un paradigme à la fois synchrone (comme dans l'exemple montré) et asynchrone (utilisant `CompletableFuture`) pour gérer les requêtes HTTP. Il prend également en charge HTTP/2 et WebSocket.

## Voir Aussi

- [Java - Envoi de demandes HTTP](https://www.baeldung.com/java-http-request)
- [API HttpClient (Java 11 et plus récent)](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)