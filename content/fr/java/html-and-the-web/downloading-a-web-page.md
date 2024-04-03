---
date: 2024-01-20 17:44:18.791304-07:00
description: "How to: En Java, `java.net.http.HttpClient` est notre ami pour t\xE9\
  l\xE9charger une page. Voici un exemple simple ."
lastmod: '2024-03-13T22:44:57.638555-06:00'
model: gpt-4-1106-preview
summary: "En Java, `java.net.http.HttpClient` est notre ami pour t\xE9l\xE9charger\
  \ une page."
title: "T\xE9l\xE9chargement d'une page web"
weight: 42
---

## How to:
En Java, `java.net.http.HttpClient` est notre ami pour télécharger une page. Voici un exemple simple :

```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class DownloadPage {

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

Si tout est bon, vous aurez le contenu de `http://example.com` dans la console.

## Deep Dive
Avant `HttpClient`, on avait `HttpURLConnection`, moins flexible et plus verbeux. Pourquoi utiliser `HttpClient`? Il supporte HTTP/2, gère mieux les connexions et le code est plus clair.

Pour les gros sites, attention aux politiques de robots (fichier `robots.txt`) : certaines parties ne doivent pas être téléchargées automatiquement.

Si vous devez télécharger régulièrement ou massivement, pensez à respecter le serveur – attendez entre les requêtes, gérez les erreurs sans spammer le serveur.

## See Also
Pour aller plus loin :

- Documentation officielle de `HttpClient`: [https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- Guide sur les expressions régulières en Java pour analyser votre contenu: [https://docs.oracle.com/javase/tutorial/essential/regex/](https://docs.oracle.com/javase/tutorial/essential/regex/)
- Détails sur le fichier `robots.txt`: [https://developers.google.com/search/docs/advanced/robots/intro](https://developers.google.com/search/docs/advanced/robots/intro)
- Une introduction à JSoup pour parser du HTML en Java: [https://jsoup.org/](https://jsoup.org/)
