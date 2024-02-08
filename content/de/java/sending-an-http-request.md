---
title:                "Einen HTTP-Request senden"
date:                  2024-01-20T18:00:07.247634-07:00
model:                 gpt-4-1106-preview
simple_title:         "Einen HTTP-Request senden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen sind der Weg, über den Programme im Internet kommunizieren. Programmierer senden sie, um Daten zu holen, zu senden, zu ändern oder zu löschen - die Grundbausteine der Interaktion mit Webdiensten.

## So geht's:
Hier ist ein einfaches Beispiel, wie man eine GET-Anfrage in Java sendet, um den Inhalt einer Webseite zu holen:

```java
import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class HttpGetRequest {

    public static void main(String[] args) {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create("http://example.com"))
                .build();

        try {
            HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
            System.out.println(response.statusCode());
            System.out.println(response.body());
        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }
    }
}
```
Sample Output:
```
200
<!doctype html>...
```

## Deep Dive:
HTTP-Anfragen gehen zurück bis zur Entstehung des Webs in den 1990ern. Sie folgen einem Request-Response-Modell. Java bietet mehrere Möglichkeiten, HTTP-Anfragen zu senden – `HttpURLConnection`, externe Bibliotheken wie Apache HttpClient, und ab Java 11 gibt's die `java.net.http` API, die modern und einfach zu bedienen ist.

Während `HttpURLConnection` Basisfunktionalität bietet, ermöglicht die `java.net.http` API mehr Kontrolle und Asynchronität. Die Alternativen dazu, wie OkHttpClient oder Apache HttpClient, bieten eigene Vorteile, wie etwa komplexere Konfigurationen und Unterstützung für ältere Java-Versionen.

Tiefer gehend, für POST-Anfragen kann man `HttpRequest.BodyPublishers` verwenden, um Content mitzusenden. Für Asynchronität bietet `HttpClient` die Funktion `sendAsync()` an, welche mit `CompletableFuture` arbeitet.

## Siehe Auch:
- [Java 11 HTTP/2 API Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [Baeldung on Java HTTP Requests](https://www.baeldung.com/java-9-http-client)
- [Oracle tutorial on HttpURLConnection](https://docs.oracle.com/javase/tutorial/networking/urls/readingWriting.html)
- [OkHttp’s GitHub Repository](https://github.com/square/okhttp)
