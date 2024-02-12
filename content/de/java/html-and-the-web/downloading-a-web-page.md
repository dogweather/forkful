---
title:                "Webseite herunterladen"
aliases:
- /de/java/downloading-a-web-page.md
date:                  2024-01-20T17:44:17.562035-07:00
model:                 gpt-4-1106-preview
simple_title:         "Webseite herunterladen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Herunterladen einer Webseite bedeutet, ihre Daten zu erfassen und lokal zu speichern. Programmierer tun dies, um Inhalte zu analysieren, zu archivieren oder offline zu verarbeiten.

## How to:
Hier ist ein einfacher Weg, um mit Java eine Webseite herunterzuladen:

```java
import java.io.*;
import java.net.URL;

public class WebPageDownloader {
    public static void main(String[] args) {
        String webPageUrl = "http://example.com";
        String outputPath = "downloaded_page.html";

        try (InputStream inputStream = new URL(webPageUrl).openStream();
             FileOutputStream outputStream = new FileOutputStream(outputPath)) {
             
            inputStream.transferTo(outputStream);
            System.out.println("Webseite wurde erfolgreich heruntergeladen!");
        } catch (IOException e) {
            System.out.println("Fehler beim Herunterladen der Webseite: " + e.getMessage());
        }
    }
}
```
Ausgabe:
```
Webseite wurde erfolgreich heruntergeladen!
```

## Deep Dive
Das Herunterladen von Webseiten ist nicht neu. Früher nutzte man Tools wie `wget` oder `curl`. Programmiersprachen haben dann eigene Methoden entwickelt, um dieses Vorgehen zu vereinfachen. 

In Java gibt es verschiedene Wege, um eine Webseite herunterzuladen. Die obige Methode verwendet `java.net.URL` und `java.io.InputStream`, was einfach und direkt ist. Aber man könnte auch `HttpClient` aus Java 11 verwenden, der eine modernere und flexiblere API bietet:

```java
HttpClient client = HttpClient.newHttpClient();
HttpRequest request = HttpRequest.newBuilder()
        .uri(URI.create("http://example.com"))
        .build();

client.sendAsync(request, HttpResponse.BodyHandlers.ofFile(Paths.get("downloaded_page.html")))
        .thenApply(HttpResponse::body)
        .thenAccept(System.out::println)
        .join();
```

Egal, welche Methode man wählt, Fehlerbehandlung ist wichtig. Verbindungsfehler, Zeitüberschreitungen und fehlerhafte URLs sind häufige Probleme.

## See Also
- [Java HttpURLConnection Documentation](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Java HttpClient Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [Apache HttpComponents for more complex tasks](https://hc.apache.org/)
- [jsoup for HTML parsing](https://jsoup.org/)
