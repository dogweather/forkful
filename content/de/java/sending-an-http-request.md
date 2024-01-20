---
title:                "Eine HTTP-Anforderung senden"
html_title:           "Bash: Eine HTTP-Anforderung senden"
simple_title:         "Eine HTTP-Anforderung senden"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Was & Warum?

Ein HTTP-Anfrage (oder HTTP-Request) ist eine Anforderung, die ein Client an einen Server sendet, um Daten zu erhalten oder zu senden. Programmierer nutzen sie, um Informationen von Webseiten zu extrahieren, APIs zu nutzen oder Web-Anwendungen zu erstellen.

## Wie geht das?

Mit Java 16 können wir dies durch die `HttpClient`, `HttpRequest` und `HttpResponse` Klassen tun. Hier ist wie:

```Java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class Main {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();

        HttpRequest request = HttpRequest.newBuilder()
                .uri(new URI("http://example.com"))
                .build();

        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());

        System.out.println(response.statusCode());
        System.out.println(response.body());
    }
}
```

Dieses Beispiel sendet eine GET-Anfrage an "http://example.com" und druckt den Statuscode und den Körper der Antwort in die Konsole. 

## Vertiefung

Die oben gezeigten Klassen (`HttpClient`, `HttpRequest`, und `HttpResponse`) wurden in Java 9 eingeführt, um die altmodische `HttpURLConnection` zu ersetzen. Die neue API ist viel flexibler und einfacher zu benutzen.

Ein alternativer Ansatz wäre die Verwendung von Bibliotheken wie OkHttp oder Apache HttpClient, die unter bestimmten Umständen mehr Funktionen bieten können.

Die Methode `HttpClient.newHttpClient()` erstellt einen neuen HttpClient mit Standardkonfiguration. Für spezielle Anforderungen, wie Proxys oder Authentifizierung, können wir einen `HttpClient.Builder` verwenden und diesen konfigurieren.

## Siehe Auch

1. [Offizielle Java-Dokumentation für HttpClient](https://docs.oracle.com/en/java/javase/16/docs/api/java.net.http/java/net/http/HttpClient.html)
3. [OkHttp Library](https://square.github.io/okhttp/)