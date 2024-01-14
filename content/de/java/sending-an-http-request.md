---
title:                "Java: Senden einer http-Anfrage"
simple_title:         "Senden einer http-Anfrage"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Warum

Das Senden von HTTP-Anfragen ist ein wichtiger Bestandteil der modernen Webentwicklung. Durch das Senden von HTTP-Anfragen können wir Daten von anderen Servern abrufen und in unsere Anwendung integrieren. Mit der Verwendung von Java können wir diese Anfragen effizient und einfach implementieren.

## Wie geht das

Die Verwendung von Java zum Senden von HTTP-Anfragen ist relativ einfach. Wir müssen zunächst die entsprechenden Bibliotheken importieren, indem wir folgende Zeile zu unserem Code hinzufügen:
```Java
import java.net.HttpURLConnection;
```
Dann können wir eine Verbindung zu einer URL herstellen und eine HTTP-Anfrage senden, wie im folgenden Beispielcode gezeigt:
```Java
// URL der API
String url = "http://www.example.com/api";

// Eine HTTP-Verbindung herstellen
HttpURLConnection conn = (HttpURLConnection) new URL(url).openConnection();

// Art der Anfrage angeben
conn.setRequestMethod("GET");

// Antwortcode erhalten
int responseCode = conn.getResponseCode();

// Daten lesen und ausgeben
BufferedReader in = new BufferedReader(new InputStreamReader(conn.getInputStream()));
String inputLine;
while ((inputLine = in.readLine()) != null) {
    System.out.println(inputLine);
}
in.close();
```
Dieser Code zeigt, wie wir eine einfache HTTP-GET-Anfrage stellen und die Daten aus der API ausgeben können. Natürlich können wir auch andere Arten von Anfragen wie POST oder PUT senden und mit den erhaltenen Daten weiterarbeiten.

## Tiefere Einblicke

Beim Senden von HTTP-Anfragen ist es wichtig, auf den Antwortcode zu achten, um sicherzustellen, dass die Anfrage erfolgreich war. Außerdem können wir verschiedene Header-Parameter hinzufügen, um zusätzliche Informationen an die Anfrage anzuhängen. Wir können auch die Verbindung schließen, sobald wir die Daten erhalten haben, um Ressourcen zu sparen. Es gibt viele verschiedene Möglichkeiten, wie wir HTTP-Anfragen in Java nutzen können, und es lohnt sich, sich tiefer damit zu befassen, um sie effektiv zu nutzen.

## Siehe auch

- [HTTP-Anfragen in Java - Ein Leitfaden für Anfänger](https://www.javatpoint.com/httpurlconnection-in-java)
- [Die offizielle Java-Dokumentation zur Verwendung von HTTP-Anfragen](https://docs.oracle.com/javase/tutorial/networking/urls/readingWriting.html)
- [Einführung in RESTful Web Services in Java](https://www.tutorialspoint.com/java/java_web_services.htm)