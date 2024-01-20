---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?

HTTP-Anfragen mit Basic-Authentifizierung sind eine typische Methode, um sich gegenüber einer API oder einem Webdienst auszuweisen, indem Benutzername und Passwort in der Header-Anfrage gesendet werden. Entwickler nutzen diese Methode, um den Zugang zu bestimmten Ressourcen zu kontrollieren und die Datensicherheit zu gewährleisten.

## Wie es geht:

Wir verwenden die Java Standardbibliothek `java.net`. Hier ist ein kurzes Code-Beispiel:

```Java
import java.net.URL;
import java.net.HttpURLConnection;
import java.util.Base64;

public class Main {
    public static void main(String[] args) throws Exception {
        URL url = new URL("http://www.example.com");
        String encoding = Base64.getEncoder().encodeToString(("username:password").getBytes());

        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestMethod("POST");
        connection.setDoOutput(true);
        connection.setRequestProperty("Authorization", "Basic " + encoding);
        System.out.println(connection.getResponseCode());
    }
}
```
Die Ausgabe dieses Codebeispiels sollte der HTTP-Antwortcode sein. Bei erfolgreicher Authentifizierung sollte `200` zurückgegeben werden.

## Vertiefung:

Die Basic-Authentifizierung war eine der ersten Authentifizierungsmethoden, die ins Internetprotokoll eingeführt wurde. Sie ist einfach zu implementieren, bietet aber nur eine minimale Sicherheitsstufe.

Alternativen zur Basic-Authentifizierung sind beispielsweise OAuth, JWT oder API-Schlüssel. Diese Methoden bieten stärkere Sicherheiten, sind aber auch komplexer in der Implementierung.

Bei der Implementierung der Basic-Authentifizierung ist die Kodierung wichtig. Das Benutzerpasswort muss in Base64 kodiert sein und im Header als Zeichenkette im Format `username:password` gesendet werden.

## Siehe auch:

Wenn du tiefer in die HTTP-Authentifizierung eintauchen möchtest, empfehle ich folgende Quellen:

- [Java Docs - HttpURLConnection](https://docs.oracle.com/en/java/javase/14/docs/api/java.base/java/net/HttpURLConnection.html)
- [MDN Docs - HTTP authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [RFC 7617 - The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)

Hinweis: Behandle deine Anmeldeinformationen und anderen vertraulichen Informationen immer mit Vorsicht, um Sicherheitslücken zu vermeiden. Wähle die Authentifizierungsmethode basierend auf den Anforderungen und der Sicherheitsstufe deines Projekts.