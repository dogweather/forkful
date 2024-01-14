---
title:                "Java: Eine http-Anfrage mit grundlegender Authentifizierung senden"
simple_title:         "Eine http-Anfrage mit grundlegender Authentifizierung senden"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Das Senden von HTTP-Anfragen mit grundlegender Authentifizierung ist eine Schlüsselfunktion für das Interagieren mit Webanwendungen. Durch die Verwendung von Benutzername und Kennwort können die Benutzer überprüfen, ob sie berechtigt sind, die Ressourcen auf der Website zu nutzen, und sicherstellen, dass ihre Daten geschützt sind.

## How To

Um eine HTTP-Anfrage mit grundlegender Authentifizierung in Java zu senden, folgen Sie den folgenden Schritten:

1. Importieren Sie die `java.net` Bibliothek für die Verwendung von `HttpURLConnection`.
2. Erstellen Sie ein `URL`-Objekt mit der Ziel-URL.
3. Erstellen Sie ein `HttpURLConnection`-Objekt mit `url.openConnection()`.
4. Legen Sie die Anfragemethode auf `GET`, `POST` oder `PUT` fest, je nach Bedarf.
5. Fügen Sie den Autorisierungsheader mit `conn.setRequestProperty("Authorization","Basic <Base64-kodierter Benutzername:Kennwort>")`.
6. Senden Sie die Anfrage mit `conn.connect()`.
7. Lesen Sie die Antwort mit `conn.getInputStream()` und `BufferedReader`.
8. Verarbeiten Sie die Antwortdaten entsprechend.

Ein Beispielcode zur Demonstration:

```Java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Base64;

public class BasicAuthHTTPRequest {
    public static void main(String[] args) throws IOException {
        // Ziel-URL definieren
        URL url = new URL("https://www.beispielwebsite.com/api/resourcen");

        // Verbindung aufbauen
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();

        // Anfragemethode setzen
        conn.setRequestMethod("GET");

        // Autorisierungsheader hinzufügen
        String userCredentials = "Benutzername:Kennwort";
        String basicAuth = "Basic " + new String(Base64.getEncoder().encode(userCredentials.getBytes()));
        conn.setRequestProperty ("Authorization", basicAuth);

        // Anfrage senden
        conn.connect();

        // Antwortdaten lesen und verarbeiten
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(conn.getInputStream()))) {
            String line;
            while ((line = reader.readLine()) != null) {
                System.out.println(line);
            }
        }
        conn.disconnect();
    }
}
```

Beispieloutput:

```
{"id": 1, "name": "Max Mustermann"}
```

## Deep Dive

Beim Senden einer HTTP-Anfrage mit grundlegender Authentifizierung müssen Benutzer den Benutzernamen und das Kennwort mithilfe von Base64-Verschlüsselung kodieren und im Autorisierungsheader angeben. Dies bietet eine grundlegende Absicherung der Daten, ist jedoch möglicherweise nicht ausreichend für hochsensible Informationen. Daher ist es wichtig, zusätzliche Sicherheitsmaßnahmen wie HTTPS zu implementieren, um eine sichere Datenübertragung zu gewährleisten.

## Siehe auch

- [Offizielle Oracle Java Dokumentation - HttpURLConnection](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
- [Baeldung Tutorial: Basic and Digest Access Authentication](https://www.baeldung.com/java-http-request-basic-digest-authentication)