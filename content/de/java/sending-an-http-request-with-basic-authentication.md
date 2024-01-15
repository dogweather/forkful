---
title:                "Eine http Anfrage mit grundlegender Authentifizierung senden"
html_title:           "Java: Eine http Anfrage mit grundlegender Authentifizierung senden"
simple_title:         "Eine http Anfrage mit grundlegender Authentifizierung senden"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Es gibt viele Anwendungsfälle, in denen es notwendig ist, eine HTTP-Anfrage mit Basisauthentifizierung zu senden. Zum Beispiel kann dies erforderlich sein, um auf eine geschützte Ressource zuzugreifen oder um Daten von einer API abzurufen, die eine Authentifizierung erfordert. Durch die Verwendung von Basisauthentifizierung wird sichergestellt, dass nur autorisierte Benutzer Zugriff auf die Ressource haben.

## Wie man es macht

Um eine HTTP-Anfrage mit Basisauthentifizierung in Java zu senden, können Sie die `java.net.HttpURLConnection` Klasse verwenden. Zunächst müssen Sie eine Verbindung zu der URL herstellen, an die Sie die Anfrage senden möchten. Dies kann beispielsweise mit der Methode `openConnection()` erfolgen. Anschließend müssen Sie die Art der Anfrage, z.B. GET oder POST, mit der Methode `setRequestMethod()` festlegen und die erforderlichen Header mit `setRequestProperty()` hinzufügen. Schließlich können Sie die Verbindung mit `connect()` herstellen und Ihre Anfrage mit `getOutputStream()` oder `getInputStream()` ausführen, je nachdem ob Sie Daten an den Server senden oder empfangen möchten.

```Java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

public class BasicAuthExample {

    public static void main(String[] args) {
        // URL to send the request to
        String url = "https://example.com/api/resource";

        // Encode the username and password in base64
        String username = "myUsername";
        String password = "myPassword";
        String authString = username + ":" + password;
        String authStringEnc = Base64.getEncoder().encodeToString(authString.getBytes(StandardCharsets.UTF_8));

        try {
            // Open connection to the URL
            URL requestUrl = new URL(url);
            HttpURLConnection connection = (HttpURLConnection) requestUrl.openConnection();

            // Set request method to GET
            connection.setRequestMethod("GET");

            // Add Authorization header with base64 encoded credentials
            connection.setRequestProperty("Authorization", "Basic " + authStringEnc);

            // Establish connection
            connection.connect();

            // Read response from server
            InputStream is = connection.getInputStream();
            BufferedReader br = new BufferedReader(new InputStreamReader(is));
            String response = "";
            String line;
            while ((line = br.readLine()) != null) {
                response += line;
            }

            // Print response from server
            System.out.println(response);

            // Close connection
            connection.disconnect();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Ausgabe

Die Ausgabe der obigen Beispielanfrage kann je nach API und Implementierung variieren. In der Regel sollte die Antwort jedoch in Form von Text oder JSON-Daten zurückgegeben werden.

## Tief eintauchen

Bei der Verwendung von Basisauthentifizierung ist es wichtig zu beachten, dass die Benutzerinformationen durch Base64-Kodierung zwar verschlüsselt, aber nicht sicher sind. Eine sicherere Alternative wäre die Verwendung von Digest-Authentifizierung oder HTTPS-Verschlüsselung. Außerdem sollten die Zugangsdaten nicht im Quellcode gespeichert werden, sondern beispielsweise in einer Konfigurationsdatei oder als Umgebungsvariable.

## Siehe auch

- [Oracle Java documentation about HTTP requests](https://docs.oracle.com/javase/tutorial/networking/urls/readingURL.html)
- [Wikipedia: Basic access authentication](https://en.wikipedia.org/wiki/Basic_access_authentication)
- [Java Code Examples for java.net.HttpURLConnection](https://www.programcreek.com/java-api-examples/?api=java.net.HttpURLConnection)