---
title:                "HTTP-Anfragen mit Basisauthentifizierung senden"
aliases:
- /de/java/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:57.132684-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-Anfragen mit Basisauthentifizierung senden"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
Das Senden einer HTTP-Anfrage mit Basic Authentication beinhaltet das Übermitteln von Benutzername und Passwort in einem Base64-codierten Header. Programmierer nutzen dies für den sicheren Zugriff auf geschützte Ressourcen auf einem Server.

## So geht's:
```java
import java.net.Authenticator;
import java.net.PasswordAuthentication;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Base64;

public class BasicAuthRequest {

    public static void main(String[] args) {
        String url = "http://example.com/api";
        String user = "meinBenutzer";
        String password = "meinPasswort";

        try {
            URL urlObj = new URL(url);
            HttpURLConnection connection = (HttpURLConnection) urlObj.openConnection();
            String basicAuthPayload = "Basic " + Base64.getEncoder().encodeToString((user + ":" + password).getBytes());

            connection.setRequestMethod("GET");
            connection.setRequestProperty("Authorization", basicAuthPayload);

            int responseCode = connection.getResponseCode();
            System.out.println("Antwort-Code: " + responseCode);
            // Verarbeiten der Serverantwort hier...

        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```
Sample Output:
```
Antwort-Code: 200
```

## Deep Dive
Vor der weiten Verbreitung von OAuth und anderen Authentifizierungsmethoden war die Basic Authentication ein Standardverfahren zum Schutz von Webressourcen. Obwohl einfach zu implementieren, ist sie wegen der leichten Entschlüsselbarkeit der Credentials nicht die sicherste Methode. HTTPS ist obligatorisch, um die Credentials zu schützen.

Alternativen zu Basic Authentication sind Digest Access Authentication, OAuth, und API-Schlüssel. In modernen Anwendungen wird Basic Authentication zunehmend durch token-basierte Authentifizierungsmechanismen wie OAuth 2.0 abgelöst, die eine höhere Sicherheit bieten.

Die `java.net`-Bibliothek ermöglicht einfachen HTTP-Zugriff, aber für komplexere Anfragen oder eine bessere Handhabung von HTTP-Verbindungen kann die Verwendung von Bibliotheken wie Apache HttpClient oder OkHttp nützlich sein.

## Siehe Auch
- RFC 7617, The 'Basic' HTTP Authentication Scheme: https://tools.ietf.org/html/rfc7617
- Java™ SE Documentation: https://docs.oracle.com/en/java/javase/
- Apache HttpClient: https://hc.apache.org/httpcomponents-client-5.1.x/index.html
- OkHttp Library: https://square.github.io/okhttp/
