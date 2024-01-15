---
title:                "Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
html_title:           "Kotlin: Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Senden einer HTTP-Anfrage mit grundlegender Authentifizierung"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum

Warum sollte man sich überhaupt mit dem Senden von HTTP-Anfragen mit Basic Authentication beschäftigen? Nun, in der heutigen Zeit ist der Austausch von Daten zwischen verschiedenen Systemen und Anwendungen unerlässlich. Oftmals ist diese Kommunikation jedoch nicht öffentlich zugänglich und erfordert daher eine Authentifizierung, um sicherzustellen, dass nur autorisierte Benutzer auf die Daten zugreifen können. Das Senden von HTTP-Anfragen mit Basic Authentication ist eine der einfachsten Methoden, um dies zu erreichen.

## How To

Um eine HTTP-Anfrage mit Basic Authentication zu senden, benötigen wir zunächst ein HTTP-Client in Kotlin. Zum Glück gibt es in der Standardbibliothek von Kotlin eine klasse HTTPClient, die dies für uns erledigen kann. Wir müssen nur noch die erforderlichen Importe hinzufügen und eine Instanz des Clients erstellen.

```
import java.net.HttpURLConnection
import java.net.URL
import java.io.BufferedReader
import java.io.InputStreamReader

fun main() {

    // URL der Anfrage
    val url = URL("https://example.com/api")

    // Erstellen einer Instanz des HTTP-Clients
    val http = url.openConnection() as HttpURLConnection

    // Festlegen des Authentifizierungs-Headers mit Benutzername und Passwort
    val user = "username"
    val password = "password"
    val credentials = "$user:$password"
    val authHeaderValue = "Basic " + Base64.getEncoder().encodeToString(credentials.toByteArray())
    http.setRequestProperty("Authorization", authHeaderValue)

    // Durchführen der Anfrage und Auslesen der Antwort
    val inputStream = http.getInputStream()
    val bufferedReader = BufferedReader(InputStreamReader(inputStream))
    val response = bufferedReader.readLine()

    // Ausgabe der Antwort
    println(response)

    // Schließen der Verbindung
    http.disconnect()
}
```

Das obige Beispiel zeigt, wie man eine HTTP-Anfrage mit Basic Authentication in Kotlin senden kann. Zunächst erstellen wir eine Instanz des HTTP-Clients mithilfe der `openConnection()`-Methode. Dann legen wir den Authentifizierungs-Header mit Benutzername und Passwort fest, indem wir die Base64-Codierung verwenden. Schließlich führen wir die Anfrage durch und lesen die Antwort aus.

## Deep Dive

Bei der Verwendung von Basic Authentification in einer HTTP-Anfrage müssen wir darauf achten, dass unsere Benutzerdaten im Header nicht unverschlüsselt übertragen werden. Aus diesem Grund verwenden wir hier die Base64-Codierung, um unsere Daten zu verschlüsseln. Es ist wichtig zu beachten, dass Base64 keine sichere Verschlüsselung ist und daher sollte diese Methode nicht für sensible Daten verwendet werden. Es ist ratsam, die Verwendung von HTTPS zu empfehlen, um sicherzustellen, dass die Daten sicher übertragen werden.

## Siehe auch

Um mehr über HTTP-Anfragen mit Basic Authentication in Kotlin zu erfahren, schauen Sie sich diese Links an:

- [Offizielle Dokumentation von Kotlin](https://kotlinlang.org/docs/reference/http-client.html)
- [Einführung in die HTTP-Authentifizierung](https://www.keycdn.com/support/http-basic-authentication)
- [Tutorial über HTTP-Anfragen mit Basic Authentication in Java](https://www.baeldung.com/java-http-request)