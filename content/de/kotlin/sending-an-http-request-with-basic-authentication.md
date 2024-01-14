---
title:                "Kotlin: Versenden einer http-Anfrage mit grundlegender Authentifizierung"
simple_title:         "Versenden einer http-Anfrage mit grundlegender Authentifizierung"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Warum
Wenn Sie eine App oder eine Website erstellen, die mit einem Server kommunizieren muss, ist es oft notwendig, eine HTTP-Anfrage zu senden. Mit der Basisauthentifizierung können Sie diese Anfragen sicherer gestalten, indem Sie Benutzernamen und Passwörter verwenden, um den Zugriff auf Ihre Ressourcen zu kontrollieren.

## Wie es geht
Um eine HTTP-Anfrage mit Basisauthentifizierung in Kotlin zu senden, müssen Sie zuerst die benötigten Bibliotheken importieren:
```
Kotlin import java.net.HttpURLConnection
import java.net.URL
import java.util.Base64
```

Als nächstes müssen Sie eine URL-Verbindung zu Ihrem Server herstellen und die Basisauthentifizierung konfigurieren:
```
Kotlin val url = URL("https://example.com/api/resource")
val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"

val username = "your_username"
val password = "your_password"
val authString = "$username:$password"
val encodedAuth = Base64.getEncoder().encodeToString(authString.toByteArray())
val authHeaderValue = "Basic $encodedAuth"

connection.setRequestProperty("Authorization", authHeaderValue)
```

Dann können Sie die Anfrage senden und die Antwort abrufen:
```
Kotlin val responseCode = connection.responseCode
val responseMessage = connection.responseMessage
println("Response Code: $responseCode")
println("Response Message: $responseMessage")
```

Wenn alles erfolgreich war, erhalten Sie eine Antwort wie diese:
```
Output Response Code: 200
Response Message: OK
```

## Tief eintauchen
Ein paar Dinge zu beachten, wenn Sie eine HTTP-Anfrage mit Basisauthentifizierung senden:

- Stellen Sie sicher, dass Sie eine sichere Verbindung verwenden (HTTPS), um Ihre Anfrage und die übertragenen Anmeldeinformationen zu schützen.
- Verwenden Sie starke und eindeutige Anmeldeinformationen, um die Sicherheit Ihrer Ressourcen zu gewährleisten.
- Eine gute Praxis ist es, die Anmeldeinformationen in einem sicheren Speicher zu speichern und sie nur zur Laufzeit abzurufen.

## Siehe auch
- [Offizielle Kotlin-Dokumentation zu HTTP-Anfragen](https://kotlinlang.org/docs/reference/networking.html)
- [Tutorial zu HTTP-Anfragen in Kotlin von Baeldung](https://www.baeldung.com/kotlin/http-request)
- [Codebeispiele für HTTP-Anfragen mit Kotlin von Code Examples](https://www.code-examples.com/http-request-kotlin/)