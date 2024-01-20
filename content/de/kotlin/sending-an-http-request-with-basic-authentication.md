---
title:                "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
html_title:           "Bash: Eine HTTP-Anfrage mit Basisauthentifizierung senden"
simple_title:         "Eine HTTP-Anfrage mit Basisauthentifizierung senden"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# In Deutsch Senden einer HTTP-Anfrage mit Basic-Authentifizierung in Kotlin

## Was & Warum?
HTTP-Basisauthentifizierung ist ein Vorgang, bei dem Benutzername und Passwort in der HTTP-Header einer Anforderung gesendet werden, um auf geschützte Ressourcen zuzugreifen. Programmierer tun dies, um Dienste zu nutzen, die eine Autorisierung erfordern, wie Web-APIs.

## Anleitung:
Machen wir eine HTTP-Anforderung mit Basisauthentifizierung in Kotlin mit Ktor, einem asynchronen I/O-Framework von JetBrains. 

### Installation:
Fügen Sie zuerst die Ktor-Client-Bibliothek zu Ihrer `build.gradle.kts` Datei hinzu:

```kotlin
dependencies {
    implementation("io.ktor:ktor-client-core:1.6.7")
    implementation("io.ktor:ktor-client-cio:1.6.7")
    implementation("io.ktor:ktor-client-auth:1.6.7")
}
```
Aktualisieren Sie die Versionen entsprechend Ihrem Projektbedarf.

### Code:
Erstellen Sie den HttpClient mit BasicAuth und senden Sie die Anforderung:

```kotlin
import io.ktor.client.HttpClient
import io.ktor.client.features.auth.Auth
import io.ktor.client.features.auth.providers.basic
import io.ktor.client.request.get
import io.ktor.client.request.header

suspend fun main() {
    val client = HttpClient {
    install(Auth) {
        basic {
            sendWithoutRequest = true
            credentials {
                BasicAuthCredentials(username = "Benutzername", password = "Passwort")
            }
        }
    }
}

val response: String = client.get("https://example.com") {
    header("Accept", "application/json")
}
println(response)
client.close()
}
```

Der obige Code sendet einen GET Request mit BasicAuth.

## Tief Tauchen:
Historisch gesehen war Basic-Auth die erste Art der HTTP-Authentifizierung, die in RFC 1945 dokumentiert und schließlich in RFC 2617 formalisiert wurde. 

Beachten Sie, dass bei der HTTP-Basisauthentifizierung Benutzername und Passwort im Klartext (basierend auf Base64) gesendet werden, was bedeutet, dass sie für einen Man-in-the-Middle-Angriff anfällig sind. Daher wird diese Methode nur mit einer HTTPS-Verbindung empfohlen.

Alternativen zur Basisauthentifizierung sind Bearer-Token, OAuth und Sessions. Diese alternativen Ansätze bieten in der Regel bessere Sicherheit und mehr Features. 

In der Ktor-Implementierung macht `sendWithoutRequest = true` die Bibliothek proaktiv. Anstatt auf einen `401 Unauthorized` zu warten, sendet es die Anmeldeinformationen direkt.

## Siehe Auch:
* [Ktor Docs on HTTP Client](https://ktor.io/docs/http-client-engines.html)
* [Ktor Auth Feature](https://ktor.io/docs/basic.html)
* [RFC 7617 - Grundlegende HTTP-Authentifizierung](https://tools.ietf.org/html/rfc7617)