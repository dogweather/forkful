---
title:                "HTTP-Anfragen mit Basisauthentifizierung senden"
date:                  2024-01-20T18:01:59.469239-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-Anfragen mit Basisauthentifizierung senden"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Was & Warum?
HTTP-Anfragen mit Basic Authentication senden Nutzernamen und Passwort im Base64-codierten Format im `Authorization`-Header, um sich gegenüber einem Service zu authentifizieren. Programmierer nutzen das, um geschützte Ressourcen über eine API abzurufen oder zu manipulieren.

## So geht's:
```kotlin
import java.net.HttpURLConnection
import java.net.URL
import java.util.Base64

fun main() {
    val url = URL("https://deine-api.de/datensatz")
    val username = "nutzername"
    val password = "passwort"

    val connection = url.openConnection() as HttpURLConnection
    val encodedCredentials = Base64.getEncoder().encodeToString("$username:$password".toByteArray())
    connection.setRequestProperty("Authorization", "Basic $encodedCredentials")

    connection.apply {
        requestMethod = "GET"
        inputStream.bufferedReader().use {
            it.lines().forEach { line -> println(line) }
        }
    }
}
```
Output (abhängig von der API und den Daten):
```
{
  "id": 1,
  "name": "Beispielname",
  "beschreibung": "Das ist ein einfacher Datensatz."
}
```

## Deep Dive
Basic Authentication ist ein Teil des HTTP/1.0-Standards und wurde wegen seiner Einfachheit in vielen frühen Webanwendungen eingesetzt. Es basiert auf der Annahme, dass die Verbindung zwischen Client und Server sicher ist, was heute durch HTTPS gewährleistet wird. Alles andere wäre unsicher, da Base64 leicht zu entschlüsseln ist.

Es gibt Alternativen wie Digest Access Authentication, OAuth und moderne Token-basierte Verfahren wie JWT (JSON Web Tokens), die sicherer sind. Basic Authentication sollte daher nur über HTTPS und besser noch nur als Teil einer mehrschichtigen Sicherheitsstrategie verwendet werden.

Beim Senden einer HTTP-Anfrage in Kotlin können auch Bibliotheken wie OkHttp oder Retrofit verwendet werden, die das Handling komplexer Anfragen und verschiedener Authentifizierungsformen vereinfachen.

## Siehe Auch
- [HTTP Authentication: Basic and Digest Access Authentication (RFC 7617)](https://tools.ietf.org/html/rfc7617)
- [OkHttp Library](https://square.github.io/okhttp/)
- [Retrofit Library](https://square.github.io/retrofit/)
