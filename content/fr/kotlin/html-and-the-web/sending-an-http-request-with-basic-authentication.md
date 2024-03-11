---
date: 2024-01-20 18:02:01.754372-07:00
description: "Une requ\xEAte HTTP avec authentification de base envoie des informations\
  \ d'identification cod\xE9es en base64. Les programmeurs l'utilisent pour acc\xE9\
  der \xE0 des\u2026"
lastmod: '2024-03-11T00:14:31.687337-06:00'
model: gpt-4-1106-preview
summary: "Une requ\xEAte HTTP avec authentification de base envoie des informations\
  \ d'identification cod\xE9es en base64. Les programmeurs l'utilisent pour acc\xE9\
  der \xE0 des\u2026"
title: "Envoi d'une requ\xEAte HTTP avec authentification de base"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Une requête HTTP avec authentification de base envoie des informations d'identification codées en base64. Les programmeurs l'utilisent pour accéder à des ressources web sécurisées simplement.

## Comment faire :
```kotlin
import java.net.HttpURLConnection
import java.net.URL
import java.util.Base64

fun sendGetRequestWithBasicAuth(url: String, username: String, password: String) {
    val connection = URL(url).openConnection() as HttpURLConnection
    val credentials = "$username:$password"
    val encodedCredentials = Base64.getEncoder().encodeToString(credentials.toByteArray())

    connection.requestMethod = "GET"
    connection.setRequestProperty("Authorization", "Basic $encodedCredentials")

    val responseCode = connection.responseCode
    println("Réponse Code : $responseCode")

    if (responseCode == HttpURLConnection.HTTP_OK) {
        connection.inputStream.bufferedReader().use {
            it.lines().forEach { line -> println(line) }
        }
    } else {
        println("Échec de la requête : $responseCode")
    }
}

// Utilisation
val url = "https://votre.api/ressource"
val username = "votre_utilisateur"
val password = "votre_mot_de_passe"
sendGetRequestWithBasicAuth(url, username, password)
```

## Exploration Approfondie :
Historiquement, l'authentification de base HTTP est une des méthodes les plus simples d'authentification. Elle n'est pas la plus sécurisée car les identifiants sont facilement décodables si interceptés. C'est pourquoi HTTPS est souvent utilisé en conjonction. Des alternatives comme OAuth sont préférées pour une sécurité accrue. En Kotlin, l'envoi de requête HTTP avec authentification de base peut s'effectuer manuellement comme vu précédemment, ou en utilisant des bibliothèques telles que OkHttp ou Ktor, qui simplifient le processus et gèrent mieux les erreurs.

## Voir Aussi :

- Documentation sur l'authentification HTTP dans la RFC 7617: https://tools.ietf.org/html/rfc7617
- Guide sur OkHttp: https://square.github.io/okhttp/
- Documentation de Ktor pour les clients HTTP: https://ktor.io/docs/http-client.html
