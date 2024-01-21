---
title:                "Envoi d'une requête HTTP"
date:                  2024-01-20T18:00:12.137575-07:00
model:                 gpt-4-1106-preview
simple_title:         "Envoi d'une requête HTTP"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Envoyer une requête HTTP, c'est demander des données à un serveur web. Les programmeurs en ont besoin pour les applications qui bouffent de l'info en ligne - pensez apps météo, news, ou réseaux sociaux.

## How to:
En Kotlin, utilisez la bibliothèque `ktor` pour simplifier les requêtes HTTP :

```kotlin
import io.ktor.client.*
import io.ktor.client.engine.cio.*
import io.ktor.client.request.*
import io.ktor.client.statement.*

suspend fun fetchUrl(url: String): HttpResponse {
    val client = HttpClient(CIO)
    val response: HttpResponse = client.get(url)
    client.close()
    return response
}

// Utilisez ça dans une coroutine, genre comme ça :
// val response = fetchUrl("http://example.com")
// println(response.readText())
```

Ce code va chercher le contenu de `http://example.com`. L'output ? Le HTML de la page.

## Deep Dive
Avant `ktor`, on avait pas mal d'autres choix en Kotlin, du genre Apache HttpClient ou OkHttp. Mais `ktor` est plus récent, conçu spécialement pour Kotlin et les coroutines. Cool, parce qu'il simplifie la vie avec sa gestion d'async.

Les requêtes HTTP sont au coeur du web depuis Tim Berners-Lee en a profité pour partager des docs au CERN dans les années 90. Maintenant, elles sont partout, pour tout et n'importe quoi touchant le web.

Le truc avec les requêtes HTTP, c'est de bien gérer le réseau. Ça peut être lent, ça peut foirer. Gérez les timeouts, les erreurs réseau, et assurez-vous de fermer les connexions.

## See Also
- Ktor docs pour plus de détails : [https://ktor.io/docs/](https://ktor.io/docs/)
- Kotlin coroutines guide, ça aide pour l'async : [https://kotlinlang.org/docs/coroutines-guide.html](https://kotlinlang.org/docs/coroutines-guide.html)
- Pour la comparaison, voici OkHttp : [https://square.github.io/okhttp/](https://square.github.io/okhttp/)