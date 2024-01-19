---
title:                "Envoyer une requête http"
html_title:           "Bash: Envoyer une requête http"
simple_title:         "Envoyer une requête http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## C'est quoi et pourquoi?

Envoyer une demande HTTP, c'est comme demander une page web spécifique à un serveur. Ça permet aux programmeurs de récupérer des données, gérer des APIs, même automatiser des tâches.

## Comment faire :

Pour envoyer une demande HTTP avec Kotlin, nous utilisons ktor, une bibliothèque Kotlin. Voici un exemple rapide :

```kotlin
import io.ktor.client.*
import io.ktor.client.request.*

suspend fun main() {
    val client = HttpClient()
    
    client.use {
        val result: String = client.get("https://example.com")
    
        println(result)
    }
}
```

En exécutant ce code, vous obtiendrez la source HTML de "https://example.com".

## Plongée en détail

Ktor est relativement récent (2018) mais gagne en popularité chez les programmeurs Kotlin. Alternativement, OkHttp et Fuel sont aussi populaires pour les demandes HTTP. 

Côté implantation, Ktor est non bloquant par défaut, ce qui est intéressant pour des opérations gourmandes en I/O comme les demandes HTTP. Il utilise coroutines pour accomplir cela, une solution légère pour la gestion de la simultanéité Kotlin.

## Voir aussi :

Pour en savoir plus, visitez les documents suivants :

- [ktor.io](https://ktor.io/) (documentation officielle)
- [OkHttp](https://square.github.io/okhttp/)
- [Fuel](https://github.com/kittinunf/fuel)

Voilà, vous avez maintenant les bases pour faire des requêtes HTTP avec Kotlin !