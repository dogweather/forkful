---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Arduino: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Envoyer une requête HTTP avec une authentification de base, c'est comme présenter une carte d'identité à un serveur web pour accéder à des ressources. Les programmeurs le font pour protéger les données sensibles et contrôler l'accès.

## Comment faire:

Pour ce faire avec Kotlin, on peut utiliser la bibliothèque `khttp`. Voici un exemple:

```Kotlin
import khttp.get

fun main() {
    val username = "monNom"
    val password = "monMotDePasse"

    val response = get(
        url = "https://mon-serveur.com",
        auth = khttp.structures.BasicAuthorization(username, password)
    )
    
    println(response.statusCode)
    println(response.text)
}
```

Dans cet exemple, `monNom` et `monMotDePasse` sont vos identifiants. Remplacez `https://mon-serveur.com` avec l'URL de votre serveur. Exécutez ce programme et il imprimera le code de statut de la réponse HTTP et le corps du texte de la réponse.

## Plongée en Profondeur

L'authentification HTTP de base a été officiellement définie en 1999 dans la spécification HTTP 1.1. Malgré sa facilité d'utilisation, la sécurité est un souci majeur, car les identifiants sont tranmis en clair. En règle général, utilisez-le sur HTTPS pour une sécurité accrue. 

En dehors de `khttp`, il existe aussi d'autres bibliothèques pour gérer les requêtes HTTP avec Kotlin, comme `Fuel`, `OkHttp` et `ktor`.

Pour aller plus loin, vous pouvez utiliser des méthodes d'authentification plus avancées comme OAuth, qui offre une meilleure sécurité par token.

## Voir Aussi

1. Kotlin HTTP Request - [khttp](https://github.com/asofterspace/khttp)
2. Alternatives to khttp - [Fuel](https://github.com/kittinunf/Fuel), [OkHttp](https://square.github.io/okhttp/), [ktor](https://ktor.io/)
3. Basic and Digest Access Authentication - [RFC2617](https://tools.ietf.org/html/rfc2617)