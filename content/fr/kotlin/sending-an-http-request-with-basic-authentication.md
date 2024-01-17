---
title:                "Envoyer une requête http avec une authentification de base"
html_title:           "Kotlin: Envoyer une requête http avec une authentification de base"
simple_title:         "Envoyer une requête http avec une authentification de base"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi?
Le fait d'envoyer une requête HTTP avec une authentification basique consiste à ajouter des informations d'identification à la requête afin d'accéder à certaines ressources protégées sur le web. Les programmeurs utilisent cette méthode pour sécuriser leurs applications et s'assurer que seules les personnes autorisées peuvent y accéder.

## Comment faire:
Voici un exemple de code en Kotlin qui illustre comment envoyer une requête HTTP avec une authentification basique et afficher la réponse obtenue :

```Kotlin
val username = "john" // Remplacer par votre nom d'utilisateur
val password = "12345" // Remplacer par votre mot de passe
val url = URL("https://www.example.com/api/users") // Remplacer par votre URL

val connection = url.openConnection() as HttpURLConnection
connection.requestMethod = "GET"
connection.setRequestProperty("Authorization", "Basic " + Base64.getEncoder().encodeToString("$username:$password".toByteArray()))
val response = connection.inputStream.bufferedReader().readText()
println(response) // Affiche la réponse obtenue
```

## Plongée en profondeur:
L'authentification basique est l'une des méthodes les plus anciennes pour sécuriser les connexions sur le web. Elle est définie dans le protocole HTTP et utilise une combinaison de nom d'utilisateur et de mot de passe pour vérifier l'identité de l'utilisateur. Bien que cette méthode soit facile à mettre en place, elle présente des vulnérabilités et est généralement remplacée par des méthodes d'authentification plus sécurisées comme OAuth.

Les alternatives à l'authentification basique incluent l'utilisation de méthodes d'authentification par token, où un token est généré et utilisé pour valider l'identité de l'utilisateur, ou l'utilisation d'un serveur d'authentification centralisé tel que Keycloak.

Dans l'exemple de code ci-dessus, nous utilisons l'encodage Base64 pour sécuriser les informations d'identification envoyées dans la requête, mais cela ne garantit pas la sécurité totale de celles-ci. Il est important de prendre en compte les risques de sécurité lors de l'utilisation de l'authentification basique et de mettre en place des mesures de protection supplémentaires si nécessaire.

## Voir aussi:
- Documentation officielle de Kotlin : https://kotlinlang.org/
- Guide de référence sur l'envoi de requêtes HTTP avec Kotlin : https://www.baeldung.com/kotlin-http-request