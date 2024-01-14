---
title:                "Kotlin: Envoyer une requête HTTP"
simple_title:         "Envoyer une requête HTTP"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Pourquoi
Les requêtes HTTP sont un élément essentiel de la programmation moderne. Elles permettent aux applications de communiquer avec des serveurs à travers le web et d'échanger des données. Apprenez comment les utiliser correctement pour tirer le meilleur parti de vos applications !

## Comment faire
Pour effectuer une requête HTTP en Kotlin, vous pouvez utiliser une librairie externe telle que OkHttp ou utiliser les fonctions natives fournies par le langage. Voici un exemple de code utilisant les fonctions natives pour effectuer une requête GET et afficher le résultat dans la console :

```Kotlin
val url = "https://api.github.com/users/username"
val response = URL(url).readText()
println(response)
```

Cela enverra une requête à l'API GitHub pour récupérer les informations du profil de l'utilisateur donné et affichera le résultat dans la console. Il est également possible de spécifier des paramètres et des en-têtes dans la requête pour des fonctionnalités plus avancées.

## Plongée en profondeur
Lors de l'envoi d'une requête HTTP, il est important de comprendre les différents éléments qui la composent. Une requête est généralement composée d'une URL, d'une méthode (GET, POST, PUT, DELETE), d'éventuels paramètres et de corps de données. Le serveur répondra ensuite avec un code de statut et un résultat, généralement au format JSON ou XML.

Pour une plus grande flexibilité et une meilleure gestion des erreurs, il est recommandé d'utiliser une librairie externe telle que Retrofit ou Volley pour gérer les requêtes HTTP dans vos applications Kotlin.

# Voir aussi
- Tutoriel sur l'utilisation de Retroft en Kotlin : https://futurestud.io/tutorials/retrofit-getting-started-and-android-client
- Documentation officielle des fonctions de requête HTTP en Kotlin : https://kotlinlang.org/api/latest/jvm/stdlib/index.html?search=http%20requests
- Liste des librairies populaires pour gérer les requêtes HTTP en Kotlin : https://github.com/KotlinBy/awesome-kotlin#networking