---
title:                "Envoi d'une requête http"
html_title:           "Kotlin: Envoi d'une requête http"
simple_title:         "Envoi d'une requête http"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des applications web, il est probable que vous ayez besoin d'envoyer une requête HTTP à un serveur pour récupérer des données. Kotlin fournit une manière simple et efficace de le faire.

## Comment Faire 

```Kotlin 
// Importez la bibliothèque okhttp
import okhttp3.*

// Créez un client OkHttpClient
val client = OkHttpClient()

// Construisez l'URL de votre requête
val url = "https://monsite.com/api/articles"

// Créez une requête GET
val request = Request.Builder()
    .url(url)
    .build()

// Exécutez la requête et obtenez la réponse
val response = client.newCall(request).execute()

// Récupérez le corps de la réponse
val responseBody = response.body?.string()

// Imprimez le résultat dans la console
println(responseBody)
```

Output : Vous devriez voir les données de votre requête s'afficher dans la console.

## Plongez Plus Profondément

En utilisant la bibliothèque okhttp, vous pouvez personnaliser votre requête en ajoutant des paramètres, des en-têtes et même en gérant les erreurs. Vous pouvez également utiliser des méthodes HTTP autres que GET, comme POST, PUT, DELETE, etc. pour modifier les données sur le serveur. Avec Kotlin, vous avez la flexibilité de choisir la méthode qui convient le mieux à votre application.

## Voir Aussi

- [Documentation OkHttp](https://square.github.io/okhttp/)
- [Vidéo sur les requêtes HTTP avec Kotlin](https://www.youtube.com/watch?v=CUbY3zfEMWg)
- [Tutoriel Kotlin sur les requêtes HTTP](https://www.raywenderlich.com/11359307-kotlin-tutorial-for-android-getting-started)