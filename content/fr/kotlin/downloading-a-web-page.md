---
title:                "Le téléchargement d'une page web"
html_title:           "Kotlin: Le téléchargement d'une page web"
simple_title:         "Le téléchargement d'une page web"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Qu'est-ce que et Pourquoi?

Télécharger une page web signifie récupérer le contenu d'une page web à partir d'un serveur distant et l'afficher sur votre propre ordinateur. Les programmeurs le font pour automatiser les tâches répétitives, extraire des informations à des fins de traitement de données ou pour le développement de logiciels.

## Comment faire:

```Kotlin
val url = URL("https://exemple.com")
val urlConnection = url.openConnection() as HttpURLConnection

try {
    val responseCode = httpURLConnection.responseCode
    if (responseCode == HttpURLConnection.HTTP_OK) {
        val input = urlConnection.inputStream
        val bufferedReader = BufferedReader(InputStreamReader(inputStream))
        val response = StringBuffer()

        bufferedReader.useLines { lines -> lines.forEach { response.append(it) } }

        println(response.toString())
    } else {
        println("Erreur lors de la récupération de la page web")
    }
} finally {
    urlConnection.disconnect()
}
```

## Plongée en profondeur:

Télécharger une page web est une pratique courante en développement web et en traitement de données. Les programmeurs peuvent également utiliser des bibliothèques telles que OkHttp ou Retrofit pour une mise en œuvre plus conviviale. Les méthodes utilisées pour télécharger une page web incluent les requêtes GET et POST, ainsi que la manipulation des cookies et des en-têtes.

## À voir aussi:

- [Guide complet sur la récupération de pages web en Kotlin](https://developer.android.com/kotlin/network/internet-protocols)
- [OkHttp, bibliothèque de requêtes HTTP pour Kotlin](https://square.github.io/okhttp/)
- [Retrofit, bibliothèque de requêtes HTTP pour Kotlin](https://square.github.io/retrofit/)