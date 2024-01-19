---
title:                "Télécharger une page web"
html_title:           "Bash: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi?

Télécharger une page web, c'est enregistrer son contenu sur votre disque dur. Les programmeurs le font pour travailler hors ligne, pour du scraping web, ou pour analyser la structure d'une page.

## Comment Faire:

On va faire un exemple simple avec Ktor (une bibliothèque Kotlin). On aura besoin de sa dépendance dans build.gradle:

```Kotlin
dependencies {
    implementation 'io.ktor:ktor-client-core:1.6.3'
    implementation 'io.ktor:ktor-client-cio:1.6.3'
}
```

Maintenant, le code pour télécharger une page web:

```Kotlin
import io.ktor.client.*
import io.ktor.client.request.*

suspend fun main() {
    val client = HttpClient()
    val pageContent = client.get<String>("https://google.com")
    println(pageContent)
}
```
Ce script affiche le contenu de la page Google. Modifiez l'URL pour télécharger d'autres pages.

## Plongée en profondeur:

Historiquement, les téléchargements de pages web étaient plus complexes, nécessitant des outils comme cURL. Aujourd'hui, des bibliothèques comme Ktor simplifient la tâche.

Il existe plusieurs alternatives à Ktor, comme OkHttp et Apache HttpClient. OkHttp est parfait pour les opérations plus complexes, tandis qu'Apache HttpClient offre une compatibilité maximale.

Concernant le fonctionnement interne de l'exemple, la fonction `get` demande une réponse de type `String`. Ktor récupère la page, la convertit en String et la retourne.

## Voir Aussi:

- Documentation de Ktor: https://ktor.io/docs/welcome.html
- Guide OkHttp: https://square.github.io/okhttp/
- Documentation Apache HttpClient: https://hc.apache.org/httpcomponents-client-5.0.x/tutorial/html/fundamentals.html