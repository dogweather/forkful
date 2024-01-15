---
title:                "Téléchargement d'une page web"
html_title:           "Kotlin: Téléchargement d'une page web"
simple_title:         "Téléchargement d'une page web"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi 

Si vous êtes intéressé par le développement web ou que vous souhaitez créer une application qui récupère des données à partir d'internet, savoir comment télécharger une page web est une compétence cruciale à avoir. Cela vous permet d'accéder à une mine d'informations en ligne et d'intégrer ces données dans vos projets.

## Comment faire 

Pour télécharger une page web en utilisant Kotlin, vous pouvez utiliser la bibliothèque OkHttp. Voici un exemple de code pour télécharger une page Google :

```Kotlin
// Importer la bibliothèque OkHttp
import okhttp3.OkHttpClient
import okhttp3.Request

// Définir l'URL de la page à télécharger
val url = "https://www.google.com"

// Créer une instance d'OkHttpClient
val client = OkHttpClient()

// Créer une requête HTTP pour cette URL
val request = Request.Builder().url(url).build()

// Exécuter la requête et stocker la réponse dans une variable
val response = client.newCall(request).execute()

// Afficher le contenu de la réponse
println(response.body()?.string())
```

Ce code utilise la fonction `execute()` pour exécuter la requête et stocker la réponse dans une variable. Ensuite, la fonction `string()` est utilisée pour afficher le contenu de la réponse en tant que chaîne de caractères.

Vous pouvez également spécifier des paramètres dans la requête si nécessaire, en utilisant les méthodes telles que `addHeader()` ou `addQueryParameter()`.

## Plongée en profondeur 

Lorsque vous téléchargez une page web, vous obtenez généralement le code HTML source de la page, qui est essentiellement du texte brut. Si vous voulez extraire des informations spécifiques de cette page, vous devrez peut-être utiliser un outil comme Jsoup pour analyser et parcourir le code HTML.

En utilisant Jsoup, vous pouvez extraire des éléments spécifiques de la page en utilisant des sélecteurs CSS ou XPath. Voici un exemple de code pour obtenir tous les liens sur une page :

```Kotlin
// Importer la bibliothèque Jsoup
import org.jsoup.Jsoup

// Définir l'URL de la page à télécharger
val url = "https://www.google.com"

// Télécharger la page en utilisant Jsoup et stocker le résultat dans une variable
val doc = Jsoup.connect(url).get()

// Utiliser le sélecteur CSS pour obtenir tous les liens de la page
val links = doc.select("a[href]")

// Parcourir les liens et afficher leur URL
for(link in links) {
    println(link.attr("abs:href"))
}
```

En utilisant des sélecteurs plus précis, vous pouvez extraire des informations telles que le titre de la page, le texte d'un élément spécifique ou même des images.

## Voir aussi 

- [Documentation OkHttp](https://square.github.io/okhttp/)
- [Documentation Jsoup](https://jsoup.org/apidocs/)
- [Exemple de scraping de données avec Kotlin](https://www.geeksforgeeks.org/scrape-a-website-using-kotlin/)