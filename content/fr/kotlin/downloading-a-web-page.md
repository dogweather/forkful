---
title:                "Kotlin: Télécharger une page web"
simple_title:         "Télécharger une page web"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Pourquoi

Télécharger une page Web est une tâche courante pour les programmeurs modernes qui souhaitent extraire des données d'une source en ligne ou utiliser des informations dans des applications. Cela peut également vous aider à analyser des sites Web et à en extraire des informations pertinentes pour votre usage personnel ou professionnel. Dans cet article, nous allons explorer comment télécharger une page Web en utilisant le langage de programmation Kotlin.

## Comment le faire

Pour télécharger une page Web en utilisant Kotlin, nous aurons besoin de la bibliothèque JSoup, qui est un parseur HTML qui nous permettra de naviguer et de récupérer des éléments à partir d'une page Web.

Pour commencer, nous devons d'abord importer la bibliothèque JSoup dans notre fichier Kotlin en utilisant la ligne de code suivante :

```Kotlin
import org.jsoup.Jsoup
```

Ensuite, nous pouvons utiliser la fonction `connect()` de JSoup pour spécifier l'URL de la page Web que nous voulons télécharger :

```Kotlin
val url = "https://www.example.com"
val document = Jsoup.connect(url).get()
```

Nous pouvons maintenant utiliser la variable `document` pour extraire des éléments spécifiques de la page en utilisant des sélecteurs CSS. Par exemple, si nous voulons récupérer le contenu d'une balise `<h1>`, nous pouvons utiliser la méthode `select()` suivie du sélecteur CSS correspondant :

```Kotlin
val title = document.select("h1").text()
println(title) // affiche le contenu de la balise h1
```

Vous pouvez également utiliser un boucle pour extraire une liste d'éléments et les stocker dans une liste en utilisant la méthode `select()` :

```Kotlin
val links = document.select("a").map { link -> link.attr("href") }
println(links) // affiche une liste des liens présents sur la page
```

## Plongée en profondeur

En plus de la récupération de contenu, la bibliothèque JSoup offre également des fonctionnalités pour modifier et soumettre des formulaires sur une page Web ainsi que pour gérer les cookies et les connexions HTTPS.

De plus, JSoup nous donne la possibilité de traiter du contenu HTML malformé et de le convertir en un document propre avec des méthodes telles que `clean()` et `tidy()`. Cela peut être utile lors de l'analyse de pages Web complexes ou mal structurées.

## Voir aussi

Vous pouvez trouver plus d'informations sur l'utilisation de Kotlin pour télécharger des pages Web sur les liens suivants :

- Documentation officielle de JSoup : https://jsoup.org/cookbook/
- Tutoriel vidéo sur Kotlin et JSoup : https://www.youtube.com/watch?v=mYXtV-h7aa4
- Projet GitHub avec des exemples de téléchargement de pages Web en Kotlin : https://github.com/RogueAlex/Kotlin-Web-Scraper