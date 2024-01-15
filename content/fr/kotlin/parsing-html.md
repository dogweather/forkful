---
title:                "Analyse de l'HTML"
html_title:           "Kotlin: Analyse de l'HTML"
simple_title:         "Analyse de l'HTML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

# Pourquoi

Il peut être utile de pouvoir extraire et manipuler des données directement à partir du code HTML d'une page web. Cela peut être utile pour automatiser des tâches, collecter des données ou créer des applications liées au web.

# Comment faire

Pour le faire en utilisant Kotlin, il existe des bibliothèques telles que jsoup ou ktparse qui facilitent grandement la tâche. Voici un exemple de code pour extraire le titre et le contenu d'une balise <article> à partir d'un fichier HTML :

```Kotlin

val document = Jsoup.parse(html) // html est une variable contenant le code HTML
val article = document.select("article") // sélectionne la balise article
val titre = article.select("h1").text() // sélectionne le titre de l'article
val contenu = article.select("p").first() // sélectionne le premier paragraphe du contenu

print("Titre: $titre \nContenu: $contenu")

// Résultat:
// Titre: Bienvenue dans le monde de Kotlin
// Contenu: Kotlin est un langage de programmation moderne et polyvalent conçu pour Java Virtual Machine (JVM).

```

Il est également possible de parcourir les différentes balises et éléments en utilisant des boucles. Par exemple, pour afficher tous les liens d'une page HTML :

```Kotlin

val links = document.select("a[href]") // sélectionne tous les éléments <a> avec un attribut href
for (link in links) {
    println(link.attr("href")) // affiche l'attribut href de chaque élément
}

```

# Approfondir

La bibliothèque jsoup permet également de modifier et d'ajouter des éléments à un document HTML. Il est également possible de combiner l'utilisation de Kotlin avec d'autres technologies telles que XPath pour une sélection plus précise, ou encore Selenium pour automatiser des tâches sur des sites web interactifs.

# Voir aussi

- Documentation officielle de Kotlin : https://kotlinlang.org/docs/reference/
- Bibliothèque jsoup : https://jsoup.org/
- Bibliothèque ktparse : https://github.com/collokia/kt-parse