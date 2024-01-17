---
title:                "Analyser le html"
html_title:           "Kotlin: Analyser le html"
simple_title:         "Analyser le html"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Parser HTML est le processus de lire et d'analyser le code HTML pour extraire les données ou les informations qu'il contient. Les programmeurs font cela pour pouvoir utiliser ces données dans leurs applications ou pour automatiser des tâches telles que l'extraction de contenu Web.

## Comment faire:
Voici un exemple simple de code en Kotlin pour réaliser un parsing HTML:

```Kotlin
val htmlString = "<html><body><h1>Bienvenue</h1></body></html>"

val document = Jsoup.parse(htmlString)
val heading = document.select("h1")

println(heading.text())
```

Output: Bienvenue

## Plongée en profondeur:
Parser HTML n'était pas aussi répandu dans le passé car les pages Web étaient généralement statiques et construites à la main. Cependant, avec l'avènement des applications Web dynamiques, le parsing HTML est devenu plus courant pour extraire des données en temps réel. Il existe également des alternatives telles que l'utilisation de l'API DOM native du navigateur ou des bibliothèques telles que Nokogiri en Ruby.

En termes d'implémentation, la bibliothèque Jsoup est largement utilisée en Kotlin pour réaliser un parsing HTML. Elle utilise des sélecteurs CSS pour extraire les données du document HTML.

## Voir aussi:
- La documentation officielle de Jsoup: https://jsoup.org/
- Un tutoriel pas à pas pour apprendre le parsing HTML en Kotlin: https://www.tutorialkart.com/kotlin/parse-html-in-kotlin-using-jsoup/