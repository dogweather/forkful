---
title:                "Analyser le HTML"
html_title:           "Kotlin: Analyser le HTML"
simple_title:         "Analyser le HTML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

"L'analyse HTML" est le processus d'interprétation du code HTML afin d'extraire des informations spécifiques. Les programmeurs le font pour récolter des données, manipuler des pages web et automatiser des tâches.

## Comment faire:

Nous utiliserons jsoup, une bibliothèque Kotlin/Java très populaire pour l'analyse HTML. Pour commencer, ajoutez la dépendance suivante dans votre fichier build.gradle:

```Kotlin
dependencies {
    implementation 'org.jsoup:jsoup:1.13.1'
}
```
Voici un exemple simple pour extraire des titres d'un site Web:

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val document = Jsoup.connect("https://votre-site-web.com").get()
    val titles = document.select("h2.title").map { it.text() }
    println(titles)
}
```

Créez une connexion avec le site Web, sélectionnez les éléments que vous voulez et mappez-les au texte.

## Exploration Approfondie:

L'analyse HTML a été autour depuis les premiers jours du web. Javascript et Perl étaient très populaires pour cela, mais ils ont leur propre complexité. Kotlin, en revanche, est conçu pour être concis et sûr, ce qui le rend plus attrayant pour les tâches comme l'analyse HTML.

Bien que jsoup soit très populaire, des alternatives comme HtmlCleaner et jtidy existent. Ces bibliothèques peuvent avoir des avantages spécifiques à certains cas d'utilisation, mais jsoup reste le choix le plus populaire grâce à sa simplicité d'utilisation.

L'implémentation d'analyse HTML avec jsoup fonctionne en interprétant le DOM (Document Object Model) du HTML. Après avoir analysé le code HTML, jsoup génère une arborescence d'objets qui peut être naviguée et manipulée facilement.

## Voir Aussi:

Pour plus d'informations, consultez les sources suivantes:

1. [Documentation officielle de Jsoup](https://jsoup.org/)
2. [Kotlin for JavaScript](https://kotlinlang.org/docs/js-overview.html)
3. [HtmlCleaner Library](https://htmlcleaner.sourceforge.io/)
4. [jtidy Library](http://jtidy.sourceforge.net/)