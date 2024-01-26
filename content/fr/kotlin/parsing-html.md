---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:32:47.497411-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi ?)
L'analyse de HTML, c'est lire et comprendre le code HTML avec un programme. On fait ça pour extraire des données ou manipuler la structure du contenu web.

## How to: (Comment faire ?)
On va utiliser Jsoup, une bibliothèque Java populaire qui fonctionne bien avec Kotlin.

```kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Salut, Kotlin!</title></head>" +
               "<body><p>Ceci est un paragraphe.</p></body></html>"
    val doc = Jsoup.parse(html)
    val title = doc.title()
    val pText = doc.select("p").first()?.text()

    println("Le titre est: $title")
    println("Texte du paragraphe: $pText")
}
```
Sortie :
```
Le titre est: Salut, Kotlin!
Texte du paragraphe: Ceci est un paragraphe.
```

Ce code parse un simple document HTML et extrait le titre et le texte du paragraphe.

## Deep Dive (Plongée en profondeur)
Historiquement, analyser du HTML était compliqué et sujet à erreurs en raison de la souplesse du format HTML. Les bibliothèques comme Jsoup ont grandement simplifié la tâche en fournissant une interface DOM claire et en gérant les bizarreries du HTML pour nous.

Il existe des alternatives à Jsoup, comme HtmlUnit ou le parsing basé sur les expressions régulières, mais elles ont leurs propres complexités ou limites. Jsoup brille avec sa facilité d'utilisation et sa capacité à gérer du HTML "réel", souvent imparfait.

L'implémentation repose sur l'utilisation d'un sélecteur CSS pour accéder aux éléments désirés, très similaire à jQuery en JavaScript, rendant la transition entre les langages presque transparente pour les développeurs.

## See Also (Voir aussi)
- [Jsoup Documentation](https://jsoup.org/)
- [Kotlin Programming Language](https://kotlinlang.org/)
- [HTML Parsing in Java using Jsoup - Tutorial](https://www.baeldung.com/java-with-jsoup)
