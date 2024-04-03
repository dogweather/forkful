---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:19.244408-07:00
description: "Parser du HTML signifie diss\xE9quer le balisage d'une page web en quelque\
  \ chose qu'un programme peut comprendre et manipuler. Les programmeurs analysent\
  \ le\u2026"
lastmod: '2024-03-13T22:44:57.736432-06:00'
model: gpt-4-0125-preview
summary: "Parser du HTML signifie diss\xE9quer le balisage d'une page web en quelque\
  \ chose qu'un programme peut comprendre et manipuler."
title: Analyse Syntaxique du HTML
weight: 43
---

## Comment faire :
Kotlin rend le parsing HTML simple avec des bibliothèques comme Jsoup. Voici comment procéder :

```Kotlin
import org.jsoup.Jsoup

fun main() {
    val html = "<html><head><title>Page d'exemple</title></head><body><p>Ceci est un test.</p></body></html>"
    val doc = Jsoup.parse(html)

    val title = doc.title()
    println("Titre : $title")  // Sortie : Titre : Page d'exemple

    val pText = doc.select("p").first()?.text()
    println("Paragraphe : $pText")  // Sortie : Paragraphe : Ceci est un test.
}
```

Nous récupérons le titre et le texte du paragraphe, ce qui n'est qu'un aperçu de ce que Jsoup peut faire. Mais c'est un début.

## Plongée en profondeur :
Avant Kotlin, Java était le choix préféré pour cela, souvent de manière maladroite. Jsoup a changé la donne en proposant une approche à la jQuery. Cependant, le parsing HTML n'est pas exclusif à Jsoup ; d'autres bibliothèques comme HtmlUnit ou même les expressions régulières (bien que déconseillées) existent. Avec Jsoup, vous assurez que votre analyse respecte la structure du document. Il utilise un modèle DOM, permettant la sélection et la manipulation des éléments. Il est résilient, aussi - il peut parse même le HTML le plus désordonné.

## Voir également :
Plongez plus profondément dans Jsoup :

- Documentation officielle de Jsoup : https://jsoup.org/
- Livre "Kotlin for Android Developers" : https://antonioleiva.com/kotlin-android-developers-book/
- Site officiel du langage de programmation Kotlin : https://kotlinlang.org/

Pour des discussions plus larges et des tutoriels sur le web scraping et l'analyse :

- Web Scraping avec Kotlin et Jsoup : https://medium.com/@hadiyarajesh/web-scraping-with-kotlin-and-jsoup-8b5b6c31c5a5
- Analyse du HTML sur Android avec Kotlin et Jsoup : https://proandroiddev.com/parsing-html-on-android-1b766658be6a
