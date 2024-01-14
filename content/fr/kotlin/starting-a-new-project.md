---
title:    "Kotlin: Commencer un nouveau projet"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Pourquoi
 
Vous vous demandez peut-être pourquoi vous devriez vous lancer dans un nouveau projet de programmation en Kotlin. Eh bien, la réponse est simple : Kotlin est un langage de programmation moderne et puissant, qui combine la concision et la sécurité de la compilation statique avec la flexibilité et l'expressivité de la programmation orientée objet. Cela en fait un choix idéal pour de nombreux projets, qu'il s'agisse de développement d'applications mobiles, de création de sites Web ou de programmation côté serveur.
 
 ## Comment faire
 
Maintenant que vous êtes convaincu(e) de l'utilité de Kotlin, passons à la partie pratique. Voici quelques exemples de code et de résultats qui vous aideront à démarrer votre nouveau projet :
 
```Kotlin
fun main(args: Array<String>) {
    println("Bonjour le monde!")
}
```
 
Cet exemple montre comment utiliser Kotlin pour afficher une simple phrase à l'écran. Comme vous pouvez le constater, la syntaxe est concise et facile à comprendre. Vous pouvez également utiliser des variables, des boucles, des fonctions et bien d'autres fonctionnalités pour créer des programmes plus complexes.
 
```Kotlin
fun calculateArea(width: Int, height: Int): Int {
    return width * height
}
 
fun main(args: Array<String>) {
    val width = 10
    val height = 5
    println(calculateArea(width, height))
}
```
 
Dans cet exemple, nous utilisons une fonction pour calculer l'aire d'un rectangle en utilisant des variables et une opération mathématique simple. Ensuite, nous appelons cette fonction dans notre fonction principale et affichons le résultat. En utilisant Kotlin, vous constaterez que vous pouvez écrire du code plus efficacement et plus proprement.
 
 ## Plongée en profondeur
 
Si vous souhaitez vous plonger plus profondément dans la création d'un nouveau projet en Kotlin, il existe de nombreuses ressources disponibles pour vous aider. Vous pouvez consulter la documentation officielle de Kotlin pour en savoir plus sur la syntaxe et les fonctionnalités du langage. Vous pouvez également rejoindre des communautés de développeurs Kotlin pour poser des questions et discuter avec d'autres passionnés du langage.
 
Enfin, n'oubliez pas de consulter les autres ressources incluses dans la section "See Also" pour découvrir plus de conseils et de tutoriels ! Bon codage !
 
## Voir également
 
- [Documentation officielle de Kotlin](https://kotlinlang.org/docs/home.html)
- [Communauté de développeurs Kotlin](https://kotlinlang.slack.com)
- [Tutoriels Kotlin sur YouTube](https://www.youtube.com/playlist?list=PLS3KlXA0DwMpsJ3NgarFdnw4JS8rh9mTL)