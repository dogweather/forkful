---
title:    "Kotlin: Écriture vers l'erreur standard"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Pourquoi

Beaucoup de programmeurs ont entendu parler de l'écriture sur la sortie d'erreur standard, mais se demandent peut-être pourquoi ils devraient s'en soucier. Eh bien, cela peut sembler une chose insignifiante, mais écrire sur la sortie d'erreur standard est en fait une étape importante pour s'assurer que votre code fonctionne correctement et pour déboguer plus facilement les erreurs.

# Comment faire

Il est très facile de faire une erreur dans votre code, surtout lorsque vous débutez en programmation. Parfois, il peut sembler que tout fonctionne bien, mais en vérifiant la sortie d'erreur standard, vous pouvez voir que votre code a en fait généré des erreurs silencieuses qui peuvent causer des problèmes plus tard. Heureusement, Kotlin a une façon simple de gérer cela.

````Kotlin
fun main() {
    println("Hello World!")
    System.err.println("Oops, something went wrong.")
}
````
Lorsque vous exécutez ce code, vous verrez "Hello World!" imprimé dans la sortie standard, mais aussi "Oops, something went wrong." dans la sortie d'erreur standard. Cela peut sembler une petite chose, mais cela peut vous sauver la mise lors du débogage de votre code.

# Plongée en profondeur

Alors pourquoi utiliser System.err.println() plutôt que simplement println()? Eh bien, println() envoie du texte à la sortie standard tandis que System.err.println() envoie du texte à la sortie d'erreur standard. En envoyant vos erreurs à la sortie d'erreur standard, vous pouvez facilement les rechercher et les corriger au lieu de les manquer complètement.

Il est également important de noter que la sortie d'erreur standard est différente de la sortie standard. La sortie d'erreur standard est généralement utilisée pour afficher des messages d'erreur ou de débogage, tandis que la sortie standard est utilisée pour afficher des informations utiles pour les utilisateurs. En utilisant la sortie d'erreur standard, vous séparez ces deux types de messages et pouvez les gérer plus efficacement.

# Voir aussi

- [Kotlin Documentation sur l'output console](https://kotlinlang.org/docs/tutorials/command-line.html#output)

- [System.err.println() vs System.out.println()](https://stackoverflow.com/questions/11316713/system-err-println-vs-system-out-println)