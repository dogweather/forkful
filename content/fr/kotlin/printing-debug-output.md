---
title:                "Kotlin: Affichage du débogage"
programming_language: "Kotlin"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est important de savoir comment afficher des informations de débogage lors de la programmation en Kotlin afin de résoudre rapidement des erreurs dans votre code. Cela peut vous faire gagner du temps et vous aider à comprendre le fonctionnement de votre programme.

## Comment Faire

Voici un exemple simple de code Kotlin avec des instructions permettant de voir des informations de débogage :

```
fun main() {
    val x: Int = 5
    println("x est égal à $x.") // Affiche x est égal à 5.
    println("Le double de x est égal à ${x * 2}.") // Affiche Le double de x est égal à 10.
}
```

Dans cet exemple, nous avons utilisé la fonction `println` pour afficher des informations de débogage. Cette fonction permet d'afficher une chaîne de caractères et peut également afficher des valeurs de variables en les plaçant à l'intérieur des accolades `${}`.

Vous pouvez également utiliser la fonction `print` si vous ne voulez pas que le curseur se déplace à la ligne suivante après chaque instruction d'impression.

## Plongée en Profondeur

Il existe d'autres façons d'afficher des informations de débogage en Kotlin. Par exemple, la fonction `debug` du package `kotlinx.serialization` peut être utilisée pour imprimer des détails sur une variable, même lorsqu'elle est nullable. Vous pouvez également utiliser la fonction `assert` pour vérifier des conditions dans votre code et afficher un message d'erreur si elles ne sont pas respectées.

Il est également important de noter que l'utilisation excessive de l'affichage de débogage peut ralentir considérablement votre programme. Il est donc conseillé de supprimer ces instructions d'impression une fois que vous avez résolu les erreurs ou compris le fonctionnement de votre code.

## Voir Aussi

- [Documentation officielle de Kotlin sur le débogage](https://kotlinlang.org/docs/tutorials/command-line.html#debugging-with-assert)
- [Article Medium sur l'affichage de débogage en Kotlin](https://medium.com/better-programming/debugging-in-kotlin-76335e96dd2b)
- [Exemple de débogage en Kotlin avec IntelliJ IDEA](https://blog.jetbrains.com/idea/2011/02/deep-debugging-of-intellij-idea-kotlin-plugin/)