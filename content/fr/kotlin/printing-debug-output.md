---
title:                "Afficher la sortie de débogage"
html_title:           "Kotlin: Afficher la sortie de débogage"
simple_title:         "Afficher la sortie de débogage"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & pourquoi?
L'impression de sortie de débogage dans la programmation est une méthode utilisée par les programmeurs pour tester et suivre l'exécution de leur code. Le code de débogage est généralement utilisé pour identifier et résoudre des erreurs et des bugs dans le programme.

## Comment faire:
```Kotlin
val num1 = 5
val num2 = 3
println("Le résultat est: ${num1 + num2}")
```

```Kotlin
// Output:
Le résultat est: 8
```
Pour imprimer la sortie de débogage dans Kotlin, il suffit d'utiliser la fonction `println()` en passant le texte ou la valeur que vous souhaitez imprimer. Vous pouvez également utiliser la concaténation de chaînes à l'aide de l'opérateur `$` pour imprimer des valeurs de variables dans le texte.

## Plongée en profondeur:
L'impression de sortie de débogage est une technique courante utilisée par les programmeurs depuis le début de l'informatique. Elle est notamment utile pour suivre le flux d'exécution du code et identifier les erreurs que l'on pourrait rencontrer. Dans Kotlin, il existe également une autre méthode pour imprimer la sortie de débogage à l'aide de la fonction `Log.d()` de la bibliothèque Log4j.

## Voir aussi:
- [Documentation officielle de Kotlin](https://kotlinlang.org/docs/reference/basic-types.html)
- [Article sur l'impression de débogage dans Java](https://www.baeldung.com/java-print-debug-output)
- [Article sur l'utilisation de la fonction Log.d() dans Kotlin](https://blog.mindorks.com/kotlin-log-a-guide-to-logging-b26c6415e4a5)