---
title:                "Kotlin: Écrire vers les erreurs standards."
simple_title:         "Écrire vers les erreurs standards."
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi

Dans le processus de développement de logiciels, il est souvent nécessaire de déboguer du code ou de détecter des erreurs à mesure qu'elles surviennent. L'une des façons d'accomplir cela est en écrivant sur la sortie standard d'erreur, ou "standard error" en anglais. Dans cet article, nous allons voir pourquoi et comment utiliser cette technique en programmation Kotlin.

## Comment faire

Pour écrire sur la sortie standard d'erreur en Kotlin, il suffit d'utiliser la fonction `System.err.println()` avec le message que vous souhaitez afficher entre parenthèses. Voici un exemple de code :

```Kotlin
fun main() {
    System.err.println("Erreur : impossible d'ouvrir le fichier.")
}
```
Lorsque vous exécutez ce code, le message "Erreur : impossible d'ouvrir le fichier." sera affiché sur la console en rouge pour indiquer qu'il s'agit d'une erreur. La syntaxe est similaire à l'utilisation de `System.out.println()` pour écrire sur la sortie standard.

## Deep Dive

Pour une utilisation plus avancée de l'écriture sur la sortie standard d'erreur en Kotlin, il est possible de définir un `PrintStream` spécifique pour gérer les erreurs. Cela peut être utile si vous souhaitez rediriger les erreurs vers un fichier ou un gestionnaire d'erreurs personnalisé. Voici un exemple de code :

```Kotlin
import java.io.*

fun main() {
    val errorStream = PrintStream(FileOutputStream("erreurs.txt"))
    System.setErr(errorStream)
    System.err.println("Une erreur s'est produite dans le fichier.")
}
```
Dans cet exemple, nous créons un `PrintStream` pour écrire dans le fichier "erreurs.txt". Ensuite, nous utilisons la fonction `System.setErr()` pour rediriger les erreurs vers ce `PrintStream` et enfin, nous utilisons la fonction `System.err.println()` pour écrire notre message d'erreur. Cela peut être particulièrement utile pour le débogage lorsqu'un utilisateur n'a pas accès à la console.

# Voir aussi

- [Documentation officielle de Kotlin](https://kotlinlang.org/docs/reference/basic-syntax.html)
- [Article sur la gestion des erreurs en Kotlin](https://www.baeldung.com/kotlin/logging-and-exception-handling)
- [Tutoriel complet sur Kotlin](https://www.tutorialspoint.com/kotlin/index.htm)