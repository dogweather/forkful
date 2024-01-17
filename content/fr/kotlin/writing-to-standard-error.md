---
title:                "Écrire vers l'erreur standard"
html_title:           "Kotlin: Écrire vers l'erreur standard"
simple_title:         "Écrire vers l'erreur standard"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?
Le terme « écrire vers la sortie d'erreur standard » (ou « écrire vers la stderr ») fait référence à une pratique de programmation consistant à envoyer des messages d'erreur vers un flux de sortie appelé standard error. Les programmeurs utilisent cela pour signaler les erreurs et les problèmes dans leur code.

# Comment faire:
Voici un exemple de code en Kotlin pour écrire vers la stderr:
 ```Kotlin
fun main() {
    System.err.println("Erreur détectée !")
}
```
Cela produira une sortie qui ressemble à ceci:
```
Erreur détectée !
```
Vous pouvez également spécifier le type d'erreur dans votre message:
```Kotlin
fun main() {
    val num = 10
    try {
        num / 0
    } catch (e: Exception) {
        System.err.println("Erreur de division par zéro: ${e.message}")
    }
}
```
Résultat:
```
Erreur de division par zéro: / by zero
```

# Plongée en profondeur:
Écrire vers la stderr est une pratique courante dans la programmation depuis de nombreuses années. Avant cette méthode, les programmeurs utilisaient souvent des pop-ups ou des messages sur la sortie standard pour signaler les erreurs. Cependant, cela peut gêner l'utilisateur et le flux de sortie standard est généralement utilisé pour d'autres informations importantes. Écrire vers la stderr permet de séparer clairement les messages d'erreur du reste de la sortie.

Une alternative à cette méthode serait d'utiliser des logs pour gérer les erreurs. Cependant, écrire vers la stderr est souvent plus simple et plus rapide à mettre en place.

En implémentation, écrire vers la stderr est similaire à l'écriture vers la sortie standard. La seule différence est que vous utilisez System.err au lieu de System.out.

# Voir aussi:
Pour plus d'informations sur l'écriture vers la stderr en Kotlin, consultez la documentation officielle de Kotlin sur les flux de sortie.
https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.system/-print-stream/err.html

Pour en savoir plus sur la manipulation des erreurs en général, consultez cet article sur la gestion des exceptions en Kotlin.
https://kotlinlang.org/docs/reference/exceptions.html