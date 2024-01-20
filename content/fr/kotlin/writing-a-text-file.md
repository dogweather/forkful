---
title:                "Écriture d'un fichier texte"
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?

Écrire dans un fichier texte signifie enregistrer des données en format lisible. C'est essentiel pour sauvegarder des configurations, enregistrer des logs ou partager des informations entre systèmes et utilisateurs.

## Comment faire :

```kotlin
import java.io.File

fun main() {
    val text = "Bonjour, ceci est un test d'écriture dans un fichier."
    File("example.txt").writeText(text)
}
```

Sortie : Un fichier nommé `example.txt` est créé avec le contenu "Bonjour, ceci est un test d'écriture dans un fichier."

Pour ajouter du texte sans écraser le contenu existant :

```kotlin
import java.io.File

fun main() {
    val text = "Ajoutons cette ligne."
    File("example.txt").appendText(text)
}
```

Sortie : `example.txt` contient maintenant le texte original plus "Ajoutons cette ligne."

## Exploration approfondie

Historiquement, l'écriture de fichiers en code est aussi ancienne que la programmation elle-même, permettant la persistance des données au-delà de la durée d'exécution du programme.

Alternativement, on peut utiliser `BufferedWriter`, `FileWriter` ou l'API `Files` de Java NIO pour plus de contrôle et d'efficacité, surtout avec des fichiers volumineux. 

Kotlin fournit l'API standard de Java avec quelques ajouts pour faciliter le développement, comme les fonctions `writeText` et `appendText`.

## Voir aussi

- [Documentation Kotlin pour File writeText](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/write-text.html)
- [Documentation Kotlin pour File appendText](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/append-text.html)
- [Oracle Java tutorials - File I/O](https://docs.oracle.com/javase/tutorial/essential/io/)
- [Stack Overflow: Kotlin Write to File](https://stackoverflow.com/questions/35421699/how-to-write-to-file-in-kotlin)