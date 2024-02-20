---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:16.445611-07:00
description: "\xC9crire un fichier texte en Kotlin implique la cr\xE9ation d'un fichier\
  \ et l'insertion de contenu textuel \xE0 l'int\xE9rieur, une t\xE2che courante pour\
  \ le stockage\u2026"
lastmod: 2024-02-19 22:05:16.503614
model: gpt-4-0125-preview
summary: "\xC9crire un fichier texte en Kotlin implique la cr\xE9ation d'un fichier\
  \ et l'insertion de contenu textuel \xE0 l'int\xE9rieur, une t\xE2che courante pour\
  \ le stockage\u2026"
title: "R\xE9diger un fichier texte"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Écrire un fichier texte en Kotlin implique la création d'un fichier et l'insertion de contenu textuel à l'intérieur, une tâche courante pour le stockage de données, la journalisation ou les paramètres de configuration. Les programmeurs le font pour sauvegarder et manipuler des données en dehors de l'espace mémoire volatile, assurant ainsi la persistance à travers les sessions.

## Comment faire :
Kotlin offre une approche simple pour écrire dans des fichiers, en tirant parti de la bibliothèque standard sans nécessiter de bibliothèques tierces supplémentaires. Voici un exemple simple :

```kotlin
import java.io.File

fun main() {
    val texteAÉcrire = "Bonjour, écriture de fichier Kotlin !"
    File("exemple.txt").writeText(texteAÉcrire)
}
```
Ce fragment de code crée un fichier nommé "exemple.txt" dans le répertoire racine du projet et écrit la chaîne `Bonjour, écriture de fichier Kotlin !` dedans. Si le fichier existe déjà, il sera écrasé.

Pour ajouter du texte à un fichier de manière plus contrôlée ou écrire de plus grandes quantités de données, vous pouvez utiliser `appendText` ou `bufferedWriter()` :

```kotlin
import java.io.File

fun appendToFile() {
    val plusDeTexte = "Ajout de plus de texte."
    File("exemple.txt").appendText(plusDeTexte)
}

fun writeWithBufferedWriter() {
    val grandTexte = "De grandes quantités de texte...\nSur plusieurs lignes."
    File("sortie.txt").bufferedWriter().use { out ->
        out.write(grandTexte)
    }
}

fun main() {
    appendToFile() // Ajoute du texte au fichier existant
    writeWithBufferedWriter() // Écrit de grandes données de texte de manière efficace
}
```

Dans la fonction `appendToFile`, nous ajoutons plus de texte à "exemple.txt" sans écraser son contenu actuel. La fonction `writeWithBufferedWriter` montre une manière efficace d'écrire de grandes quantités de texte ou de données, particulièrement utile pour minimiser les opérations d'entrée/sortie lors de la manipulation de plusieurs lignes ou de grands fichiers.

Ces exemples couvrent les opérations de base pour écrire des fichiers texte en Kotlin, démontrant la simplicité et la puissance de la bibliothèque standard de Kotlin pour les opérations d'entrée/sortie de fichiers.
