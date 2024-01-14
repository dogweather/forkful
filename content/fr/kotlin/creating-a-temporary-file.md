---
title:    "Kotlin: Création d'un fichier temporaire"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Pourquoi

La création de fichiers temporaires est une pratique courante en programmation car elle permet de stocker temporairement des données sans encombrer le système de fichiers permanent. Cela est particulièrement utile lorsqu'il s'agit de manipuler de grandes quantités de données ou de gérer des opérations transitoires.

## Comment faire

Voici un exemple simple de création d'un fichier temporaire en utilisant Kotlin :

```Kotlin
val tempFile = File.createTempFile("temp", ".txt")
val tempFileWriter = tempFile.bufferedWriter()
tempFileWriter.write("Ceci est un fichier temporaire")
tempFileWriter.close()

println(tempFile.name)
```

La première ligne crée un fichier temporaire avec un préfixe "temp" et une extension ".txt". Le deuxième ligne initialise un écrivain en mémoire pour pouvoir écrire dans le fichier. Ensuite, le contenu souhaité est écrit et le fichier est fermé. Enfin, le nom du fichier temporaire est affiché à l'écran.

Voici ce que l'on obtiendrait comme sortie dans ce cas précis :

`temp6685993457289975897.txt`

## Approfondir

La méthode `createTempFile()` prend en paramètres un préfixe, une extension et un répertoire où le fichier temporaire sera créé. Si aucun répertoire n'est spécifié, le système utilisera par défaut le répertoire de stockage des fichiers temporaires de l'utilisateur.

De plus, il est possible de spécifier un préfixe et une extension par défaut pour tous les fichiers temporaires créés en utilisant `File.createTempFile()`. Pour cela, il faut simplement ajouter ces informations dans les options de JVM au lancement du programme.

Il est également important de noter que les fichiers temporaires sont automatiquement supprimés par le système d'exploitation une fois que le programme est terminé ou qu'ils ne sont plus utilisés. Cela les rend particulièrement utiles lorsqu'il s'agit de manipuler des données sensibles qui doivent être effacées après utilisation.

## Voir aussi

- [Documentation officielle de Kotlin sur la création de fichiers temporaires](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/create-temp-file.html)
- [Article sur la gestion des fichiers en mémoire en utilisant Java et Kotlin](https://www.baeldung.com/java-write-to-file)
- [Tutoriel sur les opérations avancées avec les fichiers en Kotlin](https://www.tutorialkart.com/kotlin/create-file-in-kotlin/)