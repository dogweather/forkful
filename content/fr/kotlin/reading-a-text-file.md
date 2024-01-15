---
title:                "Lecture d'un fichier texte"
html_title:           "Kotlin: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous avez déjà travaillé avec des fichiers de texte dans vos projets de programmation, vous savez que c'est une tâche assez courante. Dans cet article, nous allons vous montrer comment lire un fichier de texte en utilisant le langage de programmation Kotlin. Que vous soyez un développeur expérimenté ou débutant, maîtriser cette compétence peut vous faire gagner beaucoup de temps et d'efforts.

# Comment faire

Pour lire un fichier de texte en Kotlin, nous allons utiliser la classe `File` et la méthode `readLines()`. Voici un exemple de code pour vous montrer comment cela fonctionne :

```
// Importer la classe File
import java.io.File

// Définir le chemin du fichier de texte
val filePath = "nom_du_fichier.txt"

// Créer une instance de la classe File en utilisant le chemin du fichier
val file = File(filePath)

// Lire toutes les lignes du fichier et les stocker dans une liste
val lines: List<String> = file.readLines()

// Parcourir la liste et afficher chaque ligne
for (line in lines) {
    println(line)
}
```

Si nous utilisons un fichier de texte avec le contenu suivant :

```
Salut!
Bienvenue dans notre guide de lecture de fichiers texte en Kotlin.
Nous espérons que cela vous sera utile!
```

Alors l'output de notre code sera :

```
Salut!
Bienvenue dans notre guide de lecture de fichiers texte en Kotlin.
Nous espérons que cela vous sera utile!
```

# Deep Dive

Comme vous pouvez le voir dans l'exemple de code ci-dessus, la méthode `readLines()` lit toutes les lignes du fichier et les stocke dans une liste. Si vous souhaitez manipuler les données du fichier d'une autre manière que simplement les afficher, vous pouvez utiliser la méthode `forEachLine()` :

```
// Lire le fichier et appliquer une opération à chaque ligne
file.forEachLine { line ->
    // Votre code ici
}
```

De plus, la classe `File` offre plusieurs autres méthodes utiles pour gérer les fichiers, telles que `exists()` pour vérifier si le fichier existe, `delete()` pour supprimer un fichier et `writeText()` pour écrire du texte dans un fichier. N'hésitez pas à explorer ces méthodes pour mieux comprendre leurs fonctionnalités.

# Voir aussi

Pour en savoir plus sur la manipulation de fichiers en Kotlin, vous pouvez consulter les ressources suivantes :

- [Documentation officielle de Kotlin sur les entrées/sorties](https://kotlinlang.org/docs/reference/keyword-reference.html#fileio)
- [Article de Baeldung : Reading and Writing Files in Kotlin](https://www.baeldung.com/kotlin-reading-writing-files)
- [Tutoriel de Codecademy : Learn File I/O in Kotlin](https://www.codecademy.com/learn/learn-kotlin/modules/learn-kotlin-files-and-i-o)

Maintenant, vous êtes prêt à lire des fichiers de texte en utilisant Kotlin dans vos propres projets. Bonne programmation !