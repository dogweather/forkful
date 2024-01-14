---
title:                "Java: Vérification de l'existence d'un répertoire"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Vérifier si un répertoire existe est une tâche courante en programmation Java. Cela peut être utile lors de la gestion de fichiers et de dossiers, ou pour s'assurer qu'une certaine structure de dossiers existe avant d'exécuter un code spécifique. Dans cet article, nous allons voir comment effectuer cette vérification en utilisant Java.

## Comment faire

Pour vérifier si un répertoire existe, nous pouvons utiliser la méthode `exists()` de la classe `java.io.File`. Cette méthode renvoie un booléen, `true` si le répertoire existe et `false` dans le cas contraire. Voici un exemple de code :

```Java
File directory = new File("chemin/vers/mon/dossier"); // modifier le chemin avec un répertoire existant
if(directory.exists()){
    System.out.println("Le répertoire existe !");
} else {
    System.out.println("Le répertoire n'existe pas.");
}
```

Si vous exécutez ce code, vous verrez la phrase "Le répertoire existe !" s'afficher car nous avons donné un chemin vers un répertoire existant. Si le répertoire n'existe pas, le message "Le répertoire n'existe pas." sera affiché.

## Plongée profonde

Il est également possible de vérifier si un répertoire existe en utilisant la classe `java.nio.file.Files` et sa méthode `exists()`. Cette méthode prend en paramètre un objet `Path` qui représente le chemin vers le répertoire. Voici un exemple de code :

```Java
Path directoryPath = Paths.get("chemin/vers/mon/dossier"); // modifier le chemin avec un répertoire existant
if(Files.exists(directoryPath)){
    System.out.println("Le répertoire existe !");
} else {
    System.out.println("Le répertoire n'existe pas.");
}
```

De plus, la classe `java.nio.file.Files` offre également d'autres méthodes utiles pour vérifier si un répertoire existe, telles que `isDirectory()` pour vérifier si le chemin mène bien à un répertoire et non à un fichier.

## Voir aussi

Vous pouvez consulter ces liens pour en savoir plus sur la vérification de l'existence d'un répertoire en Java :

- [Documentation officielle de la classe `java.io.File`](https://docs.oracle.com/javase/7/docs/api/java/io/File.html#exists())
- [Documentation officielle de la classe `java.nio.file.Files`](https://docs.oracle.com/javase/7/docs/api/java/nio/file/Files.html#exists(java.nio.file.Path, java.nio.file.LinkOption...))