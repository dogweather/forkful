---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Java: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des fichiers dans votre code Java, il est important de vérifier si un répertoire existe avant de l'utiliser. Cela peut éviter des erreurs et rendre votre code plus robuste.

## Comment faire

Pour vérifier si un répertoire existe en utilisant Java, vous pouvez utiliser la méthode `exists()` de la classe `File`. Voici un exemple de code qui vérifie si le répertoire "monRepertoire" existe :

```java
File monRepertoire = new File("chemin/vers/monRepertoire");
if (monRepertoire.exists()) {
  System.out.println("Le répertoire existe !");
} else {
  System.out.println("Le répertoire n'existe pas !");
}
```

Vous pouvez également utiliser la méthode `isDirectory()` pour vérifier si un fichier est un répertoire ou non. Voici un exemple de code qui utilise cette méthode :

```java
File monFichier = new File("chemin/vers/monFichier.txt");
if (monFichier.isDirectory()) {
  System.out.println("Ceci est un répertoire !");
} else {
  System.out.println("Ceci n'est pas un répertoire !");
}
```

## Deep Dive

La méthode `exists()` utilise les systèmes de fichiers de votre ordinateur pour vérifier si un répertoire existe. En général, cette méthode renvoie `true` si un fichier ou un répertoire porte le même nom que celui que vous avez spécifié. Cependant, il y a certaines limitations à cette méthode : par exemple, elle peut renvoyer `true` même si vous n'avez pas la permission d'accéder au répertoire.

De plus, il est important de noter que la méthode `exists()` ne vérifie pas si le répertoire est vide ou non. Si vous avez besoin de vérifier si un répertoire est vide, vous pouvez utiliser la méthode `listFiles()` pour obtenir un tableau des fichiers et sous-répertoires contenus dans le répertoire, et ensuite vérifier si ce tableau est vide ou non.

## Voir aussi

- [La documentation Javadocs de la classe `File`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Un tutoriel sur la manipulation des fichiers en Java](https://www.w3schools.com/java/java_files.asp)