---
title:                "La Lecture d'un fichier texte"
html_title:           "Java: La Lecture d'un fichier texte"
simple_title:         "La Lecture d'un fichier texte"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi
Si vous êtes débutant en programmation ou cherchez à apprendre un nouveau langage, vous vous demandez peut-être pourquoi vous devriez lire un fichier texte en Java. Eh bien, la lecture d'un fichier texte est une fonctionnalité importante de tout langage de programmation et en apprendre les bases vous aidera à comprendre comment manipuler et analyser des données.

## Comment faire
Pour lire un fichier texte en Java, il y a quelques étapes à suivre :

1. Importez la classe **File** de la bibliothèque ``java.io`` pour accéder aux méthodes pour lire et écrire des fichiers.
2. Utilisez la méthode ``FileReader`` pour ouvrir le fichier et le stocker dans une variable.
3. Ensuite, utilisez la classe ``BufferedReader`` pour lire le fichier ligne par ligne en utilisant la méthode ``readLine()``.
4. N'oubliez pas de gérer les erreurs avec un bloc ``try-catch`` pour éviter tout crash de votre code.

Voici un exemple de code simple pour lire un fichier texte nommé "exemple.txt" :

```Java
import java.io.File;
import java.io.FileReader;
import java.io.BufferedReader;
 
class LectureFichierTexte {
  public static void main (String[] args) {
    try {
      File fichier = new File("exemple.txt");
      FileReader reader = new FileReader(fichier);
      BufferedReader bufferedReader = new BufferedReader(reader);
 
      String ligne;
      while ((ligne = bufferedReader.readLine()) != null) {
        System.out.println(ligne);
      }
      reader.close();
 
    } catch (Exception e) {
      e.printStackTrace();
    }
  }
}
```

Pour cet exemple, si votre fichier contient les lignes "Bonjour" et "au revoir", le résultat affiché sera :

```
Bonjour
au revoir
```

## Plongée en profondeur
Il existe plusieurs façons de lire un fichier texte en Java en utilisant différentes classes et méthodes. Par exemple, vous pouvez utiliser la classe ``Scanner`` pour lire directement des données à partir d'un fichier texte, ou encore utiliser la bibliothèque Apache Commons IO pour une gestion plus avancée des fichiers.

De plus, vous pouvez également spécifier l'encodage du fichier à lire en utilisant la méthode ``FileReader(String file, Charset charset)`` pour éviter tout problème avec des caractères spéciaux.

## Voir aussi
- [Tutoriel Java officiel pour lire et écrire des fichiers](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Méthodes pour lire des fichiers en Java](https://www.geeksforgeeks.org/ways-read-file-java/)
- [Documentation de la classe File](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/File.html)
- [Documentation de la classe BufferedReader](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/io/BufferedReader.html)