---
title:                "Java: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Lire et manipuler des fichiers texte est une compétence importante en programmation Java. En utilisant des techniques appropriées, vous pouvez facilement lire des données à partir de fichiers et les traiter pour une variété d'applications telles que l'analyse de données, la génération de rapports et bien plus encore. Dans cet article, nous allons vous montrer comment lire un fichier texte en utilisant Java.

## Comment faire

Pour lire un fichier texte en Java, vous devez suivre les étapes suivantes :

1. Tout d'abord, créez une instance de la classe `File` en spécifiant le chemin absolu ou relatif du fichier que vous souhaitez lire.
2. Utilisez cette instance de la classe `File` pour créer une instance de la classe `FileReader`, qui sera utilisée pour lire le fichier.
3. Enveloppez la `FileReader` dans une instance de la classe `BufferedReader` pour améliorer les performances de lecture.
4. Utilisez la méthode `readLine()` de la classe `BufferedReader` pour lire chaque ligne du fichier.
5. Traitez les données lues selon les besoins de votre application.
6. N'oubliez pas de fermer les ressources ouvertes une fois l'opération de lecture terminée.

Voici un exemple de code en utilisant ces étapes pour lire un fichier texte :

```Java
import java.io.*;

public class Main {
  public static void main(String[] args) throws IOException{
    // Créer une instance de la classe File en spécifiant le chemin du fichier
    File file = new File("fichier.txt");
    // Créer une instance de la classe FileReader en utilisant la classe File
    FileReader fileReader = new FileReader(file);
    // Envelopper FileReader avec BufferedReader pour améliorer les performances de lecture
    BufferedReader bufferedReader = new BufferedReader(fileReader);
    String line;
    // Utiliser readLine() pour lire chaque ligne du fichier
    while ((line = bufferedReader.readLine()) != null) {
        // Traiter les données lues selon les besoins de votre application
        System.out.println(line);
    }
    // N'oubliez pas de fermer les ressources
    bufferedReader.close();
  }
}
```

## Plongée profonde

Maintenant que nous avons vu comment lire un fichier texte en Java, il est important de noter certains points clés à garder à l'esprit :

- Il est recommandé d'utiliser la classe `BufferedReader` pour améliorer les performances de lecture, surtout si vous manipulez de grands fichiers.
- La méthode `readLine()` renvoie `null` lorsque la fin du fichier est atteinte.
- Il est important de traiter les exceptions lors de la lecture d'un fichier en utilisant l'opérateur `try-catch` ou en ajoutant une clause `throws` aux méthodes appelantes.

## Voir aussi

- [Java File Class](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Java FileReader Class](https://docs.oracle.com/javase/7/docs/api/java/io/FileReader.html)
- [Java BufferedReader Class](https://docs.oracle.com/javase/7/docs/api/java/io/BufferedReader.html)