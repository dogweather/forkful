---
title:                "Java: La lecture d'un fichier texte"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

##Pourquoi

La lecture de fichiers texte est une tâche essentielle en programmation Java. Que vous souhaitiez lire des données à partir d'un fichier de configuration ou traiter un document texte, savoir comment manipuler les fichiers texte est très utile pour tout programmeur Java.

##Comment faire

Pour lire un fichier texte en utilisant Java, vous pouvez suivre ces étapes simples:

1. Ouvrez le fichier en utilisant la classe `File` et `FileReader`. Vous pouvez également spécifier l'encodage du fichier en utilisant `Charset`.
```Java
File file = new File("monfichier.txt");
FileReader fileReader = new FileReader(file, Charset.forName("UTF-8"));
```

2. Enveloppez le `FileReader` avec un `BufferedReader` pour améliorer les performances de lecture. Vous pouvez également lire le fichier ligne par ligne avec la méthode `readLine()` jusqu'à ce que vous atteigniez la fin du fichier.
```Java
BufferedReader bufferedReader = new BufferedReader(fileReader);
String line;
while ((line = bufferedReader.readLine()) != null) {
  // Faites quelque chose avec la ligne lue
}
```

3. Enfin, n'oubliez pas de fermer le `BufferedReader` pour libérer les ressources après avoir terminé la lecture.
```Java
bufferedReader.close();
```

##Plongée en profondeur

Outre les étapes de base pour la lecture d'un fichier texte, il existe également de nombreuses options pour personnaliser votre méthode de lecture. Vous pouvez utiliser des classes telles que `Scanner` pour lire des données formatées à partir du fichier ou `LineNumberReader` pour suivre le numéro de ligne de chaque ligne lue.

Il est également essentiel de comprendre différentes erreurs possibles lors de la lecture d'un fichier, telles que les exceptions `FileNotFoundException` et `IOException`. Veillez à les gérer correctement dans votre code pour éviter les plantages inattendus.

##Voir aussi

- [Tutoriel sur la lecture et l'écriture de fichiers en Java](https://www.tutorialspoint.com/java/io/java_io_filewriter.htm)
- [Documentation officielle sur la classe File](https://docs.oracle.com/javase/8/docs/api/java/io/File.html)
- [Exemples de code pour la lecture de fichiers en Java](https://www.programiz.com/java-programming/examples/read-file)