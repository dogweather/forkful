---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Reading a Text File in Java

## Quoi et Pourquoi? (What & Why?)
La lecture d'un fichier texte est une opération cruciale qui permet d'accéder au contenu dans un format lisible par l'homme. Les programmeurs l'utilisent pour manipuler, traiter et analyser des données organisées ligne par ligne.

## Comment Faire: (How to:)
Pour lire un fichier texte en Java, nous utilisons généralement la classe BufferedReader.

```Java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadFile{
    public static void main(String[] args) {
        String line;
        try {
            BufferedReader reader = new BufferedReader(new FileReader("test.txt"));
            while ((line = reader.readLine()) != null) {
                System.out.println(line);
            }
            reader.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Ce code ouvre le fichier `test.txt` et imprime chaque ligne sur la console.

## Deep Dive

La lecture de fichiers en Java a évolué au fil des années. Auparavant, nous utilisions `FileInputStream` et `DataInputStream` mais `BufferedReader` est plus efficace car il lit le texte plus rapidement grâce à un tampon intern.

Il existe plusieurs alternatives à `BufferedReader`, notamment `Scanner` et `Files.lines()` dans Java 8. `Scanner` est utile lorsque nous voulons analyser le texte, alors que `Files.lines()` est plus facile à utiliser avec les Streams en Java.

Un détail d'implémentation important à noter est que `BufferedReader` doit toujours être fermé pour éviter les fuites de mémoire. Depuis Java 7, nous pouvons utiliser le try-with-resources qui ferme automatiquement le `BufferedReader`.

## Voir Aussi: (See Also:)
[JavaDocs pour BufferedReader](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)  
[Tutoriel Oracle sur I/O](https://docs.oracle.com/javase/tutorial/essential/io/index.html)  
[Guide de Java 8 Stream](https://www.oracle.com/technical-resources/articles/java/ma14-java-se-8-streams.html)