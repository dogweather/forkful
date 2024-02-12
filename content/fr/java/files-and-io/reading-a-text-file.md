---
title:                "Lecture d'un fichier texte"
aliases:
- /fr/java/reading-a-text-file.md
date:                  2024-01-20T17:54:19.446497-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture d'un fichier texte"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Lire un fichier texte, c'est quoi ? C’est parcourir un fichier et en extraire des données pour les utiliser dans votre programme. Pourquoi ? Parce qu'on a souvent besoin de lire des configurations, des données externes ou du contenu à traiter.

## How to:
```Java
import java.nio.file.*;
import java.io.IOException;

public class FileReaderExample {
    public static void main(String[] args) {
        Path path = Paths.get("exemple.txt");
        
        try {
            String content = Files.readString(path);
            System.out.println("Contenu du fichier : ");
            System.out.println(content);
        } catch (IOException e) {
            System.err.println("Erreur lors de la lecture du fichier : " + e.getMessage());
        }
    }
}
```
Sortie attendue :
```
Contenu du fichier : 
Voici le texte contenu dans le fichier exemple.txt.
```

## Deep Dive
Lire des fichiers en Java est une pratique courante depuis les premières versions. Autrefois, `BufferedReader` et `FileReader` étaient souvent utilisés, mais ils nécessitaient beaucoup de code fastidieux. En Java 8, l’API `Files` a été introduite, simplifiant la lecture avec des méthodes comme `readAllLines()` ou `readString()` (Java 11). Ce changement est parti d’une volonté de rendre le code plus lisible et plus concis. Alternativement, on peut utiliser des bibliothèques comme Apache Commons IO pour gérer des cas plus complexes.

## See Also
- Documentation Oracle sur les I/O : https://docs.oracle.com/javase/tutorial/essential/io/
- API `java.nio.file.Files` : https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html
- Apache Commons IO : https://commons.apache.org/proper/commons-io/
