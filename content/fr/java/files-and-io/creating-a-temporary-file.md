---
date: 2024-01-20 17:40:33.673913-07:00
description: "How to: (Comment faire : ) Java offre `Files.createTempFile` pour fabriquer\
  \ un fichier temp. Il se supprime tout seul \xE0 la fin de l'ex\xE9cution du\u2026"
lastmod: '2024-03-13T22:44:57.664358-06:00'
model: gpt-4-1106-preview
summary: Java offre `Files.createTempFile` pour fabriquer un fichier temp.
title: "Cr\xE9ation d'un fichier temporaire"
weight: 21
---

## How to: (Comment faire : )
Java offre `Files.createTempFile` pour fabriquer un fichier temp. Il se supprime tout seul à la fin de l'exécution du programme. Voici comment:

```java
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class TempFileExample {
    public static void main(String[] args) {
        try {
            // Créer un fichier temporaire
            Path temp = Files.createTempFile("monTemp", ".txt");
            
            // Affiche le chemin du fichier temporaire
            System.out.println("Fichier temporaire créé à : " + temp);

        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Sample output:

```
Fichier temporaire créé à : /tmp/monTemp1234567890.txt
```

## Deep Dive (Plongeon en profondeur)
Les fichiers temporaires remontent à l'époque des disquettes, quand l'espace était compté et qu'on voulait éviter de perdre des données. En Java, `java.io.File` a d'abord proposé de les créer, mais `java.nio.file.Files` est plus récent et plus performant, grâce à la gestion des exceptions et au support des chemins symboliques.

Le choix de l'emplacement du fichier est critique. Java stocke souvent les temporaires dans le dossier `/tmp` sous Unix ou le dossier `%TEMP%` sous Windows, mais on peut changer ça avec l'argument `-Djava.io.tmpdir`.

D'autres approches pour tester du code ou manipuler des données sans créer de fichiers physiques incluent l'utilisation de bases de données en mémoire ou de Mocks.

## See Also (Voir aussi)
- [Official Java Documentation for Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
