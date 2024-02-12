---
title:                "Création d'un fichier temporaire"
aliases:
- /fr/java/creating-a-temporary-file.md
date:                  2024-01-20T17:40:33.673913-07:00
model:                 gpt-4-1106-preview
simple_title:         "Création d'un fichier temporaire"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et Pourquoi?)
Créer un fichier temporaire en Java, c'est générer un fichier pour des données éphémères. Les programmeurs le font souvent pour tester des morceaux de code ou gérer des données sensibles qui ne doivent pas rester sur le disque dur indéfiniment.

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
