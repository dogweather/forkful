---
title:                "Vérifier si un répertoire existe"
date:                  2024-01-20T14:56:59.241864-07:00
html_title:           "Go: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Vérifier si un répertoire existe permet de s'assurer que les fichiers sont où on pense qu'ils sont. Les programmeurs font ça pour éviter des erreurs, comme lire ou écrire dans un répertoire qui n'existe pas.

## How to:
Java nous offre `Files.exists()` pour vérifier facilement :

```java
import java.nio.file.Files;
import java.nio.file.Paths;

public class DirectoryExistsDemo {
    public static void main(String[] args) {
        String directoryPath = "/chemin/vers/le/repertoire";

        if(Files.exists(Paths.get(directoryPath))) {
            System.out.println("Le répertoire existe !");
        } else {
            System.out.println("Le répertoire n'existe pas.");
        }
    }
}
```

Si le répertoire existe, le programme sortira :
```
Le répertoire existe !
```

Sinon, il affichera :
```
Le répertoire n'existe pas.
```

## Deep Dive
Avant Java NIO (New Input/Output), on vérifiait l'existence d'un répertoire avec `File.exists()`. La NIO apporta une approche plus flexible et performante pour manipuler les systèmes de fichiers.

Alternativement, `Files.notExists(Path)` peut être utilisé pour une logique inversée. Mais attention, `Files.exists(Path)` et `Files.notExists(Path)` ne sont pas parfaitement symétriques – si les deux retournent `false`, cela ne signifie pas que le fichier est dans un état Schrödinger-esque, mais qu'il y a peut-être un problème d'accès.

Il est aussi possible de vérifier les droits d'accès en même temps avec la méthode `Files.isReadable(Path)` ou `Files.isWritable(Path)` pour les opérations de lecture et écriture respectivement.

## See Also
- Documentation Oracle sur la classe Files : https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html
- Tutoriel Oracle sur la file NIO.2 : https://docs.oracle.com/javase/tutorial/essential/io/fileio.html
- Guide d'introduction à Java NIO : https://www.baeldung.com/java-nio-2-file-api
