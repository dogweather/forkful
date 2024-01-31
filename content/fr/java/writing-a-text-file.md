---
title:                "Écriture d'un fichier texte"
date:                  2024-01-19
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"

category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? 
Écrire un fichier texte, c'est enregistrer des données en format lisible par l'homme. Les développeurs font ça pour sauvegarder des configurations, des logs, ou pour communiquer avec d'autres programmes.

## How to:
Java fournit un API simple pour écrire dans des fichiers. Voici un exemple :

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ExampleFileWriter {
    public static void main(String[] args) {
        String textToWrite = "Salut le monde!";
        String filePath = "example.txt";

        try (BufferedWriter writer = new BufferedWriter(new FileWriter(filePath))) {
            writer.write(textToWrite);
            writer.newLine(); // Ajoute une nouvelle ligne après le texte
            writer.write("Au revoir le monde!");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
Output (`example.txt`):
```
Salut le monde!
Au revoir le monde!
```

## Deep Dive
Historiquement, Java a beaucoup évolué pour simplifier l'écriture de fichiers. Alternativement, `Files.write` de Java NIO offre une approche moderne et concise. Les détails d'implémentation incluent la gestion de l'Unicode et la performance de l'écriture.

## See Also
- Documentation Oracle sur les `BufferedWriter` : https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/io/BufferedWriter.html
- Guide Oracle pour `Files.write`: https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/nio/file/Files.html#write(java.nio.file.Path,java.lang.Iterable,java.nio.charset.Charset,java.nio.file.OpenOption...)
- Tutorial sur Baeldung pour écrire des fichiers texte en Java : https://www.baeldung.com/java-write-to-file
