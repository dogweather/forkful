---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:28:11.853228-07:00
description: "Comment faire : Le package New I/O (NIO) de Java (`java.nio.file`) offre\
  \ une approche plus polyvalente pour traiter avec les fichiers. Voici une mani\xE8\
  re\u2026"
lastmod: '2024-03-13T22:44:57.663128-06:00'
model: gpt-4-0125-preview
summary: Le package New I/O (NIO) de Java (`java.nio.file`) offre une approche plus
  polyvalente pour traiter avec les fichiers.
title: "R\xE9diger un fichier texte"
weight: 24
---

## Comment faire :


### Utiliser `java.nio.file` (Bibliothèque Standard)
Le package New I/O (NIO) de Java (`java.nio.file`) offre une approche plus polyvalente pour traiter avec les fichiers. Voici une manière simpliste d'écrire dans un fichier en utilisant `Files.write()`:

```java
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;

public class TextFileWriterNIO {
    public static void main(String[] args) {
        List<String> lignes = Arrays.asList("Ligne 1", "Ligne 2", "Ligne 3");
        try {
            Files.write(Paths.get("exemple.txt"), lignes);
            System.out.println("Fichier écrit avec succès !");
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
```

Sortie :

```
Fichier écrit avec succès !
```

### Utiliser `java.io` (Bibliothèque Standard)
Pour une approche plus traditionnelle, `java.io.FileWriter` est un bon choix pour écrire simplement des fichiers textes :

```java
import java.io.FileWriter;
import java.io.IOException;

public class TextFileWriterIO {
    public static void main(String[] args) {
        try (FileWriter writer = new FileWriter("exemple.txt")) {
            writer.write("Bonjour, Monde !\n");
            writer.append("Ceci est une autre ligne.");
            System.out.println("Fichier écrit avec succès !");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Sortie :

```
Fichier écrit avec succès !
```

### Utiliser Apache Commons IO
La bibliothèque Apache Commons IO simplifie de nombreuses opérations, y compris l'écriture de fichiers. Voici comment écrire dans un fichier en utilisant `FileUtils.writeStringToFile()` :

D'abord, ajoutez la dépendance à votre projet. Si vous utilisez Maven, incluez :

```xml
<dependency>
  <groupId>org.apache.commons</groupId>
  <artifactId>commons-io</artifactId>
  <version>2.11.0</version> <!-- Vérifiez la dernière version -->
</dependency>
```

Ensuite, utilisez le code suivant pour écrire du texte dans un fichier :

```java
import org.apache.commons.io.FileUtils;
import java.io.File;
import java.io.IOException;

public class TextFileWriterCommonsIO {
    public static void main(String[] args) {
        try {
            FileUtils.writeStringToFile(new File("exemple.txt"), "Ceci est du texte écrit en utilisant Commons IO.", "UTF-8");
            System.out.println("Fichier écrit avec succès !");
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

Sortie :

```
Fichier écrit avec succès !
```
