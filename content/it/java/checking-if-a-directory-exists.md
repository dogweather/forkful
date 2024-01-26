---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-20T14:56:43.986257-07:00
html_title:           "Gleam: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

Verificare l'esistenza di una directory significa controllare se una specifica cartella è presente sul file system. I programmatori eseguono questa operazione per evitare errori durante la lettura/scrittura dei file o per decidere se creare una nuova directory o meno.

## Come Fare:

Per verificare l'esistenza di una directory in Java, possiamo usare il metodo `exists()` della classe `java.nio.file.Files` in combinazione con la classe `java.nio.file.Paths`.

```java
import java.nio.file.*;

public class DirectoryExists {
    public static void main(String[] args) {
        Path path = Paths.get("/percorso/alla/directory");
        
        boolean directoryExists = Files.exists(path);
        
        if (directoryExists) {
            System.out.println("La directory esiste.");
        } else {
            System.out.println("La directory non esiste.");
        }
    }
}
```

Output:
```
La directory esiste.
```
o
```
La directory non esiste.
```

## Approfondimento

Prima di Java 7, per verificare l'esistenza di una directory, spesso si usava il metodo `exists()` della classe `java.io.File`. Questo approccio è ancora valido, ma `java.nio.file` è più moderno e offre funzioni più potenti. Una considerazione importante è che `Files.exists()` non garantisce l'atomicità nelle operazioni di file: ci possono essere cambiamenti nel file system tra la verifica e l'eventuale operazione successiva.

In alternativa, per verificare l'esistenza e anche controllare i permessi, si può usare `Files.isDirectory()` che conferma l'esistenza e verifica se il path è una directory.

```java
boolean isDirectory = Files.isDirectory(path);
```

Un'altra pratica comune nei contesti multi-thread o concorrenti è gestire le eccezioni invece di usare i controlli preventivi, affidandosi al principio EAFP (Easier to Ask for Forgiveness than Permission).

## Vedi Anche

- [Documentazione ufficiale `java.nio.file.Files`](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/nio/file/Files.html)
- [Documentazione ufficiale `java.io.File`](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Tutorial sul file I/O in Java](https://docs.oracle.com/javase/tutorial/essential/io/)
