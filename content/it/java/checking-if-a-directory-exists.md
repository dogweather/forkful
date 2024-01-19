---
title:                "Verifica se una directory esiste"
html_title:           "Java: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Controllare se una Directory Esiste in Java

## Cos'è e Perché?

Controllare se una directory esiste è l'atto di verificazione del fatto che una particolare cartella sia presente nel file system. I programmatori lo fanno per evitare errori durante l'esecuzione del codice, ad esempio, quando si tenta di leggere o scrivere in una directory inesistente.

## Come fare:

In Java, usiamo la classe `java.nio.file.Files` e il metodo `exists()` per controllare se una directory esiste. Ecco un breve esempio di codice:

```Java
import java.nio.file.Files;
import java.nio.file.Paths;

public class Main {
    public static void main(String[] args) {
        if(Files.exists(Paths.get("/path/to/your/directory"))) {
            System.out.println("La directory esiste");
        } else {
            System.out.println("La directory non esiste");
        }
    }
}
```
Se la directory esiste, questo codice stamperà "La directory esiste", altrimenti "La directory non esiste".

## Approfondimento:

La possibilità di controllare se una directory esiste è un'aggiunta relativa ai tempi recenti alla programmazione, ed è tipica degli ambienti di sviluppo moderni come Java.

Java fornisce anche metodi alternativi, ad esempio `Files.notExists()`, che restituisce `true` se il percorso non esiste. Tuttavia, `Files.exists()` rimane la scelta più popolare, in quanto consente una programmazione più intuitiva.

Da un punto di vista dell'implementazione, `Files.exists()` usa le API del sistema operativo sottostante per determinare se il percorso esiste o meno. Questo significa che lo stato della directory viene effettivamente verificato e non viene usata nessuna cache.

## Guarda anche:

Ci sono molte altre risorse disponibili per approfondire l'uso di `Files` e `Paths` in Java. Ecco alcuni collegamenti utili:

- La documentazione ufficiale Oracle su [java.nio.file.Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html) e [java.nio.file.Paths](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Paths.html)
  
- Un articolo utilissimo sul [lavoro con i file in Java](https://www.baeldung.com/java-io)
  
- Una guida dettagliata sulla [gestione dei file e delle directory in Java](https://www.journaldev.com/851/java-copy-file)