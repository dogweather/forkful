---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Creare un file temporaneo in Java significa costruire un file che viene utilizzato solo per un breve periodo di tempo e viene cancellato alla fine dell'uso. Questo è spesso utile quando si vogliono raccogliere i dati intermedi durante l'esecuzione di un programma in modo sicuro e senza modificare i dati di input originali.

## Come Fare:
Usiamo la classe File della libreria java.io per creare un file temporaneo. Ecco un esempio:
```Java
import java.io.File;
import java.io.IOException;

public class TemporaryFileExample {
    public static void main(String[] args) {
        try {
            // Crea file temporaneo
            File tempFile = File.createTempFile("myTempFile", ".tmp");

            // Stampa il percorso del file
            System.out.println("Il file temporaneo Path=" +tempFile.getAbsolutePath());

            // cancella il file quando il programma termina
            tempFile.deleteOnExit();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```
In questo esempio, `File.createTempFile("myTempFile", ".tmp")` crea un file temporaneo e `tempFile.deleteOnExit()` assicura che venga cancellato quando il programma termina.

## Approfondimenti:
Storicamente, i file temporanei sono stati utilizzati inizialmente in ambito mainframe per supportare il multi-tasking e per consentire un'elaborazione più efficiente dei dati. In Java, abbiamo la comodità della gestione dei file temporanei incorporata nella classe File.

Un'alternativa alla creazione di un file temporaneo potrebbe essere l'uso di una struttura dati in memoria, come un array o una lista. Tuttavia, i file temporanei sono particolarmente utili quando i dati sono troppo grandi per essere conservati interamente in memoria.

Durante la creazione di un file temporaneo, Java prima cerca il percorso di directory specificato dalla proprietà del sistema java.io.tmpdir. Se non è specificato, utilizza il percorso di directory temporanea del sistema operativo host.

## Vedi Anche:
Per ulteriori informazioni, suggerisco di consultare la [documentazione ufficiale di Java](https://docs.oracle.com/javase/7/docs/api/java/io/File.html) e [Java create temporary file tutorial](https://www.journaldev.com/9481/java-create-temporary-file).