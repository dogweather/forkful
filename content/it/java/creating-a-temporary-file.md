---
title:                "Creazione di un file temporaneo"
html_title:           "Java: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo è utile quando si desidera memorizzare temporaneamente dei dati tra le diverse operazioni di un programma o quando si vuole evitare conflitti tra file con lo stesso nome.

## Come creare un file temporaneo in Java

Per creare un file temporaneo in Java, è possibile utilizzare la classe `File` e il metodo `createTempFile()`. Vediamo un esempio di codice:

````java
// Importiamo la classe File
import java.io.File;

public class TempFileExample {
    public static void main(String[] args) {
        try {
            // Creiamo un file temporaneo con prefisso "temp"
            // e suffisso ".txt"
            File tempFile = File.createTempFile("temp", ".txt");
            
            // Stampiamo il percorso completo del file creato
            System.out.println("Percorso del file temporaneo: " + tempFile.getAbsolutePath());
            
            // Rimuoviamo il file temporaneo
            tempFile.delete();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
````
Output:
Percorso del file temporaneo: C:\Users\NomeUtente\AppData\Local\Temp\temp05337338816669124679.txt

## Approfondimento sulla creazione di file temporanei

I file temporanei vengono creati in una directory specifica fornita dal sistema operativo. Utilizzando il metodo `createTempFile()` senza specificare un percorso, il file verrà creato nella directory predefinita. Inoltre, il file verrà automaticamente cancellato quando il programma termina o in caso di errore.

Per specificare un percorso personalizzato, è possibile utilizzare il secondo parametro del metodo `createTempFile()`. Ad esempio, se vogliamo che il file temporaneo venga creato nella directory corrente, possiamo scrivere:

````java
File tempFile = File.createTempFile("temp", ".txt", new File("."));
````

Per eliminare manualmente un file temporaneo, è possibile utilizzare il metodo `delete()` della classe `File`. Per evitare la creazione di file duplicati, è consigliato eliminare il file appena non viene più utilizzato.

## Vedi anche
- [Documentazione ufficiale di Java su File](https://docs.oracle.com/javase/10/docs/api/java/io/File.html#createTempFile-java.lang.String-java.lang.String-java.io.File-) 
- [Tutorial su come creare e gestire file temporanei in Java](https://www.baeldung.com/java-temporary-files)