---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Che cosa e Perche?

Leggere un file di testo è un'operazione comune in programmazione che permette di prendere dati da un file esterno e usarli nel tuo programma. Questo è molto utile quando si lavora con grandi volumi di dati, come le configurazioni dell'applicazione o l'input dell'utente.

## Come fare:

Ecco un esempio di come leggere un file di testo con Java:

```Java
import java.nio.file.*;
import java.io.*;

public class Main {
  public static void main(String[] args) {
    try {
      String content = new String(Files.readAllBytes(Paths.get("fileDiTesto.txt")));
      System.out.println(content);
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
```
Output previsto:

```
Ecco una riga di testo.
Questa è un'altra riga di testo.
```
## Il tuffo nel profondo:

Nell'era pre-Java 7, i programmatori usavano principalmente le classi `BufferedReader` e `FileReader` per leggere i file di testo. Tuttavia, da Java 7 in poi, è stato introdotto il package `java.nio.file` che fornisce metodi più efficienti e facili da usare come `Files.readAllBytes()`.

Ci sono diverse alternative per leggere un file di testo in Java, come l'uso dello scanner o il mappaggio della memoria file. La scelta del metodo più adatto dipende dalle tue esigenze. Ad esempio, lo scanner è più adatto quando si leggono dati di input utente, mentre `Files.readAllBytes()` è più utile per leggere interi file in una volta sola.

In termini di implementazione, quando chiami `Files.readAllBytes()`, il metodo apre un flusso di input, legge tutti i byte dal file e infine chiude il flusso. Tutto questo viene fatto in un unico passaggio, risparmiando la necessità di scrivere codice supplementare per la gestione delle risorse.

## Guarda anche:

- [Java Documentation: Class Files](https://docs.oracle.com/javase/8/docs/api/java/nio/file/Files.html)
- [Java I/O Tutorial by Oracle](https://docs.oracle.com/javase/tutorial/essential/io/index.html)
- [Baeldung Guide: Java Read File](https://www.baeldung.com/java-read-file)