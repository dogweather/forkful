---
title:    "Java: Leggere un file di testo"
keywords: ["Java"]
---

{{< edit_this_page >}}

## Perché leggere un file di testo in Java

Lettura di file di testo è una delle attività più comuni in programmazione Java, specialmente quando si lavora con grandi quantità di dati o si desidera importare dati esterni nel proprio programma. Leggere un file di testo può essere utile anche per l'analisi di dati, la generazione di report o l'elaborazione di testi.

## Come fare per leggere un file di testo in Java

Per leggere un file di testo in Java, è necessario seguire alcuni semplici passaggi:

1. Aprire il file utilizzando la classe `File` e il suo costruttore `File(String pathname)`, specificando il percorso del file.
2. Creare un oggetto `Scanner` che leggerà il file aperto. Utilizzando il metodo `hasNextLine()`, è possibile verificare se il file contiene ancora righe da leggere.
3. Utilizzare il metodo `nextLine()` per leggere una riga alla volta dal file e salvarla in una variabile.
4. Chiudere il file utilizzando il metodo `close()` per evitare memory leak e liberare le risorse utilizzate.

Ecco un esempio di codice che legge un file di testo contenente una lista di nomi e li stampa a schermo:

```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class LeggiFile {

    public static void main(String[] args){
        File file = new File("lista_nomi.txt");
        try {
          Scanner scanner = new Scanner(file);
          while (scanner.hasNextLine()){
              String nome = scanner.nextLine();
              System.out.println(nome);
          }
          scanner.close();
        } catch (FileNotFoundException e) {
            System.out.println("File non trovato!");
        }
    }
}
```

Ecco l'output del codice precedente:

```
Maria
Luca
Giulia
Andrea
```

## Approfondimento: leggere un file di testo con Java
Oltre al metodo descritto in precedenza, esistono altre opzioni per leggere un file di testo in Java. Ad esempio, è possibile utilizzare la classe `BufferedReader` per leggere il file in modo più efficiente o la classe `FileReader` se si desidera lavorare con caratteri invece di stringhe.

Inoltre, è possibile specificare un charset (set di caratteri) per gestire correttamente caratteri speciali o accenti all'interno del file.

Scegliere il metodo più adatto dipenderà dalle esigenze del proprio progetto e dalle prestazioni desiderate.

## Vedi anche
- [Documentazione ufficiale Java sulla classe File](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)
- [Documentazione ufficiale Java sulla classe Scanner](https://docs.oracle.com/javase/7/docs/api/java/util/Scanner.html)
- [Tutorial su come leggere un file di testo in Java](https://www.baeldung.com/java-read-file)