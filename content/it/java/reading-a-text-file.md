---
title:    "Java: Lettura di un file di testo"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo è un'operazione comune nella programmazione Java. Con questo semplice processo, puoi estrarre informazioni da un file di testo e utilizzarle nel tuo programma. Questo blog post spiegherà come leggere un file di testo utilizzando Java e fornirà informazioni più dettagliate sul processo.

## Come

Per leggere un file di testo con Java, devi seguire questi passaggi:

1. Apri il file di testo utilizzando la classe `File`.
2. Converti il file in un'istanza della classe `Scanner` per eseguire la lettura.
3. Utilizza il metodo `hasNextLine()` per verificare se ci sono ancora righe da leggere.
4. Se il metodo restituisce `true`, usa il metodo `nextLine()` per leggere la riga successiva.
5. Chiudi il file utilizzando il metodo `close()`.

Ecco un esempio di codice che legge un file di testo contenente una lista di nomi e stampa ogni nome su una nuova riga:

```Java
import java.io.File;
import java.io.IOException;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        try {
            // Apri il file di testo
            File file = new File("nomi.txt");
            // Converti il file in un'istanza della classe Scanner
            Scanner scanner = new Scanner(file);
            // Utilizza un ciclo while per leggere ogni riga del file
            while (scanner.hasNextLine()) {
                // Leggi la riga successiva e stampala
                String nome = scanner.nextLine();
                System.out.println(nome);
            }
            // Chiudi il file
            scanner.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

L'output di questo esempio sarà:

```
Maria
Luigi
Giulia
Marco
```

## Approfondimento

Esistono diversi modi per leggere un file di testo con Java. Una delle opzioni più comuni è utilizzare la classe `BufferedReader`, che offre maggiori funzionalità rispetto alla classe `Scanner`.

Inoltre, è importante considerare l'encoding del file di testo, ovvero il modo in cui i caratteri sono memorizzati all'interno del file. Se il file è in un encoding diverso da quello di default utilizzato dal sistema, potresti ottenere risultati inaspettati durante la lettura del file. Per specificare l'encoding del file, puoi utilizzare il costruttore della classe `Scanner` che accetta un parametro `charset`:

```Java
Scanner scanner = new Scanner(file, "UTF-8");
```

In alternativa, puoi utilizzare un'istanza della classe `InputStreamReader` per leggere il file utilizzando un encoding specifico:

```Java
InputStreamReader reader = new InputStreamReader(new FileInputStream(file), "UTF-8");
```

In entrambi i casi, è importante gestire correttamente le eccezioni che possono essere generate durante la lettura del file.

## Vedi anche

- Java File Input/Output: https://docs.oracle.com/javase/tutorial/essential/io/file.html
- Classe Scanner Java: https://docs.oracle.com/javase/7/docs/api/java/util/Scanner.html
- Classe BufferedReader Java: https://docs.oracle.com/javase/7/docs/api/java/io/BufferedReader.html