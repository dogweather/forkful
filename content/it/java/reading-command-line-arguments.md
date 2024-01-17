---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

Leggere gli argomenti dalla riga di comando è un'operazione comune nel linguaggio di programmazione Java. Gli argomenti dalla riga di comando sono gli input che un programma riceve da un utente durante l'esecuzione. I programmatori spesso utilizzano questa funzionalità per rendere i propri programmi più interattivi e personalizzabili per l'utente finale.

## Come fare:

Per leggere gli argomenti dalla riga di comando in Java, è possibile utilizzare la classe ```Scanner```, che permette di leggere input da diverse fonti, inclusi gli argomenti dalla riga di comando. Di seguito è riportato un esempio di codice che legge un argomento dalla riga di comando e lo stampa a schermo:

```java
import java.util.Scanner;

public class Main {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        // Legge il primo argomento dalla riga di comando
        String input = scanner.next();
        // Stampa l'argomento letto
        System.out.println("Il tuo input è: " + input);
    }
}
```

Esempio di esecuzione del programma con input "Ciao mondo!" dalla riga di comando:

```
$ java Main Ciao mondo!
Il tuo input è: Ciao
```

## Approfondimento:

La possibilità di leggere gli argomenti dalla riga di comando è stata introdotta in Java nella versione 1.0, pubblicata nel 1996. Oltre all'uso della classe ```Scanner```, è anche possibile utilizzare la classe ```BufferedReader``` o l'interfaccia ```CommandListener``` per leggere gli argomenti dalla riga di comando.

## Vedi anche:

- [Java Scanner Class](https://docs.oracle.com/javase/8/docs/api/java/util/Scanner.html)
- [Java BufferedReader Class](https://docs.oracle.com/javase/8/docs/api/java/io/BufferedReader.html)
- [Java CommandListener Interface](https://docs.oracle.com/javase/8/docs/api/java/io/CommandListener.html)