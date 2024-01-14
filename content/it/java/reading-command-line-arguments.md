---
title:                "Java: Lettura degli argomenti della riga di comando."
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

##Perché
Spesso nella programmazione, siamo chiamati ad interagire con l'utente per ottenere dati o input. Una delle tecniche utilizzate per ottenere dati in modo dinamico è attraverso l'utilizzo di  comandi della linea di comando. In questo post, esploreremo come leggere questi argomenti della linea di comando in Java e impareremo ad utilizzarli efficacemente nei nostri programmi.

##Come fare
Per leggere gli argomenti della linea di comando in Java, possiamo utilizzare l'oggetto args del metodo main. In questo oggetto sono memorizzati tutti gli argomenti passati al programma al momento dell'esecuzione.

Ecco un esempio pratico:

```Java
public class Main {
  public static void main(String[] args) {
    System.out.println("Hai inserito " + args.length + " argomenti.");

    for (int i = 0; i < args.length; i++) {
      System.out.println("Argomento " + (i+1) + ": " + args[i]);
    }
  }
}
```

Nel codice sopra, stiamo semplicemente stampando il numero di argomenti passati e il loro valore.

Supponiamo di eseguire il programma con i seguenti argomenti dalla linea di comando: "Java Main arg1 arg2 arg3". L'output del programma sarà il seguente:

```
Hai inserito 3 argomenti.
Argomento 1: arg1
Argomento 2: arg2
Argomento 3: arg3
```

Possiamo anche utilizzare gli argomenti della linea di comando per impostare valori in variabili all'interno del programma, evitando così di doverli dichiarare manualmente nel codice. Ad esempio, nel caso di una calcolatrice, potremmo passare due numeri come argomenti in input per eseguire un'operazione.

##Deep dive
Oltre all'oggetto args, esiste anche un'altra classe che possiamo utilizzare per gestire gli argomenti della linea di comando in Java, ovvero la classe Scanner. Utilizzando questa classe, possiamo leggere argomenti specifici dalla linea di comando in modo più strutturato, ad esempio indicando un tipo di dato specifico (intero, stringa, float, ecc.).

Ecco un esempio di utilizzo della classe Scanner:

```Java
import java.util.Scanner;

public class Main {
  public static void main(String[] args) {
    Scanner sc = new Scanner(System.in);

    System.out.println("Inserisci il tuo nome:");
    String nome = sc.next();

    System.out.println("Inserisci la tua età:");
    int eta = sc.nextInt();

    System.out.println("Ciao, " + nome + ", hai " + eta + " anni!");
  }
}
```

In questo esempio, dopo aver compilato il programma, verrà richiesto all'utente di inserire il proprio nome e la propria età. Tali valori verranno poi stampati a schermo.

##Vedi anche
- [Documentazione ufficiale di Java sugli argomenti della linea di comando](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Tutorial su come leggere gli argomenti della linea di comando in Java](https://www.guru99.com/command-line-arguments.html)
- [Esempi di utilizzo di Scanner per leggere gli argomenti dalla linea di comando in Java](https://www.baeldung.com/java-command-line-arguments)