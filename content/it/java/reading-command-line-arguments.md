---
title:    "Java: Leggere gli argomenti della riga di comando"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/java/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Spesso quando scriviamo un programma vogliamo avere la possibilità di personalizzarlo al momento dell'esecuzione. Una delle modalità più comuni per farlo è utilizzare gli "argomenti della linea di comando" (command line arguments). In questo articolo esploreremo come leggere e manipolare questi argomenti all'interno di un programma Java.

## Come Fare

Per leggere gli argomenti della linea di comando, dobbiamo utilizzare il parametro `args` nel metodo `main`, in questo modo:

```Java
public static void main(String[] args) {
    // codice del programma
}
```

Questo parametro è un array di stringhe che contiene i valori degli argomenti passati al momento dell'esecuzione del programma. Possiamo quindi accedere a questi argomenti utilizzando l'indice dell'array, ad esempio `args[0]` per il primo argomento, `args[1]` per il secondo, e così via.

Per capire meglio come funziona, vediamo un esempio pratico. Supponiamo di avere un programma che accetta due argomenti: un nome e un'età. Possiamo stampare questi valori utilizzando il seguente codice:

```Java
public static void main(String[] args) {
    System.out.println("Ciao " + args[0] + ", hai " + args[1] + " anni.");
}
```

Se eseguiamo questo programma con i seguenti argomenti: `Mario 35`, l'output sarà: `Ciao Mario, hai 35 anni.`

## Approfondimento

Oltre a leggere gli argomenti della linea di comando, possiamo anche manipolarli e convertirli in altri tipi di dati. Ad esempio, se vogliamo convertire l'età in un intero, possiamo utilizzare il metodo `Integer.parseInt()`:

```Java
int age = Integer.parseInt(args[1]);
```

Inoltre, è possibile controllare il numero di argomenti passati utilizzando `args.length`. In questo modo possiamo gestire eventuali errori nel caso in cui l'utente non abbia fornito il numero corretto di argomenti.

## Vedi Anche

- [Documentazione Java su command line arguments](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/Process.html)
- [Tutorial su come leggere argomenti della linea di comando in Java](https://www.baeldung.com/java-command-line-arguments)