---
title:    "Java: Stampa dell'output di debug"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/java/printing-debug-output.md"
---

{{< edit_this_page >}}

# Perché

Nel mondo della programmazione, siamo costantemente alle prese con l'eliminazione di bug e la risoluzione di errori. In questi casi, la stampa di output di debug può essere uno strumento incredibilmente utile per comprendere il funzionamento del nostro codice e trovare le cause dei problemi.

# Come farlo

Per stampare output di debug in Java, possiamo utilizzare il metodo System.out.println(). Questo metodo accetta come argomento una stringa che verrà stampata a video. Vediamo un esempio:

```Java
int x = 10;
System.out.println("Il valore di x è: " + x);
```

Una volta eseguito il codice, vedremo l'output seguente nella nostra console:

```
Il valore di x è: 10
```

Possiamo anche usare il metodo System.out.print() per stampare senza andare a capo alla fine della stringa. Inoltre, possiamo anche utilizzare i caratteri di escape come "\n" per andare a capo e "\t" per aggiungere una tabulazione.

```Java
String nome = "Mario";
System.out.print("Ciao,");
System.out.print(nome);
System.out.print("!");
```

L'output di questo codice sarà:

```
Ciao,Mario!
```

# Approfondimento

Oltre a semplicemente stampare variabili e stringhe, possiamo anche sfruttare l'output di debug per capire il flusso del nostro codice e individuare eventuali errori. Ad esempio, possiamo stampare dei messaggi all'interno di condizioni if o cicli for per capire se determinate parti del nostro codice vengono eseguite o meno.

```Java
for (int i = 0; i < 5; i++) {
    // stampiamo solo i numeri pari
    if (i % 2 == 0) {
        System.out.println("Il valore di i è: " + i);
    }
}
```

L'output di questo codice sarà:

```
Il valore di i è: 0
Il valore di i è: 2
Il valore di i è: 4
```

In questo modo possiamo verificare se il nostro ciclo viene eseguito nel modo in cui ci aspettiamo e individuare eventuali errori di logica.

# Vedi anche

- Documentazione di Java: https://docs.oracle.com/en/java/
- Tutorial di output di debug in Java: https://www.baeldung.com/java-print-debug-statement
- Debugging in Java: https://code.tutsplus.com/tutorials/how-to-debug-java-applications--cms-26809