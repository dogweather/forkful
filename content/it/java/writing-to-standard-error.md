---
title:                "Java: Scrivere su output di errore standard"
programming_language: "Java"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Perché scrivere su standard error

Scrivere su standard error è un'abilità fondamentale per ogni programmatore Java. Quando si scrive su standard error, si invia un messaggio di errore o avviso al sistema di output invece di stamparlo sullo standard output. Questo può essere utile per il debug o per indicare errori critici durante l'esecuzione del programma.

## Come farlo

Per scrivere su standard error in Java, è possibile utilizzare la classe System.err e il suo metodo println(). Questo metodo accetta un argomento di tipo String che verrà stampato su standard error.

```Java
System.err.println("Questo è un messaggio di errore!");
```

L'output di questo codice sarà:

```Java
Questo è un messaggio di errore!
```

## Approfondimento

Scrive su standard error ha anche dei vantaggi rispetto alla semplice stampa su standard output. Ad esempio, se si utilizza un sistema di redirect output su un file, gli errori verranno comunque visualizzati sullo schermo, mentre gli standard output verranno scritti solo nel file di output.

È importante notare che standard error è diverso da standard output e viene solitamente visualizzato sullo schermo in un colore diverso, come il rosso. Questo può aiutare a distinguere gli errori dagli output del programma durante il debug.

Inoltre, è possibile utilizzare la classe System.setErr() per impostare uno stream di output diverso per standard error, ad esempio un file di log o una finestra di dialogo.

# Vedi anche

- [Java System Class Documentazione](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html)
- [Java Stream Class Documentazione](https://docs.oracle.com/javase/8/docs/api/java/io/PrintStream.html)