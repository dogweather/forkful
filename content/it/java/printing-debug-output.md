---
title:                "Java: Stampa dell'output di debug"
programming_language: "Java"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare l'output di debug può sembrare un'operazione semplice e banale per molti programmatori, ma in realtà è un'attività fondamentale per risolvere bug e problemi nel codice. Con la corretta implementazione, la stampa degli output di debug può essere uno strumento potente per individuare e risolvere errori.

## Come fare

Ci sono diversi modi per stampare output di debug in Java, ma il più comune è utilizzare il metodo `System.out.println()`. Questo metodo prende in input una stringa o un oggetto e lo stampa sulla console. Ecco un esempio di codice che stampa una variabile di tipo `int`:

``` Java
int numero = 5;
System.out.println(numero);
```

L'output di questo codice sarà:

```
5
```

Possiamo anche utilizzare il metodo `System.out.printf()` per stampare output di debug in modo più formattato. Questo metodo permette di specificare il tipo di dato e il formato di stampa. Ecco un esempio che stampa un numero decimale con due cifre dopo la virgola:

``` Java
double decimale = 3.1415;
System.out.printf("%.2f", decimale);
```

L'output di questo codice sarà:

```
3.14
```

## Approfondimento

Oltre ai due metodi sopra menzionati, ci sono anche altre opzioni per stampare output di debug in Java. Per esempio, possiamo utilizzare il logger della libreria `java.util.logging` per ottenere output più dettagliati e controllare il livello di log di ogni messaggio. Possiamo anche utilizzare il debugger integrato in una IDE per eseguire il codice passo dopo passo e vedere i valori delle variabili in ogni istante.

È importante ricordare che dopo aver risolto i bug, è buona pratica rimuovere o commentare gli output di debug dal codice finale in modo da non appesantirlo eccessivamente.

## Vedi anche

- [Documentazione ufficiale di Java per il metodo System.out.println](https://docs.oracle.com/javase/8/docs/api/java/lang/System.html#println-java.lang.String-)
- [Tutorial su come utilizzare il logger in Java](https://www.vogella.com/tutorials/Logging/article.html)
- [Guida all'utilizzo del debugger in Eclipse](https://www.eclipse.org/community/eclipse_newsletter/2017/august/article1.php)