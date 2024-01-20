---
title:                "Stampa dell'output di debug"
html_title:           "Arduino: Stampa dell'output di debug"
simple_title:         "Stampa dell'output di debug"
programming_language: "Java"
category:             "Java"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/java/printing-debug-output.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

La stampa del debug output è un metodo utilizzato dai programmatori per tenere traccia del flusso di esecuzione del codice. Questo permette a chi sviluppa di localizzare più facilmente gli errori ("bugs") e monitorare il comportamento dell'applicazione.

## Come si fa:

Ecco un esempio semplice di come stampare debug output in Java utilizzando il metodo `System.out.println()`.

```Java
public class HelloWorld {
   public static void main(String[] args) {
      System.out.println("Inizio del programma");

      int x = 10;
      System.out.println("x = " + x);

      x = x * 2;
      System.out.println("x raddoppiato = " + x);

      System.out.println("Fine del programma");
   }
}
```

Questa applicazione produrrà il seguente output:

```
Inizio del programma
x = 10
x raddoppiato = 20
Fine del programma
```

## Approfondimento:

La stampa del debug output ha radici storiche, quando i programmatori dovevano utilizzare tecniche di basso livello per tracciare il flusso del programma. Con l'avvento dei linguaggi di alto livello e degli ambienti di sviluppo integrati (IDE), queste tecniche sono diventate molto più semplici.

Un'alternativa a `System.out.println()` in Java potrebbe essere l'utilizzo di un sistema di logging, come ad esempio Log4J. Questi strumenti offrono una maggiore flessibilità rispetto alla semplice stampa del debug output, ma possono essere eccessivi per programmi più piccoli o meno complessi.

Per quanto riguarda i dettagli di implementazione, `System.out.println()` stampa le informazioni sull'output standard (solitamente la console da cui è stato avviato il programma), seguito da una nuova riga.

## Da Vedere Anche:

Per un approfondimento sulla stampa del debug output e sulle tecniche di debug in Java, questi sono alcuni link utili:

- ["Debugging in Java"](https://docs.oracle.com/en/java/javase/16/troubleshoot/debugging.html)
- ["How to Use Log4J"](https://logging.apache.org/log4j/2.x/manual/index.html)
- ["How to Use Java Logger"](https://docs.oracle.com/javase/8/docs/api/java/util/logging/package-summary.html)