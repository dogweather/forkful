---
date: 2024-01-20 17:56:14.846737-07:00
description: "Leggere gli argomenti della riga di comando significa accedere ai parametri\
  \ passati al tuo programma quando viene avviato da una shell. I programmatori lo\u2026"
lastmod: 2024-02-19 22:05:02.385218
model: gpt-4-1106-preview
summary: "Leggere gli argomenti della riga di comando significa accedere ai parametri\
  \ passati al tuo programma quando viene avviato da una shell. I programmatori lo\u2026"
title: Lettura degli argomenti della riga di comando
---

{{< edit_this_page >}}

## Cosa & Perché?
Leggere gli argomenti della riga di comando significa accedere ai parametri passati al tuo programma quando viene avviato da una shell. I programmatori lo fanno per personalizzare l’esecuzione del programma o per specificarne la configurazione senza dover cambiare il codice.

## Come fare:
Ecco un esempio su come accogliere gli argomenti della riga di comando in Java:

```java
public class CommandLineExample {
    public static void main(String[] args) {
        if (args.length > 0) {
            System.out.println("Ecco i tuoi argomenti:");
            for (String arg : args) {
                System.out.println(arg);
            }
        } else {
            System.out.println("Nessun argomento fornito.");
        }
    }
}
```

Per eseguirlo:

```bash
java CommandLineExample Ciao Mondo
```

Risultato:
```
Ecco i tuoi argomenti:
Ciao
Mondo
```

## Approfondimento
Letture degli argomenti da riga di comando data da anni; è una pratica comune in linguaggi come C e Python, oltre a Java. In Java, l'array `args` del metodo `main` contiene questi argomenti. Non dimenticare: i valori sono sempre `String`, anche numeri. Per altre opzioni, considera librerie come Apache Commons CLI o JCommander per parsing più avanzato.

Dettagli d'implementazione: ogni elemento dell'array `args` corrisponde a un argomento. "args[0]" è il primo argomento dopo il nome della classe. Se hai bisogno di convertire stringhe in altri tipi, usa classi come `Integer.parseInt` o simili.

## Vedi Anche
- [Documentazione Oracle sui Parametri della Riga di Comando](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- [Apache Commons CLI](https://commons.apache.org/proper/commons-cli/)
- [JCommander](http://jcommander.org/)
