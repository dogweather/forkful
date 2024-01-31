---
title:                "Lettura degli argomenti della riga di comando"
date:                  2024-01-20T17:56:16.459090-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lettura degli argomenti della riga di comando"

category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Leggere gli argomenti della riga di comando in Kotlin significa prelevare dati inseriti dall'utente al momento dell'esecuzione di un programma. È utile perché permette ai programmi di essere flessibili e adattabili alle esigenze dell'utente.

## How to:
Ecco un esempio base per leggere gli argomenti dalla riga di comando in Kotlin:

```Kotlin
fun main(args: Array<String>) {
    if (args.isNotEmpty()) {
        println("Hey! Ecco il tuo primo argomento: ${args[0]}")
    } else {
        println("Hey! Non hai fornito argomenti.")
    }
}
```

Se esegui questo codice con `kotlin MyAppKt argomento1`, l'output sarà:

```
Hey! Ecco il tuo primo argomento: argomento1
```

## Deep Dive
Nei primi tempi della programmazione, l'interazione con i programmi era principalmente attraverso la riga di comando. Oggi, nonostante le interfacce grafiche, leggere gli argomenti della riga di comando rimane cruciale per gli script e per i programmi che richiedono configurazioni al volo.

Alternativamente, si potrebbero usare file di configurazione o variabili d'ambiente, ma per la semplicità e l'immediatezza, gli argomenti della riga di comando hanno ancora il loro posto.

Kotlin, essendo interoperabile con Java, utilizza lo stesso metodo per accedere agli argomenti della riga di comando che si trova nei programmi Java. Gli argomenti sono passati come array di stringhe alla funzione `main`, e si accede a essi attraverso gli indici dell'array, come `args[0]`.

## See Also
Ecco alcuni link utili per approfondire:

- La documentazione ufficiale di Kotlin sulla funzione `main`: [Kotlin Lang - Main Function](https://kotlinlang.org/docs/command-line.html#run-the-application)
- Guida agli argomenti da riga di comando in Java, utile perché Kotlin li gestisce in modo simile: [Oracle - Command-Line Arguments](https://docs.oracle.com/javase/tutorial/essential/environment/cmdLineArgs.html)
- Una panoramica su come interagire con la riga di comando in generale: [CommandLine Tutorial](https://www.codecademy.com/articles/command-line-setup)
