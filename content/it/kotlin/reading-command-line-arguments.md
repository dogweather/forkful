---
title:                "Kotlin: Leggere gli argomenti della riga di comando"
simple_title:         "Leggere gli argomenti della riga di comando"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Spesso, quando scriviamo programmi, abbiamo bisogno di leggere delle informazioni in ingresso. Queste informazioni possono essere inserite tramite la riga di comando e per questo è importante saperle gestire correttamente. In questo articolo vedremo come leggere gli argomenti della riga di comando utilizzando Kotlin.

## Come fare

Per leggere gli argomenti della riga di comando in Kotlin, possiamo utilizzare la classe `args` che ci fornisce una lista di stringhe con tutti gli argomenti passati al nostro programma. Vediamo un semplice esempio:

```Kotlin
fun main(args: Array<String>) {
    // Stampiamo gli argomenti passati alla riga di comando
    for (arg in args) {
        println(arg)
    }
}
```

Se ad esempio eseguiamo il programma passando come argomenti "ciao" e "mondo":

```Kotlin
$ kotlin MyClass.kt ciao mondo
```

Otterremo in output:

```
ciao
mondo
```

Possiamo anche accedere ad un argomento specifico utilizzando la sintassi `args[posizione]` dove `posizione` indica la posizione dell'argomento nella riga di comando, partendo da 0. Ad esempio, se vogliamo stampare solo il secondo argomento:

```Kotlin
fun main(args: Array<String>) {
    // Stampiamo solo il secondo argomento passato alla riga di comando
    println(args[1])
}
```

Se eseguiamo il programma con gli stessi argomenti di prima, otterremo in output:

```
mondo
```

## Approfondimento

Oltre alla semplice lettura degli argomenti della riga di comando, possiamo anche controllare il numero di argomenti passati utilizzando la proprietà `size` della classe `args` e gestire eventuali errori. Ad esempio:

```Kotlin
fun main(args: Array<String>) {
    // Controlliamo che siano stati passati almeno 2 argomenti
    if (args.size < 2) {
        println("Errore: devono essere passati almeno 2 argomenti")
    } else {
        // Stampiamo gli argomenti passati
        for (arg in args) {
            println(arg)
        }
    }
}
```

Vediamo quindi che è importante gestire in modo corretto gli argomenti della riga di comando, soprattutto quando si lavora con programmi più complessi.

## Vedi anche

Se vuoi approfondire ulteriormente l'argomento, ti consiglio di dare un'occhiata a questi articoli:

- [Documentazione ufficiale di Kotlin - Command Line Arguments](https://kotlinlang.org/docs/command-line.html)
- [Tutorial di Medium - How to Read Command Line Arguments in Kotlin](https://medium.com/@kotlindroid/how-to-read-command-line-arguments-in-kotlin-b19291513eee)