---
title:                "Kotlin: Leggere gli argomenti della riga di comando"
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Lettura degli argomenti della riga di comando in Kotlin è una parte importante della programmazione, poiché consente di interagire con il programma in modo più dinamico e personalizzato.

## Come fare

Per leggere gli argomenti della riga di comando in Kotlin, si può utilizzare la funzione `main()` con il parametro `args: Array<String>` che rappresenta gli argomenti forniti all'avvio del programma. Di seguito un esempio di codice che stampa gli argomenti ricevuti:

```Kotlin
fun main(args: Array<String>) {
    println("Gli argomenti forniti sono: ")
    for (arg in args) {
        println(arg)
    }
}
```

Esempio di output:
```
Gli argomenti forniti sono:
arg1
arg2
```

## Approfondimento

Per utilizzare gli argomenti della riga di comando in modo più avanzato, si possono utilizzare le funzioni disponibili nella libreria `kotlin.system`, come ad esempio `args.drop(n)` per scartare i primi `n` argomenti, `args.contains("valore")` per verificare se un determinato valore è presente tra gli argomenti o ancora `args.joinToString()` per convertirli in una stringa separata da uno specifico delimitatore.

## Vedi anche

- Documentazione ufficiale di Kotlin sulla lettura degli argomenti della riga di comando: https://kotlinlang.org/docs/tutorials/command-line.html
- Tutorial su come utilizzare gli argomenti della riga di comando in Kotlin: https://www.baeldung.com/kotlin/command-line-arguments