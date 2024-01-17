---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Kotlin: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Che cosa & Perché?
Leggere gli argomenti della riga di comando è il processo di ottenere le informazioni inserite dall'utente durante l'esecuzione del programma attraverso la riga di comando. I programmatori spesso lo fanno per personalizzare il comportamento del programma o per ottenere input dall'utente più velocemente.

# Come fare:
Un esempio di codice Kotlin per leggere gli argomenti della riga di comando potrebbe essere il seguente:
```Kotlin
fun main(args: Array<String>) {
    println("I file inseriti sono ${args.size}")
    for (arg in args) {
        println(arg)
    }
}
```
Output:
```
I file inseriti sono 3
file1.txt
file2.txt
file3.txt
```
Questo codice stampa il numero di argomenti inseriti e poi li stampa uno per uno. Possiamo quindi utilizzare gli argomenti ottenuti per eseguire azioni specifiche nel nostro programma.

# Approfondimento:
La lettura degli argomenti della riga di comando è stata una funzionalità importante sin dai primi sistemi operativi. È diventata ancora più comune con l'avvento dei linguaggi di programmazione della riga di comando, come Bash e Perl. Tuttavia, ci sono alcune alternative per ottenere input dall'utente, come l'input da tastiera o la lettura da file.

Per leggere gli argomenti della riga di comando in Kotlin, utilizziamo l'array `args` passato al metodo `main`. Possiamo anche utilizzare la libreria `kotlin-argparser` per gestire in modo più efficiente gli argomenti della riga di comando.

# Vedi anche:
- Documentazione di Kotlin sulla lettura degli argomenti della riga di comando: https://kotlinlang.org/docs/command-line.html 
- Esempi di utilizzo di Kotlin-argparser: https://github.com/xenomachina/kotlin-argparser-examples