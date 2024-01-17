---
title:                "Verifica dell'esistenza di una directory"
html_title:           "Kotlin: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Controllare se una directory esiste è un'operazione comune che i programmatori eseguono per assicurarsi che i file siano presenti prima di accedervi. Questo è importante per evitare errori e crash nel programma.

## Come:

```Kotlin
val dir = File("/percorso/della/directory")

if (dir.exists()) {
  println("La directory esiste!")
} else {
  println("La directory non esiste.")
}

// Output: La directory esiste!

```

```Kotlin
val dir = File("/percorso/non/esistente")

if (dir.exists()) {
  println("La directory esiste!")
} else {
  println("La directory non esiste.")
}

// Output: La directory non esiste.
```

## Deep Dive:

Controllare se una directory esiste è stato un problema comune nella programmazione sin dai primi tempi dei sistemi operativi. Una delle alternative è utilizzare il comando "ls" nel terminale e controllare l'output, ma questo è considerato una pratica obsoleta e poco efficiente. L'implementazione di questa operazione in Kotlin utilizza la classe "File" dalla libreria standard, che fornisce una varietà di metodi per lavorare con file e directory. Inoltre, la classe "Paths" può essere utilizzata per creare oggetti di tipo "Path" che forniscono ulteriori metodi di utilità per lavorare con percorsi di file e directory.

## Vedi anche:

- La documentazione ufficiale di Kotlin sulla classe "File": https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/index.html
- La documentazione ufficiale di Kotlin sulla classe "Paths": https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.nio.-paths/index.html
- Un articolo su come lavorare con file e directory in Kotlin: https://medium.com/better-programming/working-with-files-and-directories-in-kotlin-26db4c8fecfb