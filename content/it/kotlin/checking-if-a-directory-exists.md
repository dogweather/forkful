---
title:                "Kotlin: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, mentre si sta scrivendo un programma in Kotlin, ci si trova nella necessità di gestire file e cartelle sul sistema operativo. Una delle operazioni più comuni è la verifica dell'esistenza di una directory, in modo da evitare errori durante l'esecuzione del programma. In questa guida vedremo come fare e perché è importante farlo.

## Come fare

Per verificare se una directory esiste, possiamo utilizzare la funzione `exists()` della classe `File` di Kotlin. Vediamo un esempio pratico:

```Kotlin
val directory = File("/path/to/directory")
if (directory.exists()) {
  print("La directory esiste!")
}
```

In questo esempio, stiamo creando un oggetto `File` che rappresenta la directory "directory" nel percorso specificato. Successivamente, utilizziamo il metodo `exists()` che ritorna `true` se la directory esiste, altrimenti `false`. Se il metodo ritorna `true`, allora possiamo procedere con le operazioni che dobbiamo eseguire sulla directory. 

Possiamo anche gestire i casi in cui la directory non esista utilizzando la logica degli `if` e degli `else`, ad esempio mostrando un messaggio di errore o creando la directory se non esiste.

```Kotlin
val directory = File("/path/to/directory")
if (directory.exists()) {
  print("La directory esiste!")
} else {
  print("La directory non esiste, creandola...")
  directory.mkdirs()
}
```

In questo secondo esempio, stiamo controllando se la directory esiste e, se non esiste, la stiamo creando utilizzando il metodo `mkdirs()`.

## Approfondimento

Ora che sappiamo come verificare l'esistenza di una directory, è importante capirne il funzionamento e come utilizzarla al meglio nel nostro codice. In termini tecnici, la funzione `exists()` scala su tutti i livelli di directory a partire dalla radice del file system finché non trova la directory specificata. Se trova la directory, il metodo ritorna `true`, altrimenti `false`.

Oltre a ciò, è importante tenere a mente che la funzione `exists()` non verifica solo l'esistenza di una directory, ma anche di un file. Quindi, se il percorso specificato è un file e non una directory, il metodo ritorna comunque `true`. È importante prestare attenzione a questo quando si utilizza la funzione nel nostro codice.

## Vedi anche

- Documentazione ufficiale di Kotlin su `File` e `exists()`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/exists.html
- Tutorial su come gestire file e directory in Kotlin: https://medium.com/@maxirosson/managing-files-and-directories-in-kotlin-for-android-developers-2c0aa83ec045?source=rss-41830ef6954d------2
- Esempi pratici di utilizzo di `exists()` in un progetto Kotlin: https://www.programcreek.com/java-api-examples/?code=Hexworks/Bonfire%2FBonfire-master%2Fcli%2Fsrc%2Fmain%2Fkotlin%2Forg%2Fhexworks%2Fbonfire%2Fcli%2FBonfireCLI.kt#