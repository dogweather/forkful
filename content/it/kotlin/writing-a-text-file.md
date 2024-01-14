---
title:                "Kotlin: Scrittura di un file di testo."
programming_language: "Kotlin"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo è un'attività fondamentale nella programmazione, in quanto permette di creare e modificare un documento contenente informazioni utili per il nostro codice.

## Come fare

Per poter scrivere un file di testo in Kotlin, useremo la classe `FileWriter` e il suo costruttore che accetta il percorso del file come argomento. Dopo aver creato un'istanza di `FileWriter`, useremo il metodo `write()` per inserire il testo all'interno del file. Infine, utilizzeremo il metodo `close()` per chiudere il file e salvare le nostre modifiche.

```Kotlin
val filePath = "percorso/del/file.txt"
val fileWriter = FileWriter(filePath)
fileWriter.write("Questo è un esempio di testo all'interno del file!")
fileWriter.close()
```

Una volta eseguito il codice, troveremo il nostro file di testo nel percorso specificato, contenente il testo che abbiamo inserito.

## Approfondimento

La classe `FileWriter` offre anche la possibilità di specificare se si vuole scrivere in append o sovrascrivere il contenuto del file esistente. Per farlo, basta aggiungere un booleano come secondo argomento nel costruttore, ad esempio:

```Kotlin
val fileWriter = FileWriter(filePath, true) // scrive in append
```

Inoltre, è possibile specificare il charset del file utilizzando il costruttore con tre argomenti:

```Kotlin
val fileWriter = FileWriter(filePath, CharSet.forName("UTF-8"))
```

Infine, per garantire la corretta gestione delle risorse, è consigliato utilizzare il blocco `use` al posto del metodo `close()`:

```Kotlin
FileWriter(filePath).use {
    it.write("Questo testo viene scritto nel file")
}
```

## Vedi anche

- [Documentazione di FileWriter in Java](https://docs.oracle.com/javase/7/docs/api/java/io/FileWriter.html)
- [Tutorial su come scrivere un file utilizzando Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-for-py/writing-files.html)