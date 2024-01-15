---
title:                "Lettura di un file di testo"
html_title:           "Kotlin: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché
Se sei nuovo nella programmazione o stai cercando di imparare una nuova lingua di programmazione, è importante essere a conoscenza dei concetti fondamentali come la lettura di file di testo. Questo ti permetterà di manipolare i dati all'interno del codice e creare programmi più complessi.

## Come fare
Per leggere un file di testo in Kotlin, puoi usare la funzione `readText()` insieme al metodo `File()` della libreria standard di Java. Ecco un esempio di codice:

```Kotlin
import java.io.File

fun main() {
    val file = File("file.txt") // imposta il percorso del file di testo
    val text = file.readText() // legge il contenuto del file e lo assegna alla variabile text
    println(text) // stampa il contenuto del file di testo
}
```

### Esempio di output:
```
Questo è un file di testo!
Buongiorno a tutti!
```

## Approfondimento
La funzione `readText()` legge l'intero contenuto di un file di testo come una stringa e restituisce un valore di tipo `String`. Questo rende facile manipolare e lavorare con i dati all'interno del codice. Inoltre, è possibile specificare l'encoding del file utilizzando il parametro `charset`, ad esempio `file.readText(charset = Charsets.UTF_8)`.

## Vedi anche
- [Documentazione Kotlin su lettura di file di testo](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/read-text.html)
- [Tutorial di base di Kotlin](https://kotlinlang.org/docs/tutorials/getting-started.html)
- [Documentazione Java su lettura di file di testo](https://docs.oracle.com/javase/tutorial/essential/io/file.html)