---
title:                "Scrivere un file di testo"
html_title:           "Kotlin: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo è un'operazione comune nella programmazione. Può essere utile per creare file di configurazione, archiviare dati o esportare risultati.

## Come fare

Per scrivere un file di testo in Kotlin, dobbiamo prima importare la classe `java.io.File`. Dopo di che, possiamo utilizzare il metodo `printWriter()` per creare un oggetto `PrintWriter` che ci permetterà di scrivere nel file.

```Kotlin
import java.io.File

fun main() {

    // Creiamo un oggetto File specificando il percorso e il nome del file
    val file = File("test.txt")

    // Creiamo un oggetto PrintWriter
    val writer = file.printWriter()

    // Scriviamo una stringa nel file
    writer.println("Questo è un esempio di file di testo scritto in Kotlin.")
    
    // Possiamo anche utilizzare il metodo write() per scrivere carattere per carattere
    writer.write("Questo è un altro esempio.")

    // Il metodo println() inserisce automaticamente un carattere di nuova riga
    // Possiamo utilizzare il metodo print() per scrivere senza andare a capo
    writer.print("Questo è un altro esempio senza andare a capo.")

    // Ricordiamoci sempre di chiudere il PrintWriter
    writer.close()

    // Possiamo anche specificare il percorso e il nome del file nel metodo printWriter()
    File("test2.txt").printWriter().use { writer ->
        writer.println("Questo è un altro esempio di file di testo.")
    }
}
```

Se eseguiamo questo codice, verranno creati due file di testo nella directory del progetto.

Contenuto del file "test.txt":

```
Questo è un esempio di file di testo scritto in Kotlin.
Questo è un altro esempio.
Questo è un altro esempio senza andare a capo.
```

Contenuto del file "test2.txt":

```
Questo è un altro esempio di file di testo.
```

## Approfondimento

Oltre al metodo `printWriter()`, possiamo utilizzare anche altri metodi della classe `java.io.File` per scrivere in un file di testo. Ad esempio, il metodo `writeText()` ci permette di scrivere una stringa nel file in una sola riga di codice.

```Kotlin
File("test.txt").writeText("Questo è un esempio utilizzando il metodo writeText().")
```

Oltre alla scrittura, la classe `java.io.File` ci permette anche di leggere il contenuto di un file utilizzando metodi come `readText()` e `readLines()`. Inoltre, è possibile specificare il formato di encoding del file utilizzando il parametro opzionale del metodo `printWriter()`.

## Vedi anche

- [Documentazione ufficiale su File I/O in Kotlin](https://kotlinlang.org/docs/tutorials/kotlin-for-py/file-io.html)
- [Come creare e scrivere su un file di testo in Kotlin](https://www.tutorialkart.com/kotlin/create-write-kotlin-text-file/)
- [Come leggere un file di testo in Kotlin](https://www.baeldung.com/kotlin/read-file)