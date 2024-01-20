---
title:                "Scrivere un file di testo"
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere un file di testo permette di salvare dati persistenti. I programmatori lo fanno per memorizzazione, configurazioni, log e scambio di dati fra applicazioni.

## Come fare:
### Scrivere un file con `PrintWriter`:
```Kotlin
import java.io.PrintWriter

fun main() {
    PrintWriter("esempio.txt").use { out ->
        out.println("Ciao, questo è un file di testo!")
    }
}
```
Output: File "esempio.txt" creato con il testo all'interno.

### Uso di `File.writeText`:
```Kotlin
import java.io.File

fun main() {
    val testo = "Ciao, questo è un file di testo!"
    File("esempio.txt").writeText(testo)
}
```
Output: File "esempio.txt" creato con il testo all'interno.

## Approfondimento
Originariamente, scrivere su file si faceva con flussi 'stream' ('InputStream'/'OutputStream') in Java. Kotlin fornisce wrapper e funzioni di estensione per rendere il processo più ergonomico, come `writeText` e `printWriter()`. Un'alternativa è l'API `Files` di NIO, utile per file di grandi dimensioni o per operazioni asincrone.

## Vedi Anche
- Documentazione ufficiale Kotlin su I/O: [kotlinlang.org](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/)
- Java NIO: [oracle.com](https://docs.oracle.com/javase/8/docs/technotes/guides/io/index.html)
- Gestione delle eccezioni in I/O file: [kotlinlang.org](https://kotlinlang.org/docs/reference/exceptions.html)