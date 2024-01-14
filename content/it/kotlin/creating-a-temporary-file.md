---
title:    "Kotlin: Creazione di un file temporaneo"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché creare un file temporaneo

Creare un file temporaneo è un'operazione comune nei linguaggi di programmazione. Spesso è necessario salvare dei dati temporanei durante l'esecuzione di un programma o per manipolare file senza apportare modifiche permanenti. Creare un file temporaneo è un modo semplice e sicuro per gestire questi dati e mantenerli separati da quelli permanenti.

## Come creare un file temporaneo con Kotlin

Per creare un file temporaneo con Kotlin, è possibile utilizzare la classe `File.createTempFile()` seguita dal nome desiderato per il file. Ad esempio:

```Kotlin
val tmpFile = File.createTempFile("temp", ".txt")
```

Questa istruzione creerà un file temporaneo con il nome "temp" e l'estensione ".txt". È anche possibile specificare la directory in cui si desidera creare il file temporaneo utilizzando il secondo parametro della funzione `createTempFile()`. Inoltre, è possibile utilizzare la funzione `deleteOnExit()` per assicurarsi che il file venga eliminato automaticamente una volta che il programma termina.

## Approfondimento: creare un file temporaneo personalizzato

Per creare un file temporaneo personalizzato, è possibile utilizzare la classe `TempFile` di Kotlin, che offre maggiore flessibilità rispetto alla classe `File` predefinita. Ad esempio:

```Kotlin
val tmpFile = TempFile("temp", ".txt", directory = "myTempFiles", deleteOnExit = false)
```

In questo esempio, stiamo creando un file temporaneo con il nome "temp" e l'estensione ".txt" nella directory "myTempFiles". Inoltre, stiamo specificando che il file non deve essere eliminato automaticamente alla chiusura del programma impostando il parametro `deleteOnExit` su `false`.

## Vedi anche
- Documentazione ufficiale Kotlin per la classe File: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/
- Documentazione ufficiale Kotlin per la classe TempFile: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-temp-file/