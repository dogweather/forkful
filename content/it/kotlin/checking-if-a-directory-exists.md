---
title:    "Kotlin: Verifica dell'esistenza di una cartella"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Controllare se una directory esiste è una pratica comune nella programmazione quando si lavora con file e cartelle. In Kotlin, ci sono alcune funzioni utili che ci permettono di verificare facilmente l'esistenza di una directory.

## Come fare

Per controllare se una directory esiste in Kotlin, possiamo utilizzare la funzione `exists()` della classe `File`. Questa funzione restituisce un valore booleano che indica se la directory esiste o meno. Ecco un esempio di codice che utilizza questa funzione:

```Kotlin
// Importiamo la classe File
import java.io.File

// Definiamo il percorso della directory che vogliamo controllare
val directoryPath = "/path/to/directory"

// Creiamo un oggetto File con il percorso della directory
val directory = File(directoryPath)

// Utilizziamo la funzione exists() per verificare l'esistenza della directory
val exists = directory.exists()

// Stampiamo il risultato
println("La directory esiste? $exists")

```

Output:

```
La directory esiste? true
```

Possiamo anche utilizzare la funzione `isDirectory()` della classe `File` per verificare se il percorso specificato corrisponde effettivamente a una directory. Questa funzione restituisce un valore booleano che indica se il percorso è una directory o meno. Ecco un esempio di codice che utilizza entrambe le funzioni:

```Kotlin
// Importiamo la classe File
import java.io.File

// Definiamo il percorso della directory che vogliamo controllare
val directoryPath = "/path/to/directory"

// Creiamo un oggetto File con il percorso della directory
val directory = File(directoryPath)

// Utilizziamo le funzioni exists() e isDirectory() per verificare l'esistenza e il tipo della directory
val exists = directory.exists()
val isDirectory = directory.isDirectory()

println("La directory esiste? $exists")
println("Il percorso specificato è una directory? $isDirectory")

```

Output:

```
La directory esiste? true
Il percorso specificato è una directory? true
```

## Approfondimento

Oltre alle funzioni `exists()` e `isDirectory()`, ci sono altre modalità per controllare se una directory esiste in Kotlin. Una di queste è l'utilizzo del metodo `listFiles()` della classe `File`, che restituisce un array contenente i file e le cartelle all'interno della directory specificata. Se questo metodo restituisce un valore `null`, significa che la directory non esiste. Possiamo quindi utilizzare una condizione `if` per verificare l'esistenza o meno della directory.

```Kotlin
// Importiamo la classe File
import java.io.File

// Definiamo il percorso della directory che vogliamo controllare
val directoryPath = "/path/to/directory"

// Creiamo un oggetto File con il percorso della directory
val directory = File(directoryPath)

// Utilizziamo il metodo listFiles() per ottenere un array dei files e cartelle all'interno della directory
val fileList = directory.listFiles()

// Verifichiamo se il metodo restituisce null per determinare l'esistenza della directory
if (fileList == null) {
    println("La directory non esiste")
} else {
    println("La directory esiste")
}

```

Output:

```
La directory esiste
```

## Vedi anche

- [Documentazione su File in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/java.io.-file/)
- [Tutorial su Kotlin per la gestione dei file](https://www.tutorialkart.com/kotlin/kotlin-file-handling-create-read-write-files/)