---
title:                "Verifica se una directory esiste"
html_title:           "Kotlin: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/kotlin/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?
Verificare se una directory esiste è l'azione di controllare la presenza di una cartella specifica nell'infrastruttura del file del sistema. Questo è fondamentale per prevenire errori nel codice quando cerchiamo di accedere o modificare file in una cartella che potrebbe non essere presente.

## Come si Fa:
Ecco come verificare se una directory esiste in Kotlin:
```Kotlin
import java.nio.file.Files
import java.nio.file.Paths

fun main() {
    val directoryPath = Paths.get("percorso/alla/tua/directory")

    if (Files.exists(directoryPath)) {
        println("La directory esiste.")
    } else {
        println("La directory non esiste.")
    }
}
```
Se la directory esiste, verrà visualizzato "La directory esiste." Altrimenti, vedrai: "La directory non esiste."

## Approfondimenti
**Contesto storico**: Nei primi giorni della programmazione, non tutti i sistemi operativi supportavano la nozione di directory, o cartelle. Oggi, è diventato uno standard e i linguaggi moderni come Kotlin forniscono api semplici per interagire con loro.

**Alternative**: Un'altra funzione che puoi usare è `Files.notExists(path)`. Questa funzione restituisce true se il file non esiste, il che può essere più utile in alcuni contesti.

**Dettagli di implementazione**: quando chiami `Files.exists(path)`, sotto il cofano, la JVM effettua una chiamata al sistema operativo per ottenere le informazioni sul file. Questo può causare un ritardo se il sistema operativo è occupato, quindi è meglio minimizzare il numero di tali chiamate.

## Vedi Anche:
- Documentazione ufficiale su File I/O in Kotlin: https://kotlinlang.org/docs/io-and-file-ops.html
- Documentazione Java su NIO.2 File API, che viene utilizzata in Kotlin: https://docs.oracle.com/javase/tutorial/essential/io/fileio.html