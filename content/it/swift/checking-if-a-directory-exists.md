---
title:                "Verifica dell'esistenza di una directory"
html_title:           "Swift: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Verificare se una directory esiste è un'operazione comune per i programmatori. Si tratta di un controllo che determina se una specifica cartella è presente nel sistema di file o meno. Ciò è utile per garantire che il codice funzioni correttamente e per gestire al meglio la gestione dei file all'interno del programma.

## Come fare:
Per verificare se una directory esiste in Swift, è possibile utilizzare il metodo ```fileExists``` della classe FileManager. Questo metodo accetta come parametro una stringa contenente il percorso della directory da controllare e restituisce un valore booleano che indica se la directory esiste o meno. Ecco un esempio di codice:

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users/io/Documenti/"

if fileManager.fileExists(atPath: directoryPath) {
    print("La directory esiste!")
} else {
    print("La directory non esiste.")
}
```
Output: La directory esiste!

## Approfondimento:
La verifica dell'esistenza di una directory è particolarmente importante in sistemi operativi multitasking, in cui più applicazioni possono accedere allo stesso file contemporaneamente. In alternativa al metodo ```fileExists```, è possibile utilizzare i metodi ```fileExists(atPath:)``` e ```fileExists(atURL:)```. Inoltre, è anche possibile utilizzare il metodo ```isReadableFile(atPath:)``` per verificare se si ha il permesso di leggere una determinata directory.

## Vedi anche:
Per ulteriori informazioni sull'utilizzo dei metodi della classe FileManager, si può fare riferimento alla documentazione ufficiale di Apple: [FileManager Class Reference](https://developer.apple.com/documentation/foundation/filemanager).