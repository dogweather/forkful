---
title:                "Creazione di un file temporaneo."
html_title:           "Swift: Creazione di un file temporaneo."
simple_title:         "Creazione di un file temporaneo."
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo è un'operazione comune quando si lavora con dati temporanei o si desidera eseguire operazioni senza alterare i file originali.

## Come

Per creare un file temporaneo in Swift, è necessario utilizzare la classe FileManager e il metodo FileManager.temporaryDirectory (corrispondente alla funzione tmpdir () in altri linguaggi di programmazione) per ottenere la directory temporanea del sistema operativo. Quindi, è possibile creare un nuovo file nella directory temporanea utilizzando il metodo FileManager.createFile (corrispondente alla funzione fopen () in altri linguaggi di programmazione). Ecco un esempio di codice che crea un file temporaneo e scrive una stringa al suo interno:

```Swift
let fileManager = FileManager.default
let tempDirectory = fileManager.temporaryDirectory

do {
    let filePath = tempDirectory.appendingPathComponent("myFile.txt")
    try fileManager.createFile(atPath: filePath.path, contents: nil, attributes: nil)
    let fileHandle = try FileHandle(forWritingTo: filePath)
    let string = "Hello World!"
    fileHandle.write(string.data(using: .utf8)!)
    fileHandle.closeFile()
} catch {
    print("Error creating temporary file: \(error)")
}
```

L'output sarà un file di testo "myFile.txt" creato nella directory temporanea del sistema operativo, contenente la stringa "Hello World!".

## Approfondimento

Quando si crea un file temporaneo, è importante tenere conto della sua durata e della gestione della memoria. In Swift, le istanze delle classi FileManager e FileHandle verranno automaticamente deallocate quando non sono più in uso, ma è buona pratica chiudere il fileHandle in modo esplicito utilizzando il metodo closeFile () per evitare problemi di memoria. Inoltre, è possibile specificare un valore a nil per il parametro "contents" del metodo FileManager.createFile () per creare un file vuoto, oppure è possibile passare un oggetto Data contenente i dati che si desidera scrivere nel file temporaneo.

## Vedi anche

- [Documentazione ufficiale di Swift su FileManager] (https://developer.apple.com/documentation/foundation/filemanager)
- [Uso di FileHandle per scrivere e leggere dati su file in Swift] (https://www.swiftbysundell.com/articles/using-filehandles-to-read-and-write-data-in-swift/)