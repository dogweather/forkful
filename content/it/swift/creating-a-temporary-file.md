---
title:    "Swift: Creazione di un file temporaneo"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo è un'utile pratica di programmazione per gestire dati temporanei o di cui si ha bisogno solo per una breve durata. Invece di utilizzare lo spazio di memoria permanente del dispositivo, è possibile creare un file temporaneo che verrà eliminato automaticamente una volta terminato il suo utilizzo.

## Come fare

Per creare un file temporaneo in Swift, è necessario utilizzare la classe `FileManager`. Ecco un esempio di codice che crea un file temporaneo e scrive al suo interno un array di numeri:

```Swift
let fileManager = FileManager.default
let tempDir = URL(fileURLWithPath: NSTemporaryDirectory())
let tempFile = tempDir.appendingPathComponent("myTempFile.txt")

do {
    try [1, 2, 3, 4, 5].write(to: tempFile, atomically: true, encoding: .utf8)
} catch {
    print("Errore durante la scrittura del file temporaneo: \(error)")
}
```

Una volta eseguito il codice, è possibile trovare il file temporaneo nella directory temporanea del dispositivo.

## Approfondimento

Creare un file temporaneo è una buona pratica di sicurezza, in quanto garantisce la protezione dei dati sensibili. Inoltre, questa tecnica può anche essere utile per gestire situazioni di memoria insufficiente su dispositivi a bassa capacità.

## Vedi anche

- [Apple documentation on temporary files](https://developer.apple.com/documentation/foundation/filemanager/1407696-createuniquefile)
- [Using temporary files in Swift](https://www.hackingwithswift.com/example-code/system/how-to-create-a-temporary-file-securely-using-contents-offilesystem)
- [File System Programming Guide by Apple](https://developer.apple.com/library/archive/documentation/FileManagement/Conceptual/FileSystemProgrammingGuide/Introduction/Introduction.html)