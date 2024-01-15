---
title:                "Verifica dell'esistenza di una cartella"
html_title:           "Swift: Verifica dell'esistenza di una cartella"
simple_title:         "Verifica dell'esistenza di una cartella"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Controllare l'esistenza di una directory è fondamentale quando si sta sviluppando un'applicazione in Swift. Questa operazione ci permette di verificare la presenza di una cartella che potrebbe essere necessaria per il funzionamento dell'applicazione stessa.

## Come fare

Per controllare se una directory esiste, utilizziamo il metodo `FileManager.default.fileExists(atPath:)` passando come parametro il percorso della directory che vogliamo verificare. In questo esempio, assumiamo che la cartella si trovi nella directory principale dell'applicazione.
 
```
Swift
let fileManager = FileManager.default
let directory = "Directory"
let directoryPath = NSHomeDirectory() + "/" + directory
if fileManager.fileExists(atPath: directoryPath) {
    print("La directory esiste")
} else {
    print("La directory non esiste")
}
```

Se la directory specificata esiste, il metodo restituisce `true` altrimenti restituisce `false`. Possiamo anche utilizzare il metodo `createDirectory(atPath:withIntermediateDirectories:attributes:)` per creare una directory in caso non esista già.

## Approfondimento

Questo metodo utilizza la classe `FileManager` per verificare l'esistenza di un file o di una directory. Possiamo anche utilizzare il metodo più specifico `fileExists(atPath:isDirectory:)` per verificare solo l'esistenza di una directory.

## Vedi anche

- [Apple Developer Documentation - FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift By Sundell - Checking if a file exists using Swift](https://www.swiftbysundell.com/tips/checking-if-a-file-exists-in-swift/)