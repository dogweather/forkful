---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-20T14:58:48.746645-07:00
html_title:           "Gleam: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
"Che cos'è e Perché?"
Controllare l'esistenza di una directory permette di sapere se un percorso nel file system contiene i dati che ci aspettiamo. Lo facciamo per evitare errori quando leggiamo, scriviamo o cancelliamo file.

## How to:
"Come Fare:"
```Swift
import Foundation

let fileManager = FileManager.default
let path = "/path/to/your/directory"

if fileManager.fileExists(atPath: path) {
    print("La directory esiste.")
} else {
    print("La directory non esiste.")
}
```
Output: `La directory esiste.` o `La directory non esiste.` a seconda del caso.

## Deep Dive:
"Approfondimento:"
Historicamente, la gestione dei file in Swift si appoggia alla libreria `Foundation`, introdotta da Apple per Objective-C e poi portata in Swift. Un'alternativa è usare le syscall di sistema UNIX direttamente tramite le API di basso livello in Swift. Tuttavia, `FileManager` è più alto livello, più sicuro e più facile da usare. Quando controlliamo l'esistenza di una directory, il sistema effettua un'operazione di I/O, che può essere costosa: usalo con giudizio.

## See Also:
"Vedi Anche:"
- Documentazione Apple su `FileManager`: [Documentazione FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- Discussioni sull'uso efficiente delle API di file system su Stack Overflow: [Stack Overflow File System API](https://stackoverflow.com/questions/tagged/file-system+swift)
