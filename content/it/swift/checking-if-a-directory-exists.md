---
title:                "Swift: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Se sei un programmatore Swift, potresti trovarti spesso a dover lavorare con file e directory all'interno del tuo codice. Potrebbe quindi essere utile sapere come verificare se una directory esiste o meno. In questo articolo scoprirai come farlo in modo semplice ed efficace.

## Come farlo

Per verificare se una directory esiste, utilizzeremo la classe `FileManager` di Swift. Ecco un esempio di codice:

```Swift
let fileManager = FileManager.default
let directoryPath = "/Users/nome_utente/Documenti/"
var isDirectory: ObjCBool = false
if fileManager.fileExists(atPath: directoryPath, isDirectory: &isDirectory) {
    if isDirectory.boolValue {
        print("La directory esiste.")
    } else {
        print("Il percorso specificato non è una directory.")
    }
} else {
    print("La directory non esiste.")
}
```

In questo codice, creiamo un'istanza di `FileManager` e assegniamo alla costante `directoryPath` il percorso della directory che vogliamo verificare. Utilizziamo quindi il metodo `fileExists(atPath:)` per verificare se il file o la directory esiste. Se la directory esiste, il valore di `isDirectory` verrà impostato su `true`, altrimenti su `false`.

## Approfondimento

Oltre al metodo `fileExists(atPath:)`, la classe `FileManager` offre altri metodi utili per gestire file e directory. Ad esempio, possiamo utilizzare il metodo `fileExists(atPath:)` per verificare se un file esiste, oppure `createDirectory(atPath:withIntermediateDirectories:attributes:)` per creare una nuova directory.

## Vedi anche

- [Documentazione di FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Verificare l'esistenza di un file o di una directory in Swift](https://www.hackingwithswift.com/example-code/system/how-to-check-whether-a-file-exists-using-filemanager)

Grazie per aver letto questo articolo su come verificare l'esistenza di una directory in Swift. Spero ti sia stato d'aiuto!