---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:37.364465-07:00
description: 'Come fare: La libreria standard di Swift include tutti gli strumenti
  necessari per scrivere file di testo. Ecco un approccio di base.'
lastmod: '2024-03-13T22:44:43.788217-06:00'
model: gpt-4-0125-preview
summary: La libreria standard di Swift include tutti gli strumenti necessari per scrivere
  file di testo.
title: Scrivere un file di testo
weight: 24
---

## Come fare:


### Usando la Libreria Standard di Swift
La libreria standard di Swift include tutti gli strumenti necessari per scrivere file di testo. Ecco un approccio di base:

```swift
import Foundation

let content = "Ciao, lettori di Wired! Imparare Swift è divertente."
let filePath = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let fileName = "\(filePath)/esempio.txt"

do {
    try content.write(toFile: fileName, atomically: false, encoding: String.Encoding.utf8)
    print("File scritto con successo")
} catch let error as NSError {
    print("Errore nella scrittura all'URL: \(fileName), Errore: " + error.localizedDescription)
}
```

Questo frammento di codice scrive una stringa in un file chiamato `esempio.txt` nella directory dei documenti. Gestisce errori potenziali usando la gestione degli errori do-try-catch di Swift.

### Usare FileManager per Maggior Controllo
Per maggior controllo sugli attributi del file o per verificare se il file esiste già, si può utilizzare `FileManager`:

```swift
import Foundation

let fileManager = FileManager.default
let directories = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
if let documentDirectory = directories.first {
    let fileURL = documentDirectory.appendingPathComponent("esempio.txt")
    let content = "Esplorare Swift per la gestione dei file è illuminante."

    if fileManager.fileExists(atPath: fileURL.path) {
        print("Il file esiste già")
    } else {
        do {
            try content.write(to: fileURL, atomically: true, encoding: .utf8)
            print("File creato e scritto con successo")
        } catch {
            print("Errore nella scrittura del file: \(error)")
        }
    }
}
```

### Usando Librerie di Terze Parti
Una popolare libreria di terze parti per le operazioni sui file system in Swift è `Files` di John Sundell:

Prima, aggiungi Files al tuo progetto, solitamente tramite Swift Package Manager.

```swift
// swift-tools-version:5.3
import PackageDescription

let package = Package(
    name: "IlTuoNomeDelPacchetto",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "IlTuoNomeDelTarget",
            dependencies: ["Files"]),
    ]
)
```

Poi, usala per scrivere su un file:

```swift
import Files

do {
    let file = try File(path: "/percorso/alla/tua/directory/esempio.txt")
    try file.write(string: "Swift e la libreria Files formano una combinazione potente.")
    print("File scritto con successo usando la libreria Files.")
} catch {
    print("Si è verificato un errore: \(error)")
}
```

Con la libreria `Files`, la gestione dei file diventa più semplice, permettendoti di concentrarti sulla logica di business della tua applicazione piuttosto che sulle noie della gestione dei file.
