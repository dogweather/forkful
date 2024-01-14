---
title:    "Swift: Verifica dell'esistenza di una directory"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Controllo di esistenza delle cartelle è un'attività fondamentale nella programmazione iOS. Può essere utile per verificare la presenza dei file necessari per il corretto funzionamento dell'applicazione o per gestire situazioni in cui è necessario creare o eliminare una cartella.

## Come fare

Per controllare se una cartella esiste, è necessario utilizzare la classe FileManager di Swift. Con il seguente codice, è possibile verificare la presenza di una cartella chiamata "images" nella directory principale dell'applicazione:

```
let fileManager = FileManager.default
let directoryPath = NSHomeDirectory() + "/images"

if fileManager.fileExists(atPath: directoryPath) {
    print("La cartella 'images' esiste!")
} else {
    print("La cartella 'images' non esiste.")
}
```

In questo esempio, viene creata un'istanza di FileManager e viene costruito il percorso della cartella "images" nella directory principale utilizzando la costante `NSHomeDirectory()`. Successivamente, viene utilizzato il metodo `fileExists(atPath:)` per verificare se la cartella esiste. Se la condizione è vera, viene stampato un messaggio di conferma, altrimenti viene stampato un messaggio di avviso.

## Approfondimento

La classe FileManager fornisce anche altri metodi utili per gestire le cartelle, ad esempio per creare o eliminare una cartella. Inoltre, è possibile utilizzare le proprietà della classe per ottenere informazioni dettagliate su una specifica cartella, come la data di creazione o l'ultima data di modifica.

Inoltre, è importante notare che il metodo `fileExists(atPath:)` non controlla solo l'esistenza di cartelle, ma anche di file. Pertanto, è fondamentale fornire il percorso completo della cartella nella directory principale, inclusi tutti i livelli di sottocartelle.

## Vedi anche

- [Documentazione ufficiale di Apple su FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutorial su come gestire le cartelle in Swift](https://www.swiftbysundell.com/basics/filemanager/)
- [Esempio di utilizzo di FileManager in un'applicazione iOS](https://www.appcoda.com/working-files-ios/)

Grazie per aver letto questo articolo! Spero che sia stato utile per comprendere come controllare l'esistenza di una cartella in Swift. Continua a esplorare le funzionalità di FileManager per gestire in modo efficiente i file e le cartelle nella tua applicazione iOS. Arrivederci alla prossima volta!