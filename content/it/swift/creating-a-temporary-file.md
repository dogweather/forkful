---
title:    "Swift: Creazione di un file temporaneo"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo è un'operazione utile per gli sviluppatori Swift quando è necessario archiviare temporaneamente dei dati durante l'esecuzione di un programma.

## Come fare

Per creare un file temporaneo in Swift, è necessario utilizzare la classe `FileManager`. Iniziamo creando un oggetto di questa classe:

```Swift
let fileManager = FileManager.default
```

Successivamente, definiamo il percorso in cui desideriamo creare il file temporaneo utilizzando la funzione `URLForDirectory`:

```Swift
let directoryURL = fileManager.urls(for: .cachesDirectory, in: .userDomainMask).first!
```

In questo esempio, abbiamo scelto di archiviare il file nella directory dei file di cache, ma è possibile scegliere qualsiasi altra directory adeguata per le proprie esigenze.

Una volta scelto il percorso, utilizziamo la funzione `createTemporaryFile` della classe `FileManager` per creare il nostro file temporaneo:

```Swift
let temporaryFileURL = fileManager.createTemporaryFile(directory: directoryURL)
```

Una volta creato il file, è possibile utilizzarlo normalmente come qualsiasi altro file all'interno del proprio codice. Una volta che il file non è più necessario, è possibile eliminarlo utilizzando la funzione `removeItem` della classe `FileManager`:

```Swift
do {
    try fileManager.removeItem(at: temporaryFileURL)
} catch {
    print("Errore durante l'eliminazione del file temporaneo")
}
```

## Approfondimento

La classe `FileManager` offre molte altre funzioni utili per la gestione dei file, come ad esempio la possibilità di verificare l'esistenza di un file, di copiarlo o di spostarlo in una nuova posizione.

Inoltre, esistono alcune librerie open-source che semplificano ulteriormente la creazione e la gestione di file temporanei, come ad esempio la libreria [TemporaryFile](https://github.com/nnnnzs/TemporaryFile).

## Vedi anche

- [La documentazione ufficiale di Apple sulla classe FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [La libreria TemporaryFile su GitHub](https://github.com/nnnnzs/TemporaryFile)