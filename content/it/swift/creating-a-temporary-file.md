---
title:                "Swift: Creazione di un file temporaneo"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare file temporanei è utile quando si desidera memorizzare dati temporaneamente durante l'esecuzione di un programma. Questi file possono essere utilizzati per svolgere attività specifiche, come la memorizzazione di dati temporanei, la gestione dei download o la manipolazione di file temporanei.

## Come fare

Per creare un file temporaneo in Swift, è necessario utilizzare la classe FileManager e il suo metodo `temporaryDirectory`. Di seguito è riportato un esempio di codice che crea e scrive un file temporaneo:

```Swift
let fileManager = FileManager.default
let tempDir = fileManager.temporaryDirectory
let tempFile = tempDir.appendingPathComponent("temp.txt")

do {
    try "Questo è un file temporaneo".write(to: tempFile, atomically: true, encoding: .utf8)
} catch {
    print("Impossibile scrivere il file temporaneo: \(error)")
}
```

Il codice sopra crea un file temporaneo chiamato "temp.txt" nella directory temporanea del sistema e vi scrive il testo "Questo è un file temporaneo". Il file viene creato in modo sicuro con il flag `atomically` impostato su `true`, che garantisce che il file venga salvato in modo corretto anche in caso di errori.

## Approfondimento

Esistono diversi altri modi per creare e gestire file temporanei in Swift. Ad esempio, si possono utilizzare i metodi `mkstemp` e `mkdtemp` della libreria C per creare file temporanei con nomi univoci. Inoltre, esistono anche diverse librerie open source che semplificano la gestione dei file temporanei in Swift, come ad esempio la libreria `Files`.

Inoltre, è importante tenere a mente che i file temporanei devono essere eliminati dopo averli utilizzati per evitare di occupare spazio di archiviazione inutilmente. È possibile farlo utilizzando il metodo `removeItem(atPath:)` della classe FileManager.

## Vedi anche

- [Documentazione di FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Libreria Files per la gestione dei file in Swift](https://github.com/JohnSundell/Files)
- [Tutorial su come creare file temporanei in Swift](https://techblog.badoo.com/blog/2017/11/02/the-right-way-to-do-temporary-files-in-ios/)