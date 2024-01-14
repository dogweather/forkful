---
title:    "Swift: Verifica dell'esistenza di una cartella"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Perché

Molte volte, quando si sviluppa un'applicazione o un software, è necessario accedere a una directory specifica nel sistema in cui viene eseguito il codice. In questi casi, è importante verificare se la directory esiste o meno prima di tentare di accedervi, per evitare errori e malfunzionamenti.

## Come fare

Per verificare se una directory esiste nel proprio codice Swift, possiamo utilizzare la funzione `FileManager.default.fileExists(atPath: String)` che restituisce un valore booleano. Vediamo un esempio di codice che verifica se una directory chiamata "Documenti" esiste sulla scrivania:
```Swift
if FileManager.default.fileExists(atPath: "/Users/nome_utente/Desktop/Documenti") {
    print("La directory esiste!")
} else {
    print("La directory non esiste.")
}
```
Se la directory esiste, il programma stamperà "La directory esiste!", altrimenti stamperà "La directory non esiste."

## Approfondimento

La funzione `fileExists(atPath: String)` controlla solo se esiste un file o una directory con il percorso specificato, non tiene conto di eventuali permessi di accesso o se il percorso stesso è un file invece di una directory. Inoltre, esiste anche un'altra funzione `fileExists(atPath: String, isDirectory: UnsafeMutablePointer<ObjCBool>)` che permette di verificare specificamente se il percorso è una directory piuttosto che un file.

## Vedi anche
- [Documentazione Apple su FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Tutorial su come gestire i file e le directory in Swift](https://www.ralfebert.de/ios/tutorials/filemanager/) (in inglese)