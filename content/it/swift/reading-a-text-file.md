---
title:                "Swift: Leggere un file di testo"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Stai cercando un modo semplice e veloce per leggere un file di testo in Swift? In questo articolo ti spiegheremo come farlo utilizzando semplici esempi di codice.

## Come farlo

Per leggere un file di testo in Swift, segui questi semplici passi:

1. Usa la funzione `String(contentsOfFile:)` per leggere il contenuto del file come una stringa.
2. Se necessario, utilizza la funzione `components(separatedBy:)` per suddividere la stringa in righe separate.
3. Puoi quindi utilizzare `for ... in` o `forEach` per iterare attraverso le righe e utilizzare il contenuto nel modo desiderato.

Ecco un esempio di codice che legge il contenuto di un file di testo e lo stampa a console:

```Swift
if let fileURL = Bundle.main.url(forResource: "fileDiTesto", withExtension: "txt") {
    do {
        let fileContents = try String(contentsOf: fileURL)
        let righe = fileContents.components(separatedBy: "\n")
        righe.forEach { riga in
            print(riga)
        }
    } catch {
        print("Errore durante la lettura del file: \(error)")
    }
}
```

Ecco un esempio di output per un file di testo con contenuto "Ciao mondo!\nQuesto è un esempio\ndi file di testo":

```
Ciao mondo!
Questo è un esempio
di file di testo
```

## Approfondimento

Oltre a leggere il contenuto di un file di testo, ci sono anche altre cose che si possono fare. Ad esempio, puoi utilizzare la classe `FileManager` per gestire i file e le cartelle sul tuo dispositivo iOS. Puoi anche utilizzare `String(contentsOf:usedEncoding:)` per controllare l'encoding del file e decodificare correttamente il contenuto.

## Vedi anche

- [Documentazione di Apple su la classe `String`](https://developer.apple.com/documentation/foundation/filemanager)
- [Documentazione di Apple su la classe `FileManager`](https://developer.apple.com/documentation/foundation/nsstring/1416621-init)
- [Documentazione di Apple su l'utilizzo di encoding nei file di testo](https://developer.apple.com/library/archive/qa/qa1235/_index.html)