---
title:                "Swift: Verifica se una directory esiste"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Perché

Controllare l'esistenza di una directory può essere utile per verificare se un percorso specifico è accessibile prima di eseguire operazioni su di esso.

## Come

Si può utilizzare il metodo `FileManager.fileExists(atPath:)` per verificare l'esistenza di una directory in un percorso specifico. Di seguito è riportato un esempio di codice che controlla l'esistenza della directory "Documents" nella cartella principale dell'applicazione:

```Swift
if FileManager.default.fileExists(atPath: FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0].path) {
    print("La directory 'Documents' esiste")
} else {
    print("La directory 'Documents' non esiste")
}
```

L'output di questo codice dipenderà da se la directory "Documents" esiste o meno.

## Approfondimento

Quando si verifica l'esistenza di una directory, è importante considerare che ciò non significa necessariamente che il percorso specifico sia valido o che la directory sia accessibile. Inoltre, è possibile che la directory venga creata o eliminata durante l'esecuzione del programma, rendendo necessario un continuo controllo dell'esistenza.

# Vedi anche
- [Documentation on FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift File and Directory Operations](https://learnappmaking.com/file-directory-swift/) (in inglese)
- [Esempi di FileManager in Swift](https://www.ioscreator.com/tutorials/file-manager-swift-tutorial) (in inglese)