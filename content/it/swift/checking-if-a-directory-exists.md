---
title:                "Verifica se una directory esiste"
html_title:           "Lua: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Swift"
category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Controllare se una directory esiste è un'operazione comune nell'ambito della programmazione. Questo permette ai programmatori di evitare errori fatale nel caso in cui tentino di accedere ad una directory che non esiste.

## Come fare:

Ecco un esempio di come verificare se una directory esiste in Swift:

```Swift
import Foundation

let fileManager = FileManager.default
let directory = "/path/to/directory"

if fileManager.fileExists(atPath: directory) {
    print("La directory esiste!")
} else {
    print("La directory non esiste!")
}
```

Questo codice verifica se la directory esiste e stampa un messaggio a seconda dei risultati.

## Deep Dive:

Nella programmazione storica, quando i sistemi di filesystem erano meno affidabili, verificare se una directory esiste era ancora più critico. Oggi, abbiamo a disposizione molti strumenti per gestire questi problemi, ma è ancora una buona abitudine.

Un'alternativa per controllare se una directory esiste in Swift potrebbe essere l'uso di `attributesOfItem(atPath:)` anziché `fileExists(atPath:)`, ma quest'ultimo è più semplice e diretto.

Riguardo l'implementazione, `fileExists(atPath:)` verifica non solo l'esistenza del file, ma distingue anche tra file e directory. Se ambigue impostazioni di permessi o collegamenti simbolici creano confusione, il metodo restituirebbe `false`.

## Vedi Anche:

Per approfondire la tua conoscenza in relazione a `FileManager` in Swift, consulta la documentazione ufficiale di Apple:
- [FileManager - Apple Developer Documentation](https://developer.apple.com/documentation/foundation/filemanager)
- [How to check if a file exists - Stack Overflow](https://stackoverflow.com/questions/24097826/read-and-write-data-from-text-file)