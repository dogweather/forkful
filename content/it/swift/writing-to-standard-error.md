---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"

category:             "Swift"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error (stderr) permette di separare i normali output di un programma dagli errori. Questo è utile per diagnosticare problemi e per registrare gli errori senza interferire con l'output standard.

## How to:
In Swift, usi `FileHandle.standardError` per scrivere su stderr. Ecco come:

```Swift
import Foundation

if let errorMessage = "Errore critico.\n".data(using: .utf8) {
    FileHandle.standardError.write(errorMessage)
}
```

Se esegui questo codice, vedi sul terminal:

```
Errore critico.
```

Nota: l'output di errore potrebbe non essere visibile nell'ambiente di sviluppo e potrebbe essere necessario eseguire il programma nel terminal per vederlo.

## Deep Dive
Swift non aveva un modo diretto per scrivere su stderr fino alla release di Foundation su macOS e sui sistemi Unix-like. In alternativa, potevi usare `fprintf(stderr, "messaggio")` da C. L'implementazione di Swift gestisce gli I/O come stream, e `FileHandle.standardError` è un'astrazione su `stderr` che facilita la scrittura in Swift.

## See Also
Per approfondire, consulta i seguenti link:
- Documentazione ufficiale Apple su [FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
- Tutorial su [Standard Streams](https://en.wikipedia.org/wiki/Standard_streams) da Wikipedia per capire come funzionano stdin, stdout e stderr.
