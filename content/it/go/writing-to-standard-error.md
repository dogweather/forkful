---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere sullo standard error (stderr) separa gli errori e le informazioni di log dal normale output di un programma. Si fa per diagnosticare i problemi senza interferire con gli output (stdout) che altri programmi potrebbero usare.

## How to:
Usa `os.Stderr` e `fmt.Fprintln` per scrivere su stderr:

```Go
package main

import (
	"fmt"
	"os"
)

func main() {
	errMsg := "Questo è un errore!"
	if _, err := fmt.Fprintln(os.Stderr, errMsg); err != nil {
		fmt.Println("Errore nello scrivere su stderr:", err)
	}
}
```

Output se eseguito da terminale:
```
Questo è un errore!
```

## Deep Dive
La separazione di stdout e stderr risale a sistemi Unix. Alternativamente, si può usare il pacchetto `log` di Go, impostando il logger su `os.Stderr`. Dal punto di vista dell'implementazione, `os.Stderr` è un `*os.File`, trattato come file speciale che corrisponde all'output di errore della shell.

## See Also
- La documentazione ufficiale di Go per os.Stderr: https://pkg.go.dev/os#Stderr
- Una guida sul pacchetto `log` in Go: https://pkg.go.dev/log
- Informazioni su stdout e stderr su Unix: https://en.wikipedia.org/wiki/Standard_streams
