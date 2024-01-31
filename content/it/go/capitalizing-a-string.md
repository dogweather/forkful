---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizzare una stringa significa trasformare tutti i suoi caratteri in lettere maiuscole. I programmatori lo fanno per uniformità, enfasi o per requisiti di confronto insensibili alle maiuscole.

## Come fare:
Ecco come si capitalizza una stringa in Go:

```go
package main

import (
	"fmt"
	"strings"
)

func main() {
	parole := "ciao mondo"
	paroleMaiuscole := strings.ToUpper(parole)
	fmt.Println(paroleMaiuscole)
}
```

E questo è il risultato:

```
CIAO MONDO
```

Usa `strings.ToUpper` per convertire in maiuscolo. Semplice e lineare!

## Approfondimento
In Go, `strings.ToUpper` è il modo per capitalizzare una stringa. Questa funzione esiste da tempo nel pacchetto `strings`, standard nella libreria Go.

Come alternativa, potresti usare cicli per attraversare e convertire ciascun carattere, ma perché reinventare la ruota? Inoltre, `strings.ToUpper` gestisce i casi complessi come i caratteri Unicode, che hanno regole diverse per il casing.

Dietro le quinte, `strings.ToUpper` funziona con rune, che sono tipo int32 che rappresentano punti codice Unicode. Go, abbracciando gli standard Unicode, gestisce elegantemente stringhe in molte lingue.

## Vedi Anche
- Documentazione Go su `strings.ToUpper`: https://pkg.go.dev/strings#ToUpper
- Tutorial Go su stringhe e rune: https://blog.golang.org/strings
- Unicode Standard: http://www.unicode.org/standard/standard.html
