---
title:                "Trovare la lunghezza di una stringa"
date:                  2024-01-20T17:47:26.124335-07:00
model:                 gpt-4-1106-preview
simple_title:         "Trovare la lunghezza di una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Trovare la lunghezza di una stringa significa contare quanti caratteri contiene. I programmatori lo fanno per conoscere esattamente quanto spazio occupa un testo, per troncare stringhe lunghe o per validare l'input.

## How to:
Usa `len()` per ottenere la lunghezza di una stringa. Ecco un esempio semplice:

```Go
package main

import "fmt"

func main() {
    message := "Ciao mondo"
    length := len(message)
    fmt.Println("Lunghezza della stringa:", length)
}
```

Output:
```
Lunghezza della stringa: 10
```

Nota: `len()` conta i byte, non i caratteri Unicode. Per rune, usa `utf8.RuneCountInString()`.

```Go
package main

import (
    "fmt"
    "unicode/utf8"
)

func main() {
    message := "Ciao mondo 🌍"
    byteLength := len(message)
    runeLength := utf8.RuneCountInString(message)
    
    fmt.Println("Lunghezza in byte:", byteLength)
    fmt.Println("Lunghezza in rune:", runeLength)
}
```

Output:
```
Lunghezza in byte: 19
Lunghezza in rune: 11
```

## Deep Dive
La funzione `len()` in Go è un'operazione built-in che restituisce il numero di byte di una stringa, che è la rappresentazione di base in Go: una slice di byte. Tutto è ok per l'ASCII, ma non per Unicode che può avere più byte per carattere. Ecco perché abbiamo `utf8.RuneCountInString()`.

Prima, si lavorava con l'ASCII, che era semplice: un byte per carattere. Oggi, con Unicode, è più difficile, perché i caratteri possono variare nella lunghezza dei byte.

Un'altra opzione è usare il pacchetto "unicode/utf16" se lavori con stringhe in questo encoding. La diversità di codifica è il pane quotidiano dei programmatori multilingue.

Le rune rappresentano un punto di codifica Unicode. `range` su una stringa itera sulle rune, non sui byte, utile per evitare errori con caratteri multi-byte.

## See Also
- Documentazione ufficiale di Go su stringhe e rune: [Strings, bytes, runes and characters in Go](https://blog.golang.org/strings)
- Articolo Go Blog su Unicode: [Strings, bytes, runes and characters in Go](https://go.dev/blog/strings)
- Pacchetto unicode/utf8 Go: [unicode/utf8 package](https://pkg.go.dev/unicode/utf8)
