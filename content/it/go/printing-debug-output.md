---
title:                "Go: Stampa output di debug"
programming_language: "Go"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Perché

Stampare l'output di debug è un ottimo modo per comprendere il funzionamento del proprio codice e per individuare eventuali errori. Inoltre, può essere utile durante lo sviluppo per testare singoli passaggi e verificare il corretto flusso del programma.

## Come Fare

Per stampare l'output di debug in Go, è possibile utilizzare due funzioni: `fmt.Println()` o `fmt.Printf()`. Entrambe accettano come parametro una stringa di formato, seguita da eventuali variabili da stampare. Ad esempio:

```
package main

import "fmt"

func main() {
  age := 28

  fmt.Println("L'età dell'utente è:", age)
  fmt.Printf("L'età dell'utente è: %d", age)
}
```

Questo codice produrrà l'output:

```
L'età dell'utente è: 28
L'età dell'utente è: 28
```

Utilizzando il verbo `%d` nella stringa di formato, stampiamo il valore della variabile `age` come un intero.

## Approfondimento

Esistono altri verbi di formato oltre a `%d` che possono essere utilizzati per stampare variabili di diversi tipi di dati, come `%s` per le stringhe o `%f` per i float. Inoltre, è possibile specificare un numero dopo il verbo per indicare la precisione o la larghezza del valore da stampare.

Inoltre, è possibile combinare più variabili o costanti all'interno della stringa di formato, come mostrato nell'esempio seguente:

```
package main

import "fmt"

func main() {
  name := "Marco"
  age := 28

  fmt.Printf("Ciao, mi chiamo %s e ho %d anni!", name, age)
}
```

Questo produrrà l'output:

```
Ciao, mi chiamo Marco e ho 28 anni!
```

## Vedi Anche

- Documentazione di `fmt.Printf()`: https://golang.org/pkg/fmt/#Printf
- Esempi di utilizzo di `fmt` in Go: https://gobyexample.com/string-formatting