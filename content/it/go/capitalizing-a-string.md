---
title:    "Go: Maiuscolizzare una stringa"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitare una stringa è una funzionalità fondamentale di molte applicazioni in Go. Questa operazione consente di rendere più leggibili le stringhe e facilita il confronto tra di esse. In questo articolo, impareremo come capitalizzare una stringa utilizzando il linguaggio di programmazione Go.

## Come Fare
Per capitalizzare una stringa in Go, possiamo utilizzare la funzione `strings.ToUpper()` del pacchetto `strings`. Questa funzione prende come parametro una stringa e restituisce la stessa stringa con tutti i caratteri convertiti in maiuscolo.

```Go
package main

import (
  "fmt"
  "strings"
)

func main() {
  stringa := "ciao mondo"
  stringaCapitalizzata := strings.ToUpper(stringa)
  fmt.Println(stringaCapitalizzata)
}
```

L'output di questo esempio sarà: `CIAO MONDO`.

È importante notare che la funzione `strings.ToUpper()` non modifica la stringa originale, ma ne restituisce una nuova. Per modificare la stringa originale, è necessario assegnare il risultato a una variabile.

Possiamo anche utilizzare una variante di questa funzione, `strings.Title()`, che converte il primo carattere di ogni parola in maiuscolo.

```Go
package main

import (
  "fmt"
  "strings"
)

func main() {
  stringa := "ciao mondo"
  stringaCapitalizzata := strings.Title(stringa)
  fmt.Println(stringaCapitalizzata)
}
```

L'output in questo caso sarà: `Ciao Mondo`.

## Approfondimento
Ci sono alcuni casi in cui la funzione `strings.ToUpper()` non funziona come ci si aspetta. Ad esempio, se la stringa contiene caratteri speciali o accentati, questi verranno convertiti in un carattere non riconosciuto. In questo caso, è necessario utilizzare il pacchetto `unicode` per gestire correttamente questi caratteri.

Un altro aspetto importante da considerare è che la funzione `strings.ToUpper()` utilizza le impostazioni locali del sistema operativo. Ciò significa che il risultato potrebbe variare a seconda della lingua impostata sul computer. Se si desidera un risultato coerente, si consiglia di utilizzare il pacchetto `strings.ToUpper()`.

## Vedi Anche
- Documentazione Go sul pacchetto `strings`: https://golang.org/pkg/strings/
- Esempi pratici per il pacchetto `unicode`: https://golangdocs.com/golang-unicode-packages
- Approfondimenti sulle impostazioni locali in Go: https://blog.golang.org/matchlang