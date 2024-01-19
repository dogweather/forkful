---
title:                "Confronto tra due date"
html_title:           "Elixir: Confronto tra due date"
simple_title:         "Confronto tra due date"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Cosa e Perché?
Comparare due date significa valutare se una data è anteriore, successiva o equivalente ad un'altra. Questo permette ai programmatori di calcolare l'intervallo di tempo tra queste o di eseguire azioni specifiche basate sulla differenza di tempo. 

## Come fare:
Per confrontare due date in Go, usiamo il pacchetto "time". Ecco una dimostrazione: 

```Go
package main
import "fmt"
import "time"

func main() {

  data1 := time.Date(2020, time.January, 1, 0, 0, 0, 0, time.UTC)
  data2 := time.Date(2021, time.January, 1, 0, 0, 0, 0, time.UTC)

  if data1.Before(data2) {
    fmt.Println("La prima data è precedente alla seconda.")
  } else if data1.After(data2) {
    fmt.Println("La prima data è successiva alla seconda.")
  } else {
    fmt.Println("Le date sono equivalenti.")
  }
}
```

Se esegui questo codice, vedrai: 
"La prima data è precedente alla seconda."

## Approfondimento
La comparazione delle date è una necessità fondamentale in programmazione fin dai tempi antichi. Nel linguaggio Go, "time" è l'incapsulamento di "time_t" del C originale, che viene utilizzato per rappresentare il tempo in secondi trascorsi dal 1970.

Un'alternativa per comparare le date sarebbe di convertirle in timestamp (secondi trascorsi dal 1970) e confrontare questi numeri, ma il pacchetto "time" lo fa automaticamente per noi.

Se vuoi fare comparazioni più complesse, come confrontare solo l'anno, il mese o il giorno, potresti aver bisogno di usare funzioni come `Year()`, `Month()`, `Day()` sulla data.

## Vedi anche
Per approfondire nella comparazione delle date in Go:
- Documentazione ufficiale del pacchetto "time": https://golang.org/pkg/time/
- Confronto dei timestamp in Go: https://yourbasic.org/golang/compare-dates/
- Dal C a Go: un approccio alla cronologia delle date: https://blog.golang.org/early-go-history