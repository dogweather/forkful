---
title:                "Convertire una data in una stringa"
html_title:           "Go: Convertire una data in una stringa"
simple_title:         "Convertire una data in una stringa"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Perché
Se hai mai programmato in Go, probabilmente hai incontrato la necessità di convertire una data in una stringa. Questo può essere utile per visualizzare la data in un formato specifico o per confrontare date.

## Come fare
Per convertire una data in una stringa in Go, è necessario utilizzare il pacchetto `time` e la funzione `Format`. Ecco un esempio di codice:

```Go
import "fmt"
import "time"

func main() {
  date := time.Now() // ottieni la data e l'ora correnti
  dateString := date.Format("02/01/2006") // definisci il formato della stringa
  fmt.Println(dateString) // output: 06/04/2020
}
```

Il risultato dipenderà dalla data e dall'ora correnti. Puoi modificare il formato della stringa in base alle tue esigenze seguendo le specifiche del pacchetto `time`.

### Opzioni del formato
Il pacchetto `time` offre diverse opzioni per il formato della stringa, come ad esempio:

- `02`: rappresenta il giorno con due cifre
- `01`: rappresenta il mese con una cifra
- `2006`: rappresenta l'anno con quattro cifre
- `Monday`: rappresenta il giorno della settimana completo
- `Mon`: rappresenta il giorno della settimana abbreviato
- `15`: rappresenta l'ora in formato 24 ore
- `03`: rappresenta l'ora in formato 12 ore
- `04`: rappresenta il minuto
- `05`: rappresenta il secondo

Puoi combinare queste opzioni per ottenere il formato desiderato. Per esempio, la stringa "June 04, 2020" sarebbe formattata come "January 02, 2006" in Go.

## Approfondimento
Se vuoi saperne di più sulla formattazione delle date in Go, puoi controllare la documentazione ufficiale del pacchetto `time` [qui](https://golang.org/pkg/time/#pkg-constants). Puoi anche sperimentare con diverse opzioni di formato per ottenere diverse visualizzazioni della data.

## Vedi anche
- [Documentazione ufficiale del pacchetto `time`](https://golang.org/pkg/time/)
- [Tutorial su come formattare la data in Go](https://www.digitalocean.com/community/tutorials/how-to-use-the-time-package-in-go)