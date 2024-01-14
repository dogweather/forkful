---
title:    "Go: Convertire una data in una stringa"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Perché 

La conversione di una data in una stringa è importante per visualizzare informazioni temporali in un formato leggibile per l'utente. Ad esempio, è utile per mostrare la data di pubblicazione di un articolo o la data di scadenza di un evento.

# Come fare

Per convertire una data in una stringa in Go, è possibile utilizzare la funzione `Format` del pacchetto `time`. Ecco un esempio di codice che prende la data corrente e la converte in una stringa nel formato "gg/mm/aaaa":

```Go
package main

import (
    "fmt"
    "time"
)

func main() {
    now := time.Now()
    dateString := now.Format("02/01/2006")
    fmt.Println(dateString)
}

// Output: 23/04/2021
```

# Approfondimento

La funzione `Format` accetta un parametro di tipo `string` che definisce il layout della data da convertire. In questo caso, "02/01/2006" rappresenta il formato "giorno/mese/anno" in cui si utilizza il numero di due cifre per il giorno e il mese e il numero di quattro cifre per l'anno. La data di esempio verrà visualizzata come "23/04/2021".

È importante notare che il layout di "02/01/2006" è stato scelto in quanto rappresenta una data di esempio ben conosciuta nella comunità di Go. Tuttavia, è possibile utilizzare molti altri tipi di layout, come "giorno della settimana, gg/mese/anno" o "mese mm, aaaa", a seconda delle proprie esigenze.

# Vedi anche

- Documentazione ufficiale di Go sulla funzione `Format`: https://golang.org/pkg/time/#Time.Format.
- Tutorial su come gestire le date e le stringhe in Go: https://blog.golang.org/go-slices-tips-tricks.