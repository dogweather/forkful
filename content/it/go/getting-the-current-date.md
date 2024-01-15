---
title:                "Ottenere la data attuale"
html_title:           "Go: Ottenere la data attuale"
simple_title:         "Ottenere la data attuale"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Perché

Sei stanco di codice complicato per ottenere la data corrente? Con Go, puoi facilmente ottenere la data attuale e utilizzarla nei tuoi progetti in poche righe di codice.

##Come fare

Per ottenere la data corrente in Go, è necessario importare il pacchetto "time". Quindi, puoi utilizzare la funzione "Now()" del pacchetto "time" per ottenere l'oggetto di data e ora corrente. Ecco un esempio di codice:

```Go
import "time"

func main() {
  currentDate := time.Now()
  fmt.Println(currentDate)
}
```

Questo codice stamperà la data corrente nella console in un formato come "2020-09-14 14:30:00.123456789 +0200 CEST". Puoi anche formattare la data in modi diversi utilizzando il metodo "Format()" di "time" e fornendo un layout specifico come argomento. Ad esempio:

```Go
import "time"

func main() {
  currentDate := time.Now()
  formattedDate := currentDate.Format("02.01.2006")
  fmt.Println(formattedDate)
}
```

Questo codice stamperà solo la data corrente nel formato "14.09.2020". Puoi trovare una lista completa dei layout disponibili nella documentazione di Go.

##Approfondimento

Il pacchetto "time" di Go utilizza il formato ISO 8601 come formato predefinito per le date e le ore. Ciò significa che la data viene rappresentata in una stringa standardizzata, che rende più facile la conversione e la manipolazione delle date.

Tuttavia, è importante tenere presente che l'oggetto di data e ora restituito dalla funzione "Now()" è basato sul fuso orario del sistema operativo. Se desideri utilizzare un fuso orario diverso, puoi impostarlo utilizzando il metodo "Location()" di "time" e fornendo come argomento un'istanza di "time.Location".

##Vedi anche

- Documentazione ufficiale di Go sulla gestione delle date e delle ore: https://golang.org/pkg/time/
- Tutorial su come formattare le date in Go: https://www.calhoun.io/working-with-dates-and-times-in-go/
- Esempi pratici di utilizzo delle date in Go: https://networkChuck.com/go-dates/