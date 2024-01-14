---
title:                "Go: Ottenere la data corrente"
simple_title:         "Ottenere la data corrente"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Quando si scrive un programma in Go, spesso ci si trova nella necessità di ottenere la data corrente. Ciò può essere utile per registrare quando è stato eseguito il programma, per effettuare operazioni di data o semplicemente per visualizzare la data all'utente. In questo articolo, vedremo come ottenere la data corrente utilizzando il linguaggio di programmazione Go.

## Come fare

Per ottenere la data corrente in Go, dobbiamo utilizzare la libreria "time". Possiamo iniziare importando questa libreria all'inizio del nostro programma:

```Go
import (
  "fmt"
  "time"
)
```

Una volta importata la libreria, possiamo utilizzare la funzione "Now()" per ottenere la data e l'ora correnti. Questa funzione restituisce una variabile di tipo "time.Time", che possiamo poi formattare in modo da visualizzare solo la data o solo l'ora. Ecco un esempio di codice che ci mostra la data completa:

```Go
currentDate := time.Now()
fmt.Println("La data corrente è:", currentDate)
```

Il risultato di questo codice sarà qualcosa del tipo "La data corrente è: 2021-10-13 18:00:00 +0000 UTC". Possiamo anche formattare la data in modo diverso, utilizzando il metodo "Format()" della libreria "time". Ad esempio, se vogliamo visualizzare solo la data nel formato gg/mm/aaaa, possiamo utilizzare il seguente codice:

```Go
currentDate := time.Now()
formattedDate := currentDate.Format("02/01/2006")
fmt.Println("La data corrente è:", formattedDate)
```

Il risultato di questo codice sarà "La data corrente è: 13/10/2021".

## Approfondimento

Ora che sappiamo come ottenere la data corrente in Go, vediamo alcune opzioni aggiuntive che possiamo utilizzare per personalizzare la visualizzazione della data. Innanzitutto, possiamo specificare la zona oraria in cui desideriamo visualizzare la data. Ad esempio, se vogliamo visualizzare la data nella nostra zona oraria locale, possiamo utilizzare il seguente codice:

```Go
currentDate := time.Now()
localTimezone, _ := time.LoadLocation("Local")
formattedDate := currentDate.In(localTimezone).Format("02/01/2006")
fmt.Println("La data corrente è:", formattedDate)
```

Inoltre, se vogliamo visualizzare solo una parte specifica della data, possiamo utilizzare i metodi "Year()", "Month()", "Day()" per ottenere rispettivamente l'anno, il mese e il giorno. Ecco un esempio di codice che ci mostra solo il giorno corrente:

```Go
currentDate := time.Now()
day := currentDate.Day()
fmt.Println("Oggi è il:", day)
```

## Vedi anche

- La documentazione ufficiale della libreria "time" di Go: https://golang.org/pkg/time/
- Un esempio di utilizzo della libreria "time" per calcolare la differenza tra due date: https://golangbyexample.com/golang-calculate-time-difference-datetime/