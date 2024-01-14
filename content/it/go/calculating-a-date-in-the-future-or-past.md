---
title:    "Go: Calcolare una data nel futuro o nel passato"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/go/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Perché

Calcolare una data nel futuro o nel passato può essere utile in diversi contesti, come ad esempio nella pianificazione di eventi, nel gestire deadline o nel creare notifiche programmabili.

## Come fare

Per calcolare una data nel futuro o nel passato in Go, è necessario utilizzare il pacchetto "time". Ad esempio, per ottenere la data di domani possiamo utilizzare il seguente codice:

```Go
tomorrow := time.Now().AddDate(0, 0, 1)
fmt.Println("Domani sarà il:", tomorrow.Format("02/01/2006"))
```

Questo codice utilizza il metodo "AddDate" per aggiungere un giorno alla data corrente e il metodo "Format" per ottenere la data nel formato desiderato. Il risultato in output sarà: "Domani sarà il: 08/07/2021". Ovviamente, è possibile modificare i parametri del metodo "AddDate" per ottenere una data più lontana o più vicina nel futuro o nel passato.

## Approfondimento

Il pacchetto "time" offre molti altri metodi per lavorare con date e orari in Go. Ad esempio, è possibile utilizzare il metodo "Parse" per convertire una data di testo in un oggetto di tipo "time.Time". Inoltre, è possibile utilizzare i metodi "Equal" e "Before" per confrontare due date e verificare se una è uguale o precede l'altra. Per un elenco completo dei metodi disponibili, si consiglia di consultare la documentazione ufficiale del pacchetto.

## Vedi anche

- Documentazione ufficiale del pacchetto "time": https://golang.org/pkg/time/
- Esempi di utilizzo del pacchetto "time": https://gobyexample.com/time