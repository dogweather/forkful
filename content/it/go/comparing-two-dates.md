---
title:                "Go: Confrontare due date"
programming_language: "Go"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Perché

C'è spesso la necessità di confrontare due date in un programma. Questo può essere utile per verificare la validità dei dati inseriti dall'utente o per implementare logiche di controllo temporali.

## Come fare

In Go, il confronto tra due date è possibile utilizzando il metodo `Before()` o `After()` dell'oggetto `time.Time`. Vediamo un esempio di come confrontare due date nella seguente porzione di codice:

```Go
// Definiamo le due date da confrontare
date1 := time.Date(2021, time.July, 9, 0, 0, 0, 0, time.UTC)
date2 := time.Date(2020, time.October, 15, 0, 0, 0, 0, time.UTC)

// Utilizziamo il metodo Before() per verificare se date1 è precedente a date2
if date1.Before(date2) {
    fmt.Println("date1 è precedente a date2")
}

// Utilizziamo il metodo After() per verificare se date1 è successiva a date2
if date1.After(date2) {
    fmt.Println("date1 è successiva a date2")
}
```

L'output del codice sopra riportato sarà:

```bash
date1 è successiva a date2
```

È importante notare che il metodo `Before()` e `After()` restituiscono un valore booleano (vero o falso) a seconda del risultato del confronto. Inoltre, questi metodi confrontano solo la data e non tengono conto dell'orario. Se si desidera effettuare un confronto anche sull'orario, è necessario utilizzare il metodo `Before()` o `After()` sulla data e sull'orario separatamente.

## Approfondimenti

Un aspetto importante da considerare quando si confrontano due date è la loro precisione. In Go, le date vengono gestite come oggetti `time.Time` e includono informazioni non solo sulla data, ma anche sull'orario e sulla fuso orario. Questo può influire sul risultato del confronto tra le date in base al fuso orario in cui ci si trova.

Inoltre, è importante prestare attenzione alla formattazione delle date, in quanto anche una leggera differenza nella formattazione può influire sul risultato del confronto.

Per approfondire l'argomento del confronto tra date in Go, si consiglia di consultare la documentazione ufficiale: [https://golang.org/pkg/time/#Time.Before](https://golang.org/pkg/time/#Time.Before).

## Vedi anche

- [https://golang.org/pkg/time/#Time.Before](https://golang.org/pkg/time/#Time.Before)
- [https://gobyexample.com/time-formatting-parsing](https://gobyexample.com/time-formatting-parsing)
- [https://www.golangprograms.com/go-language/golang-date-time.html](https://www.golangprograms.com/go-language/golang-date-time.html)