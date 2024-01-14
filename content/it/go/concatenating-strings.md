---
title:    "Go: Unire stringhe"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Perché

Concatenare stringhe è un'operazione comune nella programmazione Go che permette di unire due o più stringhe in una sola. Questo può risultare utile in molte situazioni, come ad esempio la formattazione di messaggi o la generazione di output per gli utenti.

## Come

Per concatenare stringhe in Go, possiamo utilizzare l'operatore `+` o la funzione `fmt.Sprint()`. Vediamo un esempio di entrambi i metodi:

```Go
// Utilizzando l'operatore +
greeting := "Ciao"
name := "Maria"
message := greeting + " " + name // Output: Ciao Maria

// Utilizzando la funzione fmt.Sprint()
age := 30
info := fmt.Sprint("Nome:", name, "Età:", age) // Output: Nome: Maria Età: 30
```

## Deep Dive

In Go, le stringhe sono immutabili, il che significa che non possiamo modificarle una volta create. Quindi, quando concateniamo le stringhe, in realtà stiamo creando una nuova stringa con i valori concatenati. Questo può avere un impatto sulle prestazioni del nostro codice se stiamo lavorando con un grande numero di stringhe.

Inoltre, dobbiamo prestare attenzione alla formattazione delle stringhe durante la concatenazione. Ad esempio, se vogliamo inserire uno spazio tra due parole, dobbiamo essere sicuri di aggiungerlo nella concatenazione stessa, altrimenti potremmo ottenere un output errato.

## Vedi anche

- [Documentazione ufficiale di Go sulla gestione delle stringhe](https://golang.org/pkg/strings/)
- [Altro tutorial su concatenazione di stringhe in Go](https://www.calhoun.io/concatenating-strings-in-go/)