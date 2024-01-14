---
title:    "Go: Ottenere la data corrente"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/go/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Perché

Ottenere la data corrente può sembrare una cosa banale, ma in realtà è molto utile per creare applicazioni che richiedono informazioni temporali. Ad esempio, si può usarla per gestire la logica di un programma in base all'ora o alla data corrente.

## Come fare

Per ottenere la data corrente in Go, possiamo utilizzare il pacchetto `time`. Possiamo inoltre specificare il fuso orario per avere una data locale.

```
import "time"

// otteniamo la data corrente
dataCorrente := time.Now()

// otteniamo la data corrente nel fuso orario specificato
dataLocale := time.Now().Local()
```

Possiamo poi stampare la data corrente utilizzando il metodo `Format` e specificando il formato desiderato. Ad esempio:

```
// otteniamo un orario nel formato 24 ore
orario24 := dataCorrente.Format("15:04")

// otteniamo una data nel formato completo
dataCompleta := dataCorrente.Format("2006-01-02")
```

L'output sarà:

```
14:30
2021-05-05
```

## Approfondimento

Se vogliamo ottenere informazioni più dettagliate sulla data corrente, possiamo utilizzare i metodi del pacchetto `time` come `Day()`, `Month()` e `Year()`. Possiamo anche eseguire operazioni matematiche, ad esempio per ottenere la data di domani possiamo utilizzare il metodo `AddDate()`.

```
// otteniamo il giorno, il mese e l'anno
giorno := dataCorrente.Day()
mese := dataCorrente.Month()
anno := dataCorrente.Year()

// otteniamo la data di domani
dataDomani := dataCorrente.AddDate(0, 0, 1)
```

Possiamo anche utilizzare il pacchetto `strconv` per convertire i valori ottenuti in stringhe, se necessario.

```
import "strconv"

// convertiamo il mese in stringa
meseStringa := strconv.Itoa(int(mese))
```

## Vedi anche

- La documentazione ufficiale sul pacchetto `time` in Go: https://golang.org/pkg/time/
- Un tutorial su come gestire le date e gli orari in Go: https://www.digitalocean.com/community/tutorials/how-to-work-with-dates-and-times-in-go
- Esempi di codice per ottenere la data corrente in diversi formati: https://gobyexample.com/time-formatting-parsing