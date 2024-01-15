---
title:                "Ricerca e sostituzione di testo"
html_title:           "Gleam: Ricerca e sostituzione di testo"
simple_title:         "Ricerca e sostituzione di testo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

Sei pronto a migliorare la tua produttività come programmatore? Oppure, sei semplicemente stanco di passare ore a cercare e sostituire del testo all'interno del tuo codice? Con Gleam, puoi semplificare questo compito e risparmiare tempo prezioso grazie alla funzione di ricerca e sostituzione di testo.

## Come Fare

Per utilizzare la funzione di ricerca e sostituzione di testo in Gleam, segui questi semplici passaggi:

1. Apri il tuo progetto o file contenente il testo da cercare e sostituire.
2. Premi "Ctrl + F" sulla tastiera per aprire la finestra di ricerca.
3. Immetti il testo che desideri cercare nella casella di ricerca.
4. Nella casella "Sostituisci con", inserisci il testo con cui vuoi sostituire quello cercato.
5. Utilizza i pulsanti "Precedente" e "Successivo" per spostarti tra le istanze del testo cercato e visualizzare l'anteprima dei cambiamenti.
6. Una volta soddisfatto delle sostituzioni effettuate, premi il pulsante "Sostituisci" o "Sostituisci tutto".

Ecco un esempio di codice in Gleam che mostra come utilizzare la funzione di ricerca e sostituzione:

```Gleam
let testo = "Questo è un esempio di testo da cercare e sostituire."
let testo_modificato = testo.replace("cercare e", "ricercare e modificare")

let output = testo_modificato
|> print
```

Output: "Questo è un esempio di testo da ricercare e modificare."

## Approfondimento

Oltre a semplicemente cercare e sostituire del testo all'interno del codice, Gleam offre anche funzionalità avanzate come la possibilità di utilizzare espressioni regolari nella ricerca e sostituzione. Ciò ti permette di effettuare modifiche in modo più preciso e veloce.

Inoltre, Gleam ti permette di effettuare ricerche case-insensitive (senza differenza tra maiuscole e minuscole), riducendo ancora di più il tempo necessario per trovare e sostituire il testo desiderato.

## Vedi Anche

- [Documentazione su ricerca e sostituzione in Gleam](https://gleam.run/documentation)
- [Tutorial su come utilizzare espressioni regolari in Gleam](https://gleam.run/tutorials/regular-expressions.html)
- [Video tutorial su come utilizzare la funzione di ricerca e sostituzione in Gleam](https://www.youtube.com/watch?v=dQw4w9WgXcQ)