---
title:                "Estrazione di sottostringhe"
date:                  2024-01-20T17:45:46.357344-07:00
model:                 gpt-4-1106-preview
simple_title:         "Estrazione di sottostringhe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Estrarre sottostringhe significa selezionare parti specifiche di una stringa di testo. I programmatori lo fanno per manipolare e analizzare i dati più facilmente, come estrarre nomi utente da indirizzi email o valori chiave da query URL.

## Come si fa:
```gleam
import gleam/string

let text = "Programmare in Gleam è fantastico!"

// Estrarre una sottostringa dall'indice 0 a 14
let greeting = string.slice(text, 0, 14)
assert greeting == "Programmare in"

// Estrarre una sottostringa fino alla fine della stringa
let description = string.slice(text, 15, string.len(text))
assert description == "Gleam è fantastico!"

// Utilizzare `drop_left` per rimuovere i primi 15 caratteri
let remaining = string.drop_left(text, 15)
assert remaining == "Gleam è fantastico!"
```

## Approfondimento
In passato, linguaggi come C richiedevano una gestione manuale della memoria per le sottostringhe, con una maggiore complessità. Oggi, linguaggi moderni come Gleam rendono l'estrazione di sottostringhe semplice e sicura, senza doversi preoccupare della gestione manuale della memoria.

Come alternative, in altri linguaggi potresti trovare funzioni come `substring()`, `substr()` o espressioni regolari. In Gleam, `slice`, `drop_left`, e `drop_right` sono tre funzioni chiave per lavorare con sottostringhe.

Dal punto di vista dell'implementazione, quando si estrae una sottostringa, spesso non si crea una nuova stringa, ma si fornisce un riferimento alla stringa originale con un offset e una lunghezza. Questo migliora le prestazioni ed è efficiente in termini di memoria.

## Vedi anche
- Playground di Gleam per esperimenti online: [https://gleam.run](https://gleam.run)
