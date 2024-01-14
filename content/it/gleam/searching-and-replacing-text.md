---
title:                "Gleam: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché

La ricerca e la sostituzione di testo sono fondamentali per mantenere il nostro codice organizzato e leggibile. Con Gleam, possiamo automatizzare questo processo e risparmiare tempo prezioso.

## Come fare

Per eseguire una ricerca e sostituire il testo in Gleam, possiamo utilizzare la funzione `String.replace` seguita da una stringa di testo da cercare e una stringa di testo da sostituire. Ad esempio:

```Gleam
let testo_sostituito = string.replace("Ciao, mondo!", "mondo", "universo")
```

Questo comando sostituirà la parola "mondo" con "universo" e restituirà la stringa "Ciao, universo!".

Se vogliamo sostituire tutte le occorrenze di una stringa, possiamo utilizzare il modificatore globale `g`.

```Gleam
let testo_sostituito = string.replace("ciao ciao ciao", "ciao", "hello", g)
```

Questo sostituirà tutte le occorrenze di "ciao" con "hello" e restituirà la stringa "hello hello hello".

## Approfondimento

Oltre alle semplici sostituzioni di base, Gleam ci offre anche la possibilità di utilizzare espressioni regolari nella nostra ricerca e sostituzione di testo. Possiamo utilizzare la funzione `Regex.replace` seguita da un'espressione regolare e una stringa di sostituzione.

```Gleam
let regex = regex.compile("ciao+", nulo)

let testo_sostituito = regex.replace("ciao ciao ciao", "hello")
```

In questo esempio, la funzione `compile` ci permette di creare una regex per cercare una o più occorrenze della parola "ciao". Il risultato della funzione `replace` sarà "hello hello hello".

## Vedi anche

- Documentazione ufficiale di Gleam sulla ricerca e la sostituzione di testo: https://gleam.run/documentation/guides/strings#replace
- Un tutorial su espressioni regolari in Gleam: https://medium.com/@gleamler/espressioni-regolari-in-gleam-4e2b5b98d5cb