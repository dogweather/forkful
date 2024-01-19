---
title:                "Ricerca e sostituzione del testo"
html_title:           "Arduino: Ricerca e sostituzione del testo"
simple_title:         "Ricerca e sostituzione del testo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

La ricerca e la sostituzione del testo sono operazioni in cui identifichiamo un pattern di caratteri in una stringa (ricerca) e lo sostituiamo con un'altro (sostituzione). I programmatori lo fanno per manipolare e gestire i dati in modo efficace.

## Come fare:

Ecco un esempio di come fare la ricerca e la sostituzione del testo in Gleam:

```Gleam
import gleam/string.{replace}

fn cambia_parola() {
    let vecchia_stringa = "Ciao Mondo"
    let nuova_stringa = string.replace(vecchia_stringa, "Mondo", "Gleam")
    assert nuova_stringa == "Ciao Gleam"
}
```

Eseguendo questo codice, l'output sarà: "Ciao Gleam".

## Approfondimento

La funzione di ricerca e sostituzione del testo è stata uno dei primi strumenti fondamentali per la manipolazione delle stringhe nel mondo della programmazione. Se stai considerando altre alternative, potresti utilizzare espressioni regolari (Regex), specialmente quando si lavora con pattern più complessi. Tuttavia, in Gleam, la funzione string.replace è di gran lunga la più comoda e leggibile.

## Per saperne di più:

Permanipolare ulteriormente le stringhe in Gleam, guarda la documentazione ufficiale: https://gleam.run/documentation/main/libraries/main/gleam/string/

Per maggiori informazioni sulle espressioni regolari (una possibile alternativa), consulta: https://www.regular-expressions.info/