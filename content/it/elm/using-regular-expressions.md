---
title:                "Utilizzo delle espressioni regolari"
html_title:           "Bash: Utilizzo delle espressioni regolari"
simple_title:         "Utilizzo delle espressioni regolari"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
Le espressioni regolari (RegEx) sono strumenti utilizzati per manipolare stringhe di testo. I programmatori le utilizzano per ricercare, sostituire e riconoscere pattern all'interno del testo, risparmiando tempo e semplificando il codice.

## Come si fa:
Ecco un esempio di come si utilizzano le RegEx in Elm:
```Elm
import Regex exposing (contains, regex)

verificaEmail : String -> Bool
verificaEmail email =
    contains (regex "[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+") email
```
Questo codice verifica se una stringa è un indirizzo email valido. Se corretto, restituisce "True", altrimenti "False".

## Approfondimento
Le espressioni regolari sono state create negli anni '50 per la manipolazione dei linguaggi formali. Oltre alle RegEx, esistono alternative come le string matching API per la manipolazione di stringhe. Tuttavia, l'implementazione delle RegEx in Elm è semplice e robusta, facendo di esse la scelta preferita quando si tratta di gestire pattern di stringhe complessi.

## Vedi anche
Per approfondire il tuo apprendimento su Elm e RegEx, consulta queste risorse:
1. [Elm Regex Package](http://package.elm-lang.org/packages/elm/regex/latest): Documentazione ufficiale del pacchetto RegEx di Elm.