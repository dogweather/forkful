---
title:                "Conversione di una stringa in minuscolo"
html_title:           "Elm: Conversione di una stringa in minuscolo"
simple_title:         "Conversione di una stringa in minuscolo"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Convertire una stringa in minuscolo è una tecnica comune usata dai programmatori per rendere una stringa più uniforme e facile da manipolare. Invece di avere stringhe con lettere maiuscole e minuscole miste, il processo di conversione le trasforma tutte in minuscolo, facilitando la ricerca e il confronto tra di esse.

## Come fare:
```Elm
-- Definiamo una stringa di esempio
let stringa = "Elm è un linguaggio di programmazione funzionale"
-- Usiamo la funzione toLower per convertire la stringa in minuscolo
String.toLower stringa
```

Output: "elm è un linguaggio di programmazione funzionale"

## Approfondimento:
La pratica di convertire le stringhe in minuscolo è diventata popolare durante i primi anni del web, quando il case sensitivity (sensibilità alle maiuscole e minuscole) era un problema comune nei linguaggi di programmazione. Google, ad esempio, utilizzava la conversione in minuscolo per uniformare le ricerche e garantire risultati accurati. Un'alternativa al toLower è la funzione toUpper, che converte una stringa in maiuscolo.

## Vedi anche:
Per ulteriori informazioni su come manipolare le stringhe in Elm, puoi consultare la documentazione ufficiale sulle funzioni relative alle stringhe: https://package.elm-lang.org/packages/elm/core/latest/String#toLower