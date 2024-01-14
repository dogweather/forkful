---
title:    "Elm: Capitalizzazione di una stringa"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte situazioni in cui potresti aver bisogno di convertire una stringa in maiuscolo. Potrebbe essere per formattare correttamente i dati prima di inviarli a un server o semplicemente per visualizzare l'input dell'utente come testo in maiuscolo. In ogni caso, è importante capire come farlo correttamente e in modo efficiente in Elm.

## Come fare

Per convertire una stringa in maiuscolo in Elm, è possibile utilizzare la funzione `String.toUpper`. Ad esempio:

```Elm
String.toUpper "ciao" -- restituisce "CIAO"
String.toUpper "elm programming" -- restituisce "ELM PROGRAMMING"
```

Come puoi vedere, la funzione accetta semplicemente una stringa come argomento e restituisce una nuova stringa in maiuscolo.

## Approfondimento

La conversione di una stringa in maiuscolo può sembrare un'operazione semplice, ma ci sono alcuni aspetti che vale la pena considerare. 

In primo luogo, è importante notare che la funzione `String.toUpper` non solo converte le lettere minuscole in maiuscole, ma rimuove anche qualsiasi carattere accentato o segno diacritico presente nella stringa. Ad esempio, se chiamiamo `String.toUpper "éàü"` il risultato sarà "EAU" invece di "ÉÀÜ".

Inoltre, è importante prestare attenzione alle regole di capitalizzazione della lingua in cui si sta scrivendo il codice. Ad esempio, in italiano le parole vengono capitalizzate in modo diverso rispetto all'inglese, quindi potrebbe essere necessario utilizzare una funzione personalizzata per gestire correttamente i casi particolari.

Infine, la funzione `String.toUpper` utilizza l'algoritmo Unicode per la conversione, quindi è compatibile con qualsiasi carattere presente nel set di caratteri Unicode.

## Vedi anche

- [Documentazione ufficiale di Elm sulle stringhe](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Unicode.org - Standard Unicode](https://unicode.org/standard/standard.html)