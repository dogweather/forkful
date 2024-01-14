---
title:    "Elm: Trasformare una stringa in minuscolo"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo è un'operazione comune nella programmazione, in particolare quando si lavora con input utente o con dati provenienti da fonti esterne. Questa funzione ci permette di uniformare i dati e di renderli più facili da gestire e confrontare.

## Come Fare

Per convertire una stringa in minuscolo in Elm, è sufficiente utilizzare la funzione `String.toLower` passando la stringa come argomento. Ecco un esempio di codice:

```Elm
myString = "ELM è un linguaggio di programmazione funzionale"
lowercaseString = String.toLower myString
```

L'output di questo codice sarà "elm è un linguaggio di programmazione funzionale". Come si può vedere, la funzione ha semplicemente convertito tutte le lettere in minuscolo.

## Approfondimento

La funzione `String.toLower` è in realtà un caso specifico della funzione `String.map`. Questa funzione prende una funzione come primo argomento e una stringa come secondo argomento, e applica la funzione a ogni carattere della stringa restituendo una nuova stringa.

Quindi, ciò che la funzione `String.toLower` fa internamente è applicare una funzione che converte un singolo carattere in minuscolo a tutti i caratteri della stringa. Questo rende la funzione versatile, in quanto è possibile definire una qualsiasi funzione di conversione dei caratteri e passarla come primo argomento alla funzione `String.map`.

## Vedi Anche

- [Documentazione di Elm String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Funzione `String.toLower`](https://package.elm-lang.org/packages/elm/core/latest/String#toLower)
- [Funzione `String.map`](https://package.elm-lang.org/packages/elm/core/latest/String#map)