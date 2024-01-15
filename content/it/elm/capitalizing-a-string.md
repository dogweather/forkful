---
title:                "Maiuscolo di una stringa"
html_title:           "Elm: Maiuscolo di una stringa"
simple_title:         "Maiuscolo di una stringa"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalize una stringa può sembrare un'operazione banale ma in realtà può essere molto utile in alcune situazioni. Ad esempio, quando si vogliono mostrare nomi o titoli in una forma più formale, o quando si vuole uniformare il formato dei dati in un'analisi.

## Come fare

Capitalizzare una stringa in Elm è molto semplice. Per farlo, basta utilizzare la funzione predefinita `String.capitalize` e passare la stringa da capitalizzare come argomento. Ad esempio:

```elm
import String

capitalizzato = String.capitalize "elm" -- restituisce "Elm"
```

Se si vuole capitalizzare la prima lettera di ogni parola in una stringa, si può utilizzare la funzione `String.words` per ottenere una lista delle parole nella stringa e poi utilizzare `List.map` insieme a `String.capitalize` per capitalizzare ogni parola. Infine, si può unire la lista di parole con `String.join` per creare di nuovo una stringa. Ad esempio:

```elm
import String

capitalizzato = String.join " " (List.map String.capitalize (String.words "elm programming")) -- restituisce "Elm Programming"
```

## Approfondimento

Nel linguaggio Elm, le stringhe sono immutabili, il che significa che non è possibile modificarle direttamente. Invece, quando si applica una funzione come `String.capitalize` a una stringa, viene creata una nuova stringa con la modifica applicata. Anche se questa operazione può sembrare ridondante, è importante comprenderne il funzionamento per evitare possibili errori nella programmazione.

Inoltre, è possibile utilizzare la funzione `String.toUpper` per capitalizzare completamente una stringa invece di capitalizzare solo la prima lettera. Questa funzione è utile quando si vuole scrivere una stringa in maiuscolo per esempio in un titolo o un'etichetta.

## Vedi anche

- [Documentazione di Elm sulle stringhe](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Esempi di utilizzo di `String.capitalize` in Elm](https://guide.elm-lang.org/strings/uppercase.html)
- [Come utilizzare `String.upper` in Elm](https://package.elm-lang.org/packages/elm/core/latest/String#upper)