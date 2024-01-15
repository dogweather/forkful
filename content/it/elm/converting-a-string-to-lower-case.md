---
title:                "Convertire una stringa in minuscolo"
html_title:           "Elm: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo è un'operazione molto comune nella programmazione ed è utile in molteplici situazioni, come ad esempio per manipolare dati o confrontare stringhe senza tenere conto delle maiuscole e minuscole.

La lingua Elm offre un modo semplice e sicuro per eseguire questa operazione, nel modo seguente:

## Come fare

Per convertire una stringa in minuscolo in Elm, è possibile utilizzare la funzione `String.toLower`. Ad esempio:

```
Elm String.toLower "CIAO" == "ciao"
```

In questo esempio, la stringa "CIAO" viene convertita in "ciao" utilizzando la funzione `String.toLower`. È importante notare che questa operazione non modifica la stringa originale, ma ne restituisce una nuova in minuscolo.

Un altro modo per convertire una stringa in minuscolo è utilizzare il metodo `String.toLowercase` sul tipo `String`, come mostrato nell'esempio seguente:

```
Elm "CIAO".toLowercase == "ciao"
```

Entrambi i metodi restituiscono un valore di tipo `String` che può essere utilizzato per ulteriori operazioni.

## Approfondimento

La conversione di stringhe da maiuscolo a minuscolo può essere un problema in alcuni linguaggi di programmazione, in quanto è necessario prestare attenzione alle codifiche dei caratteri e alle lingue supportate. Tuttavia, Elm semplifica questo processo utilizzando una codifica interna UTF-8 e supportando tutte le lettere unicode, rendendo la conversione più sicura e affidabile.

Inoltre, Elm offre anche un modo per convertire una stringa in maiuscolo utilizzando la funzione `String.toUpper`, che segue lo stesso principio dei metodi descritti sopra.

## Vedi anche

- [Documentazione di Elm sulle stringhe](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Articolo su Enki sulla manipolazione delle stringhe in Elm](https://enki.com/t/tricky-strings-in-elm/5b7f909a611f3a000116f2fa)