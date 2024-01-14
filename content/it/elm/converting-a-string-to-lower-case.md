---
title:                "Elm: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Convertire una stringa in minuscolo può essere utile quando si vuole standardizzare l'input degli utenti, rendere più semplici le operazioni di confronto tra stringhe o semplicemente per uniformità nella presentazione dei dati.

## Come fare

Per convertire una stringa in minuscolo in Elm, si può utilizzare la funzione `String.toLower`. Ecco un esempio di codice:

```Elm
stringa = "CIAO A TUTTI"
String.toLower stringa
```

L'output di questo codice sarà:

```Elm
"ciao a tutti"
```

Possiamo anche passare direttamente la stringa come argomento della funzione, senza dover prima crearne una variabile:

```Elm
String.toLower "SALVE A TUTTI"
```

E l'output sarà lo stesso. 

## Approfondimento

È importante notare che la funzione `String.toLower` è di fatto una chiamata al sistema operativo per l'esecuzione di una particolare funzione di conversione. Questo significa che il suo comportamento potrebbe variare a seconda del sistema operativo su cui viene eseguito il codice.

Inoltre, va tenuto presente che la conversione avviene in base alle regole della locale (lingua) impostata nel sistema. Ad esempio, se si utilizza una locale italiana, la conversione di una stringa come "HELLO" risulterà "hello". Ma se si utilizza una locale francese, il risultato sarà "héllö". 

È anche possibile utilizzare la funzione `String.toLowerAscii` per convertire la stringa solo in caratteri ASCII, ignorando quelli accentati. 

## Vedi anche

- Documentazione Elm su `String.toLower`: https://package.elm-lang.org/packages/elm/core/latest/String#toLower
- Altre funzioni di conversione delle stringhe: https://package.elm-lang.org/packages/elm/core/latest/String#module
- Articolo sulla gestione delle locale in Elm: https://guide.elm-lang.org/appendix/i18n.html