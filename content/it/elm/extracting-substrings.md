---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/elm/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Estrarre sottosequenze è l'operazione di ottenere una parte di una stringa, iniziando da un punto specifico e terminando in un altro. Noi programmatori lo facciamo per manipolare e analizzare porzioni di dati all'interno di una stringa più grande.

## Ecco come fare:

In Elm, `String.slice` è il tuo amico quando devi estrarre sottosequenze. Vediamo un esempio:

```Elm
import String

sottosequenza = String.slice 0 5 "Hello, World!"
```

L'output di questo codice sarà:

```Elm
"Hello"
```

La funzione `slice` inizia a contare da 0 e prende le lettere dalla posizione iniziale alla posizione finale, escludendo quest'ultima.

## Approfondimenti

- Storia: prelevare sottosequenze è un concetto risalente agli albori della programmazione. In Elm, le funzioni per farlo sono disponibili fin dalle prime versioni.

- Alternative: `String.left` e `String.right` possono essere utilizzate per estrarre sottosequenze dall'inizio o dalla fine di una stringa.

- Dettagli Implementativi: `String.slice` in Elm utilizza l'implementazione di basso livello fornita dal linguaggio JavaScript sottostante.

Inoltre, tenete presente che gli indici in Elm iniziano da 0. Quindi, se si tenta di accedere a un indice oltre la lunghezza della stringa, Elm restituirà una stringa vuota invece di un errore.

## Vedi anche:

Consulta la documentazione di Elm su [String.slice](https://package.elm-lang.org/packages/elm/core/latest/String#slice) per ulteriori dettagli.

Riferimenti esterni utili: MDN (Mozilla Developer Network) ha un'ottima documentazione di [String.prototype.slice()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/slice) in JavaScript, che potrebbe esserti utile per comprendere l'implementazione di `String.slice` in Elm.