---
title:    "Elm: Eliminazione di caratteri corrispondenti a un pattern"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché 

Quando si scrive un programma in Elm, può capitare di dover manipolare delle stringhe di testo. In alcuni casi, può essere utile eliminare dei caratteri che corrispondono ad un certo pattern. Vediamo insieme come fare.

## Come fare

Per eliminare i caratteri che corrispondono ad un pattern in Elm, possiamo utilizzare la funzione `Regex.replace` che ci permette di sostituire un match con una stringa vuota. Vediamo un esempio pratico:

```Elm

import Regex

stringa = "ciao123mondo!"

stringaModificata = Regex.replace (Regex.regex "\\d") stringa ""

```

In questo esempio, abbiamo utilizzato la funzione `Regex.replace` passando come primo argomento il pattern da cercare, nel nostro caso `\\d` che rappresenta una cifra, e come secondo argomento la stringa su cui applicare la sostituzione, ovvero `stringa`. Come terzo argomento, abbiamo passato una stringa vuota `""` che sostituirà ogni match trovato.

L'output di `stringaModificata` sarà quindi `ciamondo!`, dato che abbiamo eliminato tutti i caratteri numerici presenti.

## Approfondimento

La funzione `Regex.replace` ci permette di effettuare sostituzioni anche più complesse, utilizzando il parametro `Regex.substitution` come secondo argomento. Questo parametro ci permette di definire un pattern di sostituzione, dove `$n` rappresenta il match trovato. Vediamo un esempio:

```Elm

import Regex

stringa = "Elm è un linguaggio di programmazione funzionale"

stringaModificata = Regex.replace (Regex.regex "li(n|ngua)ggio") stringa (Regex.substitution "Lin($1)")

```

In questo caso, stiamo cercando il pattern `li(n|ngua)ggio`, quindi "linguaggio" o "lingua". Utilizzando `$1` nel parametro di sostituzione, stiamo indicando di mantenere la lettera trovata nel gruppo tra parentesi. L'output di `stringaModificata` sarà quindi `Elm è un Linaggio di programmazione funzionale`.

## Vedi anche

- [Regex.replace - Documentazione Elm](https://package.elm-lang.org/packages/elm/regex/1.0.0/Regex#replace)
- [Funzioni Regex in Elm - Elm Town 40](https://www.youtube.com/watch?v=DVia-qjOCxM)
- [Elm Italy - Comunità italiana di Elm](https://elmitaly.it/)