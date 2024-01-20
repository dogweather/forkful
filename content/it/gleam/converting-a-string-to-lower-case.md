---
title:                "Convertire una stringa in minuscolo"
html_title:           "Arduino: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Convertire una stringa in minuscolo significa cambiare tutti i suoi caratteri in lettere minuscole. I programmatori lo fanno per facilitare le operazioni di confronto tra stringhe, eliminando le variazioni dovute ai casi.

## Come fare:

In Gleam possiamo usare la funzione `string.lower` per convertire una stringa in minuscolo. Ecco un esempio:

```Gleam
import gleam/string

pub fn main(args: List(String)) {
  let s = "CIAO MONDO"
  let s_lower = string.lower(s) 
  io.println(s_lower) // stampa "ciao mondo"
}
```

## Approfondiamo

Le stringhe in informatica sono davvero antiche, risalgono ai primi tempi dell'informatica. Il concetto di convertire una stringa in maiuscolo o minuscolo esiste da quando l'ASCII è diventata la codifica standard per il testo nel 1963. 

Una alternativa alla funzione `string.lower` di Gleam potrebbe essere scrivere una propria funzione che percorre la stringa carattere per carattere, ma in genere è meno efficiente e più difficile da gestire rispetto all'uso di una funzione già presente nella libreria standard.

Dettagli sul come `string.lower` funzioni: si basa sulla funzione `unicode:downcase_list` di Erlang, e quindi opera carattere per carattero eseguendo la conversione in base alle regole Unicode.

## Continua a leggere

Per ulteriori informazioni e per consultare la documentazione completa su `string.lower`, visita il sito [https://gleam.run/stdlib/string/#lower](https://gleam.run/stdlib/string/#lower).

Si consiglia anche di leggere gli articoli correlati:
- [Una breve storia delle codifiche dei caratteri](http://www.diveintopython3.net/strings.html)
- [Documentazione di Erlang su unicode:downcase_list](https://erlang.org/doc/man/unicode.html#downcase_list-1)