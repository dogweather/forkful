---
title:                "La ricerca della lunghezza di una stringa."
html_title:           "Swift: La ricerca della lunghezza di una stringa."
simple_title:         "La ricerca della lunghezza di una stringa."
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Perché

Per molte persone, scrivere codice può sembrare una lingua straniera. Ma imparare le basi della programmazione può essere utile in tante situazioni. Ad esempio, scoprire la lunghezza di una stringa ci aiuta a manipolarla e utilizzarla nel nostro codice.

## Come

Per trovare la lunghezza di una stringa in Swift, dobbiamo utilizzare la proprietà "count". Vediamo un esempio pratico:

```
let stringa = "Ciao amici!"
print(stringa.count)
```

Questo codice stamperà il numero di caratteri nella stringa, in questo caso 11. Ovviamente, possiamo anche utilizzare questa proprietà in altri modi. Ad esempio, possiamo creare una condizione che ci dica se la stringa ha una lunghezza specifica usando un'istruzione "if":

```
let stringa = "Ciao amici!"
if stringa.count == 11 {
    print("La stringa ha 11 caratteri!")
}
```

In questo caso, visto che la stringa ha effettivamente 11 caratteri, verrà stampato il messaggio "La stringa ha 11 caratteri!".  

## Deep Dive

La proprietà `count` di Swift non è solo utile per trovare la lunghezza di una stringa. Possiamo anche utilizzarla per trovare il numero di elementi in una collezione, come un array o un dizionario. Inoltre, possiamo utilizzare la funzione `count` sui tipi numerici per ottenere il numero di cifre o la lunghezza di un numero. 

Ad esempio:

```
let array = ["mela", "banana", "arancia"]
print(array.count) // 3

let dizionario = ["chiave1": 1, "chiave2": 2, "chiave3": 3]
print(dizionario.count) // 3

let numero = 12345
print(numero.count) // 5
```

Inoltre, dobbiamo anche tenere presente che la lunghezza di una stringa può variare a seconda della lingua o dell'encoding utilizzato. Per esempio, alcuni caratteri speciali in giapponese possono occupare due spazi in una stringa, influenzando di conseguenza il valore della proprietà count. 

## Vedi anche

- [Documentazione ufficiale di Swift su `count`](https://developer.apple.com/documentation/swift/string/utf8)
- [Come ottenere la lunghezza di una stringa in altri linguaggi di programmazione](https://www.rapidtables.com/web/dev/string-length.html)