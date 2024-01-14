---
title:    "Elm: Convertire una stringa in minuscolo"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/elm/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui qualcuno potrebbe essere interessato a convertire una stringa in lettere minuscole in Elm. Ad esempio, potrebbe essere necessario confrontare due stringhe in modo case-insensitive, o semplicemente rendere il testo più leggibile per l'utente finale.

## Come Fare
Per convertire una stringa in lettere minuscole in Elm, possiamo utilizzare la funzione `String.toLower` inclusa nella libreria standard. Vediamo un esempio:

```Elm
import String

String.toLower "CIAO" -- "ciao"
```

Come possiamo vedere, il risultato è una nuova stringa con tutte le lettere convertite in minuscolo. Questo metodo funziona anche con stringhe contenenti caratteri speciali o spazi.

Per convertire una stringa contenuta in una variabile, possiamo utilizzare la funzione `String.toLower` come segue:

```Elm
import String

text = "Hello, World!"
lowerText = String.toLower text -- "hello, world!"
```

Inoltre, possiamo anche concatenare diverse funzioni per ottenere il risultato desiderato. Ad esempio, se vogliamo convertire una stringa in minuscolo e poi rimuovere gli spazi iniziali e finali, possiamo farlo in questo modo:

```Elm
import String

text = "  Hello, World!  "
lowerText = String.toLower text -- "hello, world!"
trimmedText = String.trim lowerText -- "hello, world!"
```

In questo esempio, la funzione `String.trim` viene utilizzata per rimuovere gli spazi dalle estremità della stringa risultante.

## Approfondimento
La funzione `String.toLower` in realtà utilizza la libreria di codifica Unicode e fa in modo che funzioni correttamente con tutte le lingue supportate. Inoltre, è in grado di gestire anche stringhe contenenti caratteri di escape, come "\n" o "\t". È importante sottolineare che l'operazione di conversione viene eseguita in modo immutabile, ovvero non modifica la stringa originale ma ne crea una nuova.

Un'altra caratteristica interessante di Elm è che gestisce in modo automatico la codifica UTF-8 dei caratteri. Ciò significa che possiamo utilizzare caratteri di qualsiasi lingua senza doverci preoccupare della codifica. Ad esempio, possiamo scrivere `String.toLower "Ciao, Cattolica"` senza problemi, anche se la parola "Cattolica" contiene caratteri accentati.

## Vedi anche
- Funzione `String.toLower` nella documentazione di Elm: https://package.elm-lang.org/packages/elm/core/latest/String#toLower
- Tutorial su come utilizzare funzioni di conversione di stringhe in Elm: https://guide.elm-lang.org/interop/javascript.html#convert-strings
- Articolo sulle caratteristiche di codifica Unicode e UTF-8 in Elm: https://medium.com/elmlang/the-curious-case-of-unicode-in-elm-aed9bafc7b19