---
title:    "Gleam: Ricerca della lunghezza di una stringa"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Perché

Molte volte nella programmazione abbiamo bisogno di conoscere la lunghezza di una stringa, sia per eseguire operazioni specifiche sulle stringhe stesse o per risolvere problemi più complessi. In questa breve guida, impareremo come calcolare la lunghezza di una stringa in Gleam.

## Come Fare

Per calcolare la lunghezza di una stringa in Gleam, possiamo utilizzare la funzione `String.length/1`. Questa funzione accetta una stringa come argomento e restituisce il numero di caratteri presenti nella stringa.

```Gleam
let string = "Ciao, mondo!"
let lunghezza = String.length(string)

io.println(lunghezza) // Output: 13
```

In questo esempio, abbiamo definito una variabile `string` contenente la stringa "Ciao, mondo!" e poi abbiamo utilizzato la funzione `String.length/1` per calcolare la sua lunghezza, che è 13. Infine, abbiamo stampato il risultato utilizzando la funzione `io.println/1`.

Possiamo anche utilizzare la funzione `String.length/1` su stringhe vuote o su stringhe contenenti spazi.

```Gleam
let stringa_vuota = ""
let lunghezza = String.length(stringa_vuota)

io.println(lunghezza) // Output: 0

let stringa_spazi = "      "
let lunghezza = String.length(stringa_spazi)

io.println(lunghezza) // Output: 6
```

Come possiamo vedere, il risultato è zero per una stringa vuota e il numero di spazi per una stringa contenente solo spazi.

## Deep Dive

La funzione `String.length/1` è molto utile per calcolare la lunghezza di una stringa, ma è importante ricordare che essa restituisce il numero di caratteri e non di parole. Ad esempio, se la stringa contiene parole separate da spazi, la lunghezza sarà calcolata considerando anche gli spazi.

Possiamo anche utilizzare la funzione `String.length/1` su stringhe contenenti caratteri speciali o unicode, e il risultato sarà il numero di byte utilizzati per rappresentare la stringa.

Inoltre, in Gleam è possibile accedere alla lunghezza di una stringa direttamente utilizzando `string.length` senza il bisogno di utilizzare la funzione `String.length/1`. Tuttavia, ci sono alcune differenze nei casi di stringhe contenenti caratteri speciali o unicode, quindi è sempre consigliabile utilizzare la funzione specifica.

## See Also

- [Documentazione di Gleam sulla funzione `String.length/1`](http://gleam.run/documentation#string-length)
- [Esempi di utilizzo della funzione `String.length/1`](https://github.com/gleam-lang/gleam/blob/master/examples/strings/length.gleam)
- [Altri metodi per manipolare le stringhe in Gleam](http://gleam.run/documentation#strings)