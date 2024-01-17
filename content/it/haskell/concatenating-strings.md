---
title:                "Concatenare stringhe"
html_title:           "Haskell: Concatenare stringhe"
simple_title:         "Concatenare stringhe"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che cos'è e Perché?

Concatenare stringhe è il processo di unire due o più stringhe in una singola stringa più lunga. I programmatori lo fanno per creare output di testo più complessi, come messaggi di errore dinamici o contenuto di pagina web generato dinamicamente.

## Come Fare:

Ecco un esempio di come si concatenano due stringhe in Haskell:

```Haskell
concatenaStringhe :: String -> String -> String
concatenaStringhe s1 s2 = s1 ++ s2

main = do
  let stringaUno = "Ciao,"
  let stringaDue = " mondo!"
  let stringaConcatenata = concatenaStringhe stringaUno stringaDue
  putStrLn stringaConcatenata
```

Questo codice definisce una funzione `concatenaStringhe` che prende due stringhe come input e le unisce usando l'operatore `++`. Viene poi creato un esempio di utilizzo che mostra come le due stringhe vengono concatenate per creare "Ciao, mondo!" come output.

## Approfondimento:

Concatenare stringhe è un concetto fondamentale nella programmazione e viene utilizzato in molti linguaggi di programmazione diversi. In Haskell, oltre all'operatore `++`, esiste anche una funzione `concat` che prende una lista di stringhe e le unisce tutte in una singola stringa.

In contrasto con la concatenazione di stringhe, esiste anche il concetto di interpolazione di stringhe, dove si utilizza una sintassi speciale per inserire valori all'interno di una stringa. In Haskell, l'interpolazione di stringhe si fa utilizzando il segno di dollaro `$` e le parentesi graffe `{}` attorno al valore da inserire.

L'implementazione di base di `++` in Haskell è molto efficiente, poiché sfrutta il fatto che le stringhe sono rappresentate come liste di caratteri. Tuttavia, per stringhe molto lunghe, è possibile utilizzare una struttura dati chiamata `Text` che offre prestazioni migliori.

## Vedi anche:

- [Funzioni di stringa in Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/10-Strings)
- [Documentazione di Haskell su `++` e `concat`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#g:3)