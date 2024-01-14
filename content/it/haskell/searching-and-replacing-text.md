---
title:    "Haskell: Ricerca e sostituzione di testo"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Perché 

La ricerca e sostituzione di testo è un'operazione comune e fondamentale nella programmazione, che consente di modificare rapidamente grandi quantità di dati in maniera efficace ed efficiente. Scopriamo come utilizzare questa funzione in Haskell. 

## Come fare 

Per usare la funzione di ricerca e sostituzione in Haskell, possiamo utilizzare il metodo `replace` del pacchetto `text`. Vediamo un esempio:

```Haskell
import Data.Text

main = do
  let str = "Ciao a tutti"
  let newStr = replace "Ciao" "Hello" str
  print newStr
```

Questo codice sostituirà la stringa "Ciao" nella variabile `str` con "Hello" e stamperà il nuovo valore. L'output sarà:

```
Hello a tutti
```

La funzione `replace` accetta tre parametri: il testo da sostituire, il nuovo testo e la stringa originale. Possiamo utilizzare questa funzione anche per sostituire parole più lunghe all'interno di una stringa.

## Approfondimento 

In Haskell, è possibile utilizzare una varietà di funzioni e metodi per la ricerca e sostituzione di testo. Ad esempio, possiamo utilizzare la funzione `map` per applicare una determinata operazione a ogni carattere della stringa. Vediamo un esempio:

```Haskell
import Data.Char

main = do
  let str = "Haskell"
  let newStr = map toLower str
  print newStr
```

In questo caso, abbiamo applicato la funzione `toLower` a ogni carattere della stringa e quindi stampato il risultato:

```
haskell
```

Questo esempio è un modo utile per manipolare il testo prima di eseguire la ricerca e sostituzione. Altre funzioni utili per il trattamento del testo sono `filter` e `splitOn`, che consentono di filtrare i caratteri e suddividere la stringa in base a un determinato delimitatore.

## Vedi anche 

- [Pacchetto text su Hackage](https://hackage.haskell.org/package/text)
- [Guida rapida alle funzioni di manipolazione del testo in Haskell](https://www.schoolofhaskell.com/school/starting-with-haskell/introduction-to-haskell/8_monadic-io) 
- [Tutorial completo su come utilizzare le funzioni per la manipolazione del testo in Haskell](https://wiki.haskell.org/Simple.text)