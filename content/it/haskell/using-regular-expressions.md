---
title:                "Haskell: Utilizzare le espressioni regolari"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Perchè Usare le Espressioni Regolari in Haskell

Le espressioni regolari sono uno strumento fondamentale per il parsing dei dati in linguaggi di programmazione come Haskell. Grazie alla loro flessibilità e potenza, permettono di effettuare ricerche e manipolazioni di testo in maniera efficiente e precisa. Continua a leggere per scoprire come utilizzarle nella tua programmazione in Haskell!

## Come Utilizzare le Espressioni Regolari in Haskell

Per utilizzare le espressioni regolari in Haskell, è necessario importare il modulo "Text.Regex.Posix". Una volta importato, abbiamo a disposizione le funzioni fondamentali per manipolare le espressioni regolari, come ad esempio "match" per determinare se una stringa corrisponde alla regex specificata o "subRegex" per sostituire le occorrenze di una regex con un'altra stringa.

Ecco un esempio di codice che utilizza l'espressione regolare "[0-9]+" per trovare tutte le cifre in una stringa:

```Haskell
import Text.Regex.Posix

main = do
  let stringa = "Questa è una stringa con 123 numeri."
  if (stringa =~ "[0-9]+") :: Bool
    then putStrLn "La stringa contiene almeno un numero."
    else putStrLn "La stringa non contiene numeri."
```

L'output di questo codice sarà "La stringa contiene almeno un numero." poiché la regex specificata trova la sequenza di cifre "123".

## Approfondimento sulle Espressioni Regolari in Haskell

A differenza di altri linguaggi di programmazione, le regex in Haskell sono implementate utilizzando il linguaggio funzionale (in particolare tramite la libreria "regex-base"). Questo significa che possono essere manipolate come qualsiasi altro tipo di dato all'interno del linguaggio.

Inoltre, le espressioni regolari in Haskell supportano anche alcune funzionalità avanzate, come la possibilità di utilizzare le funzioni di ordine superiore per costruire regex più complesse o l'utilizzo dei gruppi di cattura per estrarre parti specifiche delle stringhe corrispondenti.

## Vedi Anche (See Also)

- [Documentazione ufficiale sul modulo Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix/docs/Text-Regex-Posix.html)
- [Esempi pratici di utilizzo delle espressioni regolari in Haskell](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/Simple%20examples%20of%20Regular%20Expressions%20in%20Haskell) (in inglese) 
- [Un articolo dettagliato sul funzionamento delle espressioni regolari in Haskell](https://wiki.haskell.org/Regular_expressions) (in inglese)