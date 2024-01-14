---
title:    "Haskell: Estrazione di sottostringhe."
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## Perché

Estrarre sottostringhe è un'operazione fondamentale nella programmazione funzionale. È utile per manipolare stringhe o per accedere a parti specifiche di una stringa.

## Come Fare

Per estrarre una sottostringa in Haskell, possiamo utilizzare la funzione `take` per prelevare i primi n caratteri di una stringa, oppure la funzione `drop` per rimuovere i primi n caratteri e ottenere il resto della stringa.

Esempio:

```Haskell
-- Estrarre i primi 3 caratteri della stringa "casa"
take 3 "casa" 
-- Output: "cas"

-- Rimuovere i primi 5 caratteri della stringa "ciao mondo"
drop 5 "ciao mondo"
-- Output: "mondo"
```

Possiamo anche utilizzare la funzione `splitAt` per estrarre una porzione di una stringa data una posizione di indice.

Esempio:

```Haskell
-- Estrarre i primi 5 caratteri della stringa "haskell" e rimuovere il resto
splitAt 5 "haskell"
-- Output: ("haske", "ll")

-- Estrarre il resto della stringa "mondo" dalla posizione di indice 3
splitAt 3 "mondo"
-- Output: ("mon", "do")
```

## Approfondimenti

In Haskell, le stringhe sono elenchi di caratteri. Questo significa che possiamo utilizzare qualsiasi funzione che manipoli elenchi per manipolare stringhe.

È anche importante notare che le stringhe in Haskell sono immutabili, il che significa che le operazioni di estrazione creano sempre una nuova stringa anziché modificarla direttamente.

## Vedi Anche

- [Documentazione su `take`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:take)
- [Documentazione su `drop`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:drop)
- [Documentazione su `splitAt`](https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:splitAt)