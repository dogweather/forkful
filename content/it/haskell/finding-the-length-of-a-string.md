---
title:    "Haskell: Trova la lunghezza di una stringa"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Perché

Una delle attività più comuni nella programmazione è trovare la lunghezza di una stringa. Questa è una competenza fondamentale che viene utilizzata in molti contesti, sia nella scrittura di script di base che nello sviluppo di applicazioni complesse. In questo articolo, esploreremo come trovare la lunghezza di una stringa utilizzando Haskell.

## Come

Per trovare la lunghezza di una stringa in Haskell, possiamo utilizzare la funzione `length`. Questa funzione accetta una stringa come input e restituisce un numero intero che rappresenta la sua lunghezza.

```
Haskell
length "Ciao mondo"  
-- Output: 10
```

Un altro modo per trovare la lunghezza di una stringa è utilizzando la funzione `fromIntegral` insieme alla funzione `length`. Questo ci permette di ottenere un numero in virgola mobile anziché un numero intero.

```
Haskell
fromIntegral (length "Ciao mondo")  
-- Output: 10.0
```

## Deep Dive

In Haskell, una stringa è una sequenza di caratteri racchiusi tra doppi apici. Quando utilizziamo la funzione `length`, viene effettuata una scansione dei caratteri della stringa e viene contata ogni carattere trovato. Questo processo è molto efficiente e la funzione `length` è in grado di gestire stringhe di qualsiasi lunghezza.

Un'importante considerazione durante l'utilizzo della funzione `length` è che essa conta anche gli spazi bianchi. Questo significa che se abbiamo una stringa come "Ciao mondo", la funzione `length` restituirà 10 come output, poiché conta anche lo spazio tra le due parole. Possiamo evitare questo problema rimuovendo gli spazi bianchi con la funzione `filter`.

```
Haskell 
length (filter (/= ' ') "Ciao mondo")  
-- Output: 9
```

Inoltre, è possibile utilizzare la funzione `show` per visualizzare la lunghezza della stringa insieme ad altre informazioni.

```
Haskell 
"La lunghezza della stringa è " ++ show (length "Ciao mondo")  
-- Output: "La lunghezza della stringa è 10"
```

## Vedi anche

- [Documentazione ufficiale di Haskell](https://www.haskell.org/documentation/)
- [Tutorial sulle stringhe in Haskell](https://wiki.haskell.org/Strings)
- [Esempi di codice per lavorare con le stringhe in Haskell](https://www.geeksforgeeks.org/haskell-string/)
- [Una panoramica delle funzioni di base in Haskell](https://hackage.haskell.org/package/base-4.15.0.0/docs/Basic.html)