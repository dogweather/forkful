---
title:    "Haskell: Generazione di numeri casuali"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Perché

Generare numeri casuali è un'operazione molto utile in programmazione, permette di creare simulazioni, giochi e test di stress per il nostro codice. In Haskell, abbiamo la libreria "random" che ci permette di generare facilmente numeri casuali.

## Come Fare

Per utilizzare la libreria "random" dobbiamo importare il modulo nel nostro codice:

'```Haskell
import System.Random
```

Una volta che abbiamo importato il modulo, possiamo utilizzare le sue funzioni per generare numeri casuali. Ad esempio, per generare un numero intero casuale compreso tra 1 e 10, possiamo utilizzare la funzione "randomR":

'```Haskell
randomR (1, 10) :: IO Int
>>> 5
```

Notiamo che il tipo di ritorno è "IO Int", poiché la generazione di numeri casuali coinvolge l'input/output (IO) del sistema.

Possiamo anche generare più di un numero casuale utilizzando la funzione "randomRs" e specificando il numero di numeri che vogliamo ottenere:

'```Haskell
randomRs (1, 10) 5 :: IO [Int]
>>> [4, 9, 2, 7, 10]
```

## Approfondimenti

La libreria "random" utilizza un concetto chiamato "generatore di numeri casuali" per generare i nostri numeri. Possiamo specificare un seme di inizializzazione per il generatore, in modo da ottenere risultati riproducibili. Possiamo anche utilizzare la funzione "random" per generare numeri casuali di qualsiasi tipo, non solo interi.

Uno dei modi più comuni per utilizzare la generazione di numeri casuali in Haskell è attraverso la creazione di generatori infiniti, che possono essere utilizzati per creare sequenze infinite di numeri casuali. Questo può essere particolarmente utile per le simulazioni o per i giochi.

## Vedi Anche

- [Haskell Random Documentation](https://hackage.haskell.org/package/random)
- [Learn You a Haskell - Randomness](http://learnyouahaskell.com/input-and-output#randomness)