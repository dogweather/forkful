---
title:    "Haskell: Trasformare una stringa in minuscolo"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché

Nella programmazione Haskell, può essere utile convertire una stringa in minuscolo per manipolare e confrontare i dati in modo più efficace.

## Come Fare

Per convertire una stringa in minuscolo in Haskell, possiamo utilizzare la funzione `toLower` della libreria `Data.Char`. Ecco un esempio di codice:

```Haskell
import Data.Char (toLower)

lowercase :: String -> String
lowercase str = map toLower str

main = do
    putStrLn "Inserisci una stringa:"
    str <- getLine
    putStrLn ("La stringa in minuscolo è: " ++ lowercase str)
```

Ecco l'output se inseriamo la stringa "CIAO":

```
Inserisci una stringa:
CIAO
La stringa in minuscolo è: ciao
```

In questo esempio, la funzione `lowercase` utilizza la funzione `map` per applicare la funzione `toLower` a ogni carattere della stringa.

## Approfondimento

La funzione `toLower` è definita come segue:

```Haskell
toLower :: Char -> Char
```

Questa funzione prende un singolo carattere e lo converte in minuscolo se è una lettera dell'alfabeto latino. In caso contrario, il carattere rimarrà invariato.

Una cosa interessante da notare è che questa funzione è "non-deterministica", il che significa che può eventualmente restituire più di un valore per ogni input. Ciò può sembrare strano, ma è il risultato del fatto che ci sono diverse codifiche per i caratteri minuscoli e maiuscoli in diverse lingue. Ad esempio, il carattere 'I' in maiuscolo può essere convertito in 'i' o 'ı' (dotless i) in base alla codifica utilizzata.

Per altre informazioni sulle funzioni di manipolazione delle stringhe in Haskell, consigliamo di leggere la documentazione ufficiale di `Data.Char` e `Data.Text`.

## Vedi Anche

- Documentazione ufficiale di `Data.Char`: https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Char.html
- Documentazione ufficiale di `Data.Text`: https://hackage.haskell.org/package/text-1.2.3.1/docs/Data-Text.html