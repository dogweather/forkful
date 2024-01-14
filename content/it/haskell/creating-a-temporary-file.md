---
title:    "Haskell: Creazione di un file temporaneo"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché Creare un File Temporaneo in Haskell

Creare un file temporaneo può essere utile quando si vuole manipolare dati in modo dinamico e temporaneo durante l'esecuzione di un programma Haskell. Ad esempio, si potrebbe voler scrivere dei dati in un file temporaneo e poi utilizzarli per una determinata operazione, senza dover memorizzare il file in modo permanente.

## Come Creare un File Temporaneo in Haskell

Per creare un file temporaneo in Haskell, si può utilizzare la funzione `openTempFile` dal modulo `System.IO.Temp`. Questa funzione prende come argomenti il percorso in cui creare il file temporaneo, il prefisso del nome del file e l'estensione del file. Ad esempio:

```Haskell
import System.IO.Temp (openTempFile)

main = do
  (path, handle) <- openTempFile "." "temp" ".txt"
  hPutStrLn handle "Questo è un file temporaneo"
  hClose handle
  putStrLn $ "Il file temporaneo è stato creato in " ++ path
```

Il codice sopra crea un file temporaneo con il nome "tempX.txt", dove "X" è un numero univoco generato dal sistema operativo. Viene anche creato un handler per il file, che permette di scrivere all'interno del file utilizzando la funzione `hPutStrLn`. Una volta che si è finito di utilizzare il file, si deve chiudere l'handler utilizzando la funzione `hClose`.

L'esempio di output potrebbe essere:

```
Il file temporaneo è stato creato in ./temp8439.txt
```

## Approfondimento sulla Creazione di File Temporanei in Haskell

Quando si crea un file temporaneo in Haskell, il sistema operativo genera un nome univoco per il file. Inoltre, il file viene creato nella directory specificata come primo argomento della funzione `openTempFile`. Se non si specifica una directory, la funzione utilizzerà la directory di lavoro corrente.

Inoltre, è possibile utilizzare la funzione `withSystemTempFile`, sempre dal modulo `System.IO.Temp`, per creare automaticamente e gestire il file temporaneo all'interno di un blocco `IO`. Ad esempio:

```Haskell
import System.IO.Temp (withSystemTempFile)

main = withSystemTempFile "temp.txt" $ \path handle -> do
  hPutStrLn handle "Questo è un file temporaneo"
  putStrLn $ "Il file temporaneo è stato creato in " ++ path
```

Questo codice fa lo stesso lavoro del primo esempio, ma è gestito in modo automatico all'interno del blocco `withSystemTempFile`.

## Vedi Anche

- [Documentazione del modulo System.IO.Temp](https://hackage.haskell.org/package/temporary/docs/System-IO-Temp.html)
- [Creazione di file temporanei in Python](https://www.geeksforgeeks.org/python-create-temporary-file-using-tempfile-module/)