---
title:                "Haskell: Scrivere un file di testo"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché Scrivere un File di Testo?

Scrittura di un file di testo può essere utile per salvare dati, creare documenti o semplicemente esplorare la programmazione funzionale in Haskell.

## Come Fare?

Ci sono diversi modi in cui puoi scrivere un file di testo in Haskell. Possiamo usare la funzione "writeFile" per scrivere del testo su un nuovo file:

```Haskell
main = do
  let file = "mionome.txt"
      content = "Ciao, sono Alice!"
  writeFile file content
```
Questo codice creerà un file chiamato "mionome.txt" e scriverà il testo "Ciao, sono Alice!" all'interno.

## Approfondimento

Utilizzare la funzione "writeFile" è solo uno dei modi in cui possiamo scrivere un file di testo in Haskell. Possiamo anche utilizzare la funzione "putStrLn" per scrivere su console e quindi redirigere l'output su un file. Inoltre, è possibile utilizzare funzioni di gestione dei file più avanzate per scrivere e aggiornare file di testo.

## Vedi Anche

- [Funzionamento delle funzioni writeFile e putStrLn](https://www.haskell.org/tutorial/io.html)
- [Esempi di gestione avanzata dei file in Haskell](https://wiki.haskell.org/File_manipulation)