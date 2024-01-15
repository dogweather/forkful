---
title:                "Lettura di un file di testo."
html_title:           "Haskell: Lettura di un file di testo."
simple_title:         "Lettura di un file di testo."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo è un'operazione comune nella programmazione. Sapere come farlo in Haskell può aiutare ad automatizzare il processo di lettura dei dati da un file e manipolarli secondo le proprie esigenze.

## Come Fare

Per leggere un file di testo in Haskell, è necessario utilizzare le funzioni `readFile` e `lines`. La prima legge l'intero contenuto del file come una stringa, mentre la seconda suddivide la stringa in righe separate. Ecco un esempio:

```Haskell
main = do
  fileContent <- readFile "file.txt"
  let linesList = lines fileContent
  print linesList
```

Supponendo che il file "file.txt" contenga i seguenti dati:

```
Hello
World
```

L'output del programma sarà:

```
["Hello","World"]
```

Si noti che la lista è di tipo `String`, quindi se si vuole manipolarla o convertirla in un altro tipo di dato, è necessario utilizzare le opportune funzioni di conversione.

## Deep Dive

Esistono anche altre funzioni utili per leggere un file di testo in Haskell, come ad esempio `readFileLines`, che legge il file e restituisce direttamente una lista di righe. Inoltre, è possibile specificare il percorso di un file con un path assoluto o relativo utilizzando la funzione `System.FilePath` oppure gestire gli errori di lettura del file con la funzione `catch`.

## Vedi Anche

- [Documentazione ufficiale di Haskell](https://www.haskell.org/documentation/)
- [Tutorial introduttivo su Haskell](https://wiki.haskell.org/Tutorials)
- [Esempi pratici di lettura di file di testo in Haskell](https://www.tutorialspoint.com/learn_haskell/haskell_reading_files.htm)