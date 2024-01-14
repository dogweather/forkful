---
title:    "Haskell: Lettura di un file di testo"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/haskell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo è un'operazione molto comune quando si lavora con Haskell. Può essere utile per leggere input o dati da file esterni, ad esempio per analizzarli o elaborarli all'interno del tuo programma.

## Come Fare

Per leggere un file di testo in Haskell, utilizzeremo la funzione `readFile`, che prende come argomento il nome del file che si vuole leggere. Esempio:

```Haskell
contenuto <- readFile "file.txt"

putStrLn contenuto
```

Questo codice leggerà il contenuto del file `file.txt` e lo stamperà a video.

## Approfondimento

La funzione `readFile` legge il contenuto del file come una stringa. Ma cosa succede se vogliamo processare il file linea per linea? Per questo possiamo utilizzare la funzione `lines`, che separa il contenuto del file in una lista di stringhe, una per ogni linea. Esempio:

```Haskell
contenuto <- readFile "file.txt"

let linee = lines contenuto

print linee
```

Inoltre, se vogliamo modificare il contenuto del file, possiamo utilizzare la funzione `writeFile` per scrivere su di esso. Esempio:

```Haskell
let modifiche = unlines ["Prima linea", "Seconda linea", "Terza linea"]

writeFile "file.txt" modifiche
```

Questo codice sovrascrive il contenuto del file con le nuove modifiche.

## Vedi Anche

- [Documentazione ufficiale su `readFile`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:readFile)
- [Documentazione ufficiale su `lines`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:lines)
- [Documentazione ufficiale su `writeFile`](https://hackage.haskell.org/package/base/docs/Prelude.html#v:writeFile)

Grazie per aver letto questo articolo su come leggere un file di testo in Haskell! Speriamo che ora tu possa utilizzare queste conoscenze per svolgere compiti più avanzati all'interno dei tuoi progetti.