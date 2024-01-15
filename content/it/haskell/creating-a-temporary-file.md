---
title:                "Creazione di un file temporaneo"
html_title:           "Haskell: Creazione di un file temporaneo"
simple_title:         "Creazione di un file temporaneo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché
Hai mai avuto la necessità di creare un file temporaneo durante la programmazione in Haskell? Forse stai lavorando con file di grandi dimensioni o con dati sensibili e vuoi evitare di sovraccaricare la memoria del tuo computer. In questo articolo, imparerai come creare e gestire facilmente file temporanei utilizzando il linguaggio funzionale Haskell.

## Come fare
Per creare un file temporaneo in Haskell, è necessario importare il modulo `System.IO.Temp`. Questo modulo fornisce funzioni utili per la creazione e la gestione di file temporanei. Ecco un esempio di codice che crea un file temporaneo e ci scrive una stringa:

```
import System.IO.Temp (withSystemTempFile)

main = do
  withSystemTempFile "file_temporaneo.txt" $ \nomeFile gestore -> do
    hPutStrLn gestore "Questo è un file temporaneo."
    hClose gestore
```

In questo esempio, utilizziamo la funzione `withSystemTempFile` che accetta due parametri: il nome del file temporaneo e una funzione che accetta come argomenti il nome del file creato e un gestore di file. Questo gestore di file viene utilizzato per scrivere il contenuto desiderato nel file. Alla fine della funzione, il file viene automaticamente eliminato.

È inoltre possibile specificare un percorso diverso per il file temporaneo utilizzando la funzione `withTempFile`. In questo modo, è possibile controllare in quale directory verrà creato il file.

## Approfondimento
In Haskell, i file temporanei vengono creati utilizzando il sistema operativo sottostante. Ciò significa che se si utilizza la funzione `withSystemTempFile` più volte durante l'esecuzione di un programma, i file creati potrebbero essere archiviati in posizioni diverse. Inoltre, ciò può anche influire sulla sicurezza dei dati se il file contiene informazioni sensibili.

Per questo motivo, è consigliabile utilizzare il modulo `System.IO.Temp` solo se il file temporaneo non contiene dati importanti o se è possibile eliminare il file immediatamente dopo il suo utilizzo.

## Vedi anche
- Documentazione del modulo `System.IO.Temp`: https://hackage.haskell.org/package/temp-1.3.0.1/docs/System-IO-Temp.html
- Domande frequenti su Haskell: https://wiki.haskell.org/Haskell_FAQ
- Guida praticam a Haskell: https://www.schoolofhaskell.com/school/starting-with-haskell/a-practical-haskell-course