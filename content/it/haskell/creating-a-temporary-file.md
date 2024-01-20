---
title:                "Creare un file temporaneo"
html_title:           "Arduino: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

Creare un file temporaneo è una pratica comune in programmazione per memorizzare dati volatili o transitori. Questi file sono utili per la gestione di buffer di dati, cache, logging, e altro.

## Come fare:

Per creare un file temporaneo in Haskell, possiamo utilizzare funzioni di alto livello dal modulo `System.IO.Temp`.

```Haskell 
import System.IO.Temp

main :: IO ()
main = withSystemTempFile "temp.txt" $ \fpath fhandle -> do
 writeFile fpath "Ciao, Mondo!"
 s <- readFile fpath
 print s
```

Eseguendo questo programma, verrà creata un file temporaneo, vi scrive "Ciao, Mondo!" e poi stampa il contenuto letta.

## Approfondimento

L'utilizzo di file temporanei nelle applicazioni software risale ai tempi in cui la RAM era scarsa e costosa. Ancora oggi, i file temporanei sono una soluzione pratica per gestire grandi quantità di dati in modo efficiente.

In Haskell, ci sono diversi modi per gestire i file temporanei. Oltre al modulo `System.IO.Temp`, la libreria `temporary` è un'altra scelta popolare che fornisce un'API piuttosto semplice ed efficace.

Quando si crea un file temporaneo, Haskell assicura che il file abbia un nome unico nel sistema operativo e che venga eliminato una volta che l'handle del file è stato chiuso. Questo è utile per garantire che i dati temporanei non rimangono nel sistema più a lungo di quanto necessario.

## Leggi Anche

Per maggiori dettagli sul modulo `System.IO.Temp`, potete rivolgervi alla documentazione Hackage a questo link: https://hackage.haskell.org/package/temporary

Per un approccio più moderno alla gestione dei file temporanei, la libreria `turtle` fornisce una buona alternativa: https://hackage.haskell.org/package/turtle

Infine, per un'analisi approfondita dei file temporanei e delle loro varie applicazioni, consiglio la lettura di “Temporary files in Haskell” di Michael Snoyman:

https://www.snoyman.com/blog/2017/10/temporary-files-haskell/