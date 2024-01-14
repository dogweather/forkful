---
title:                "Haskell: Creazione di un file temporaneo"
programming_language: "Haskell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/haskell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo è spesso utile quando si lavora con dati transitori o quando si vuole testare una funzione senza modificare il file originale.

## Come fare

Per creare un file temporaneo in Haskell, dobbiamo importare il modulo `System.IO.Temp` e utilizzare la funzione `withSystemTempFile`. Questa funzione accetta due parametri:
- Un percorso dove creare il file temporaneo.
- Una funzione che prende come parametro il gestore del file temporaneo e vi inserisce i dati necessari.

```Haskell
import System.IO.Temp

withSystemTempFile "path/temp.txt" $ \handle -> do
    hPutStr handle "Questo testo verrà scritto nel file temporaneo."
    hClose handle
```

Una volta che abbiamo finito di lavorare con il file, questo verrà automaticamente eliminato dalla memoria.

## Approfondimento

Esistono diverse opzioni per personalizzare il modo in cui viene creato il file temporaneo. Ad esempio, è possibile specificare un prefisso per il nome del file, impostare una directory diversa da quella di default e anche decidere se conservarlo o eliminarlo una volta che la sessione è terminata.

Per maggiori informazioni e dettagli sulla creazione di file temporanei in Haskell, si consiglia di consultare la documentazione ufficiale del modulo `System.IO.Temp`.

## Vedi anche

- Documentazione del modulo `System.IO.Temp`: https://hackage.haskell.org/package/temporary-1.3/docs/System-IO-Temp.html
- Tutorial su come gestire i file in Haskell: https://wiki.haskell.org/Introduction_to_IO
- Discussione sulla creazione di file temporanei su Reddit: https://www.reddit.com/r/haskell/comments/9qt3yz/how_to_create_a_temporary_file_in_haskell/