---
title:                "Creare un file temporaneo"
html_title:           "Gleam: Creare un file temporaneo"
simple_title:         "Creare un file temporaneo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché

Creare un file temporaneo può sembrare una banalità, ma in realtà può essere molto utile in diverse situazioni. Ad esempio, può essere utilizzato per archiviare dati temporanei durante l'esecuzione di un programma o per creare un backup di file importanti prima di eseguire modifiche rischiose.

## Come farlo

Per creare un file temporaneo in Gleam, possiamo utilizzare la funzione `os.tmpfile()` che restituisce un file handler per il file temporaneo creato. Possiamo poi scrivere e leggere dal file utilizzando le funzioni `write()` e `read()` messe a disposizione dal modulo `File` di Gleam. Ecco un esempio di codice:

```gleam
import File
import os

tmpfile := os.tmpfile()

File.write(tmpfile, "Questo è un file temporaneo creato in Gleam!")

read_result := File.read(tmpfile)
assert Ok("Questo è un file temporaneo creato in Gleam!", read_result)
```

Il codice sopra crea un file temporaneo utilizzando la funzione `os.tmpfile()` e poi ne scrive il contenuto utilizzando la funzione `File.write()`. Infine, viene letto il contenuto del file con la funzione `read()` e viene effettuato un test di assert per verificare che il contenuto del file sia quello che ci aspettiamo.

## Approfondimento

La funzione `os.tmpfile()` non solo crea un file temporaneo, ma lo apre in modalità di scrittura, quindi possiamo iniziare a scrivere nel file subito dopo averlo creato. Inoltre, il file viene creato in una posizione specifica sul sistema, garantendo che il contenuto scritto nel file non verrà persi a meno che il file non venga esplicitamente eliminato. Questo lo rende un'ottima opzione per gestire dati temporanei in modo sicuro e affidabile.

## Guarda anche

- [La documentazione ufficiale su `os.tmpfile()`](https://gleam.run/modules/gleam_os/0.14.0/gleam_os#tmpfile)
- [La documentazione ufficiale su `File.write()`](https://gleam.run/modules/gleam_stdlib/0.14.0/gleam_stdlib/File#write)
- [La documentazione ufficiale su `File.read()`](https://gleam.run/modules/gleam_stdlib/0.14.0/gleam_stdlib/File#read)