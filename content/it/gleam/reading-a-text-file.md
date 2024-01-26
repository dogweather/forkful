---
title:                "Lettura di un file di testo"
date:                  2024-01-20T17:54:22.341318-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lettura di un file di testo"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Leggere un file di testo significa accedere e interpretare i dati contenuti in un file salvato sul disco. I programmatori fanno questo per elaborare o trasformare i testo, come configurazioni, dati, o log di sistemi.

## Come fare:
Ecco un esempio di come leggere un file di testo in Gleam:

```gleam
import gleam/io

pub fn main() {
  let result = io.read_file("path/alla/mia/file.txt")

  case result {
    Ok(contents) -> io.println(contents)
    Error(error) -> io.println(error)
  }
}
```

Output di esempio se la lettura ha successo:
```
Ciao, io sono il contenuto del file!
```

Se il file non può essere letto, vedrai:
```
Error(CouldNotOpenFile)
```

## Analisi Approfondita
Leggere file di testo è una pratica vecchia quanto la programmazione stessa. Con l'avvento di linguaggi moderni come Gleam, l'approccio si è semplificato, ma il fondamento rimane lo stesso: aprire un file, leggerne il contenuto e chiuderlo.

In alternativa alla lettura sincrona mostrata sopra, ci sono modi per leggere file asincronamente o in streaming, utili per file molto grandi o per non bloccare l'esecuzione del programma.

Gleam nasce dalla famiglia di linguaggi funzionali tipizzati e si basa su BEAM, la macchina virtuale di Erlang. Questo offre a Gleam robustezza e concorrenza nativa. La lettura di file in Gleam è gestita in modo sicuro, catturando gli errori come valori anziché eccezioni sollevate.

## Vedi Anche
Per approfondire e vedere altri esempi:

- [BEAM Capabilities](https://erlang.org/doc/)
- [Gleam Book](https://gleam.run/book/)

Nota: i link forniti sono generalmente in inglese, dato che il materiale in italiano per Gleam è ancora limitato.
