---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error (stderr) permette di separare i messaggi di errore dall'output standard di un programma. I programmatori lo fanno per diagnosticare problemi e tracciare errori senza intasare il normale flusso di output.

## How to:
In Fish Shell, usa `>&2` per reindirizzare l'output a stderr.

```Fish Shell
echo "Questo è un messaggio di errore" >&2
```

Output esempio quando guardi solo l'stdout (`echo "msg" >/dev/null` per ignorare stdout):

```Fish Shell
echo "Normale output"
echo "Questo è un messaggio di errore" >&2 >/dev/null
```

Vedi solo l'errore:

```
Questo è un messaggio di errore
```

## Deep Dive
Nel contesto storico, stderr è stato introdotto con Unix come uno dei tre flussi di dati standard: input, output e errore. Un'alternativa a scrivere su stderr è l'uso di file di log, ma stderr è immediato e universale. Su Fish (e altri shell Unix-like), stderr ha il file descriptor 2, ed è per questa ragione che usiamo `>&2` per reindirizzare l'output.

## See Also
- Manuale di Fish Shell su reindirizzamento: [fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html#redirects)
- Confronto tra stdout e stderr: [gnu.org/software/libc/manual/html_node/Standard-Streams.html](https://www.gnu.org/software/libc/manual/html_node/Standard-Streams.html)
- Spiegazione approfondita di file descriptors: [en.wikipedia.org/wiki/File_descriptor](https://en.wikipedia.org/wiki/File_descriptor)
