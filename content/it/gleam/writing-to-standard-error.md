---
title:                "Scrivere sull'errore standard"
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/gleam/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Scrivere su standard error (stderr) separa gli errori dai normali output del programma. I programmatori lo utilizzano per facilitare la diagnosi e il debug, mantenendo il flusso di lavoro coerente.

## Come si fa:
```Gleam
import gleam/io

pub fn main() {
  io.print("Questo va in standard output (stdout)\n")
  io.eprint("Questo è un errore e va in standard error (stderr)\n")
}
```

Output in stdout:
```
Questo va in standard output (stdout)
```

Output in stderr:
```
Questo è un errore e va in standard error (stderr)
```

## Approfondimento
Storicamente, separare stdout da stderr aiuta a maneggiare i flussi di output in modi diversi, per esempio, inviando gli errori a un file di log. Alcune alternative includono il reindirizzamento dell'output in file o il trattamento degli errori in modi personalizzati attraverso logging frameworks. In Gleam, `io.eprint` stampa specificatamente su stderr, il che è particolarmente utile quando i risultati del programma vengono incanalati o utilizzati da altri programmi.

## Altre Risorse
- [Understanding streams: stdout, stderr, and stdin](https://en.wikipedia.org/wiki/Standard_streams)
- [Guida alle redirection in shell UNIX/Linux](https://www.guru99.com/linux-redirection.html)