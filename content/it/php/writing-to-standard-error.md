---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"

category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error (stderr) serve a separare l'output normale da messaggi di errore. I programmatori lo fanno per rendere i log più puliti e per gestire meglio i flussi di output durante il debugging.

## How to:
Usa `fwrite()` con `STDERR` per scrivere su standard error in PHP:

```PHP
<?php
fwrite(STDERR, "Questo è un messaggio di errore.\n");
?>
```

Output nel terminale:

```
Questo è un messaggio di errore.
```

## Deep Dive
`STDERR` in PHP esiste da PHP 4.3.0. C'è anche `error_log()`, ma è per i log, non per stderr direttamente. `STDERR` ti consente di scrivere direttamente sul flusso di errore del processo in esecuzione, che è particolarmente utile per CLI scripts.

## See Also
- Documentazione PHP su `fwrite()`: https://www.php.net/manual/it/function.fwrite.php
- Informazioni su PHP CLI: https://www.php.net/manual/it/features.commandline.php
- Articolo su flussi di output in PHP: https://www.php.net/manual/it/wrappers.php.php
