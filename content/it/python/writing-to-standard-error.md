---
title:                "Scrivere sull'errore standard"
date:                  2024-01-19
html_title:           "Arduino: Scrivere sull'errore standard"
simple_title:         "Scrivere sull'errore standard"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere su standard error (stderr) significa inviare messaggi di errore e log in un canale separato dall'output standard. Si fa per differenziare gli errori dai risultati validi e per facilitare il debugging.

## How to:
Python rende semplice scrivere su stderr tramite il modulo `sys`. Ecco come:

```Python
import sys

print("Questo va all'output standard", file=sys.stdout)
print("Questo è un messaggio di errore", file=sys.stderr)
```

Output:
```
Questo va all'output standard
Questo è un messaggio di errore
```

Se esegui questo script, il primo messaggio apparirà come normale output, mentre il secondo, quello di errore, andrà su stderr.

## Deep Dive
Nella storia dell'informatica, differenziare i canali di output è cruciale. In UNIX, il concetto di stderr esiste dagli anni '70. In Python, `sys.stderr` è un oggetto file-like, quindi si possono usare metodi come `.write()`. Alternativamente, è possibile usare moduli di logging, che offrono più flessibilità e configurazione. Gli errori scritti in stderr possono essere reindirizzati su file o gestiti separatemente se stai eseguendo lo script da una shell.

## See Also
- Documentazione Python su I/O: https://docs.python.org/3/tutorial/inputoutput.html
- Documentazione Python sul modulo `sys`: https://docs.python.org/3/library/sys.html
- Articolo sul logging in Python: https://docs.python.org/3/howto/logging.html
