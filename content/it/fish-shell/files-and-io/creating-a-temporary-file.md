---
date: 2024-01-20 17:40:16.697939-07:00
description: 'How to: In Fish, per creare un file temporaneo usi `mktemp`. Ecco un
  esempio semplice.'
lastmod: '2024-03-13T22:44:43.878970-06:00'
model: gpt-4-1106-preview
summary: In Fish, per creare un file temporaneo usi `mktemp`.
title: Creazione di un file temporaneo
weight: 21
---

## How to:
In Fish, per creare un file temporaneo usi `mktemp`. Ecco un esempio semplice:

```Fish Shell
set tempfile (mktemp)
echo "Questo è un file temporaneo: $tempfile"
# Utilizza il file per la tua operazione
# Poi cancella il file
rm $tempfile
```

Output esempio:

```
Questo è un file temporaneo: /tmp/tmp.I5L5FA6ALx
```

## Deep Dive
Il comando `mktemp` è standard nei sistemi Unix-like e genera file o directory con un nome unico in `/tmp` per evitare conflitti. Il suo utilizzo risale agli albori di UNIX, quando gestire la concorrenza nell’accesso ai file era un problema comune. 

Alternative moderne includono l'uso di file system virtuali in RAM, come con `/dev/shm` su sistemi Linux, che sono più veloci, ma hanno limitazioni di spazio. 

A livello di implementazione, Fish Shell utilizza le API del sistema operativo sottostante per garantire che i nomi dei file temporanei siano unici e non provochino collisioni, cosa essenziale per la sicurezza e l'affidabilità degli script.

## See Also
- Documentazione Fish Shell su variabili e espansione: https://fishshell.com/docs/current/#variables
- Tutorial GNU coreutils `mktemp`: https://www.gnu.org/software/coreutils/manual/html_node/mktemp-invocation.html
- Guida alle best practices per la scrittura di script in Fish: https://github.com/jorgebucaran/fish-shell-cookbook
