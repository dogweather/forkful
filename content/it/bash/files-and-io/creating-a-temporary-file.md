---
date: 2024-01-20 17:39:27.124025-07:00
description: "How to: Una volta, si usava `mktemp` per avere nomi file sicuri nella\
  \ directory `/tmp`. Se non usi `mktemp`, rischi conflitti di nomi o problemi di\u2026"
lastmod: '2024-04-05T22:50:57.424373-06:00'
model: gpt-4-1106-preview
summary: Una volta, si usava `mktemp` per avere nomi file sicuri nella directory `/tmp`.
title: Creazione di un file temporaneo
weight: 21
---

## How to:
Creare un file temporaneo:
```Bash
tempfile=$(mktemp)
echo "Questo è un file temporaneo" > "$tempfile"
echo "Contenuto del file temporaneo:"
cat "$tempfile"
```
Uscita:
```
Questo è un file temporaneo
```
Eliminare il file quando hai finito:
```Bash
rm "$tempfile"
```

## Deep Dive
Una volta, si usava `mktemp` per avere nomi file sicuri nella directory `/tmp`. Se non usi `mktemp`, rischi conflitti di nomi o problemi di sicurezza. `mktemp` può creare sia file che directory. Con `mktemp -d` ottieni una directory temporanea.

```
tempdir=$(mktemp -d)
echo "Directory temporanea creata in: $tempdir"
```

Esistono alternative, come la variabile `$TMPDIR` o `tempfile`(obsoleta). La speciale directory `/tmp` è il posto dove di solito si mettono i file temporanei e molti sistemi la puliscono all'avvio.

## See Also
- `man mktemp` per info dettagliate su `mktemp`
- [Filesystem Hierarchy Standard](https://refspecs.linuxfoundation.org/FHS_3.0/fhs-3.0.pdf) per capire dove e perché i file temporanei vanno in `/tmp`.
- [GNU Coreutils Manual](https://www.gnu.org/software/coreutils/manual/coreutils.html) per altre operazioni su file e testo.
