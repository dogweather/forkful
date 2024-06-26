---
date: 2024-01-20 17:53:37.857004-07:00
description: 'How to: Ecco come leggere un file in Bash.'
lastmod: '2024-03-13T22:44:43.617027-06:00'
model: gpt-4-1106-preview
summary: Ecco come leggere un file in Bash.
title: Lettura di un file di testo
weight: 22
---

## How to:
Ecco come leggere un file in Bash:

```Bash
# Leggi l'intero file
cat miofile.txt

# Leggi il file riga per riga
while IFS= read -r line; do
   echo "Riga: $line"
done < miofile.txt
```

Supponiamo che `miofile.txt` sia:

```
Ciao, mondo!
Benvenuto in Bash.
```

Output:

```
Riga: Ciao, mondo!
Riga: Benvenuto in Bash.
```

## Deep Dive
Leggere file di testo è una pratica antica quanto la programmazione. `cat` è un comando classico, esiste fin dall'inizio dei sistemi Unix, dal 1971. Altre alternative includono `more`, `less`, e `head`, che offrono più controllo sull'output. Per quanto concerne l'implementazione, sembra semplice, ma vari fattori come la codifica dei caratteri e newline terminators (LF vs. CRLF) possono complicare l'interpretazione del contenuto di file testuali.

## See Also
- `man cat` per conoscere meglio il comando `cat`.
- GNU Core Utilities: https://www.gnu.org/software/coreutils/manual/coreutils.html
- Advanced Bash-Scripting Guide: http://www.tldp.org/LDP/abs/html/
