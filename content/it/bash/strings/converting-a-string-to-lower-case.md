---
date: 2024-01-20 17:37:43.971699-07:00
description: 'Come fare: Ecco alcuni modi per convertire una stringa in minuscolo
  in Bash.'
lastmod: '2024-03-13T22:44:43.585619-06:00'
model: gpt-4-1106-preview
summary: Ecco alcuni modi per convertire una stringa in minuscolo in Bash.
title: Conversione di una stringa in minuscolo
weight: 4
---

## Come fare:
Ecco alcuni modi per convertire una stringa in minuscolo in Bash:

```Bash
# Utilizzando la sintassi ${variabile,,}
str="CiAo Mondo!"
echo ${str,,}  # Output: ciao mondo!

# Con tr
echo "CiAo Mondo!" | tr '[:upper:]' '[:lower:]'  # Output: ciao mondo!

# Attraverso awk
echo "CiAo Mondo!" | awk '{print tolower($0)}'  # Output: ciao mondo!
```

## Approfondimento
Nei primi anni di sviluppo delle shell, non esistevano funzionalità native per la conversione delle stringhe. Gli utenti si affidavano a comandi esterni come `tr` e `awk`. Con Bash 4.0, introdotto nel 2009, arriva la feature `${variabile,,}` che semplifica il processo.

Diversi approcci possono essere usati:
1. Trasformazione nativa di Bash: veloce ed efficiente per script puri in Bash.
2. `tr`: utile per stream di testo e supportato da molte shell.
3. `awk`: potente per operazioni di trasformazione e manipolazione di testi complesse.

## Vedi Anche
- Bash man page: https://www.gnu.org/software/bash/manual/bash.html
- GNU 'tr' manuale: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- AWK manuale: https://www.gnu.org/software/gawk/manual/gawk.html
