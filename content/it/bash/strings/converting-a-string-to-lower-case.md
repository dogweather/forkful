---
title:                "Conversione di una stringa in minuscolo"
aliases:
- /it/bash/converting-a-string-to-lower-case.md
date:                  2024-01-20T17:37:43.971699-07:00
model:                 gpt-4-1106-preview
simple_title:         "Conversione di una stringa in minuscolo"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Convertire una stringa in minuscolo significa trasformare tutti i caratteri da maiuscolo a minuscolo. I programmatori lo fanno per uniformare i dati, per confronti di testi insensibili al maiuscolo/minuscolo o per soddisfare standard di input.

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
