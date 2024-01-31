---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
simple_title:         "Maiuscolizzare una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Cosa e Perché?)
Capitalizzare una stringa significa convertire le lettere iniziali in maiuscolo. I programmatori lo fanno per normalizzare dati come nomi o per enfatizzare titoli.

## How to: (Come fare:)
Ecco come capitalizzare stringhe in Bash:

```Bash
# Utilizzando il comando 'tr':
echo "benvenuti in italia" | tr '[:lower:]' '[:upper:]'
# Output: BENVENUTI IN ITALIA

# Solo la prima lettera:
echo "benvenuti in italia" | sed 's/.*/\u&/'
# Output: Benvenuti in italia

# Ogni parola:
echo "benvenuti in italia" | sed 's/\b./\u&/g'
# Output: Benvenuti In Italia
```

## Deep Dive (Approfondimento)
Prima di Bash 4, capitalizzare stringhe era più complesso perché non esisteva una funzione built-in. Questi metodi, `tr` e `sed`, sono storici e ancora usati oggi. Alternative includono l'uso di `awk` o linguaggi di scripting come Python. Internamente, il capitalizing avviene tramite mappatura delle lettere minuscole con le loro corrispondenti maiuscole nella tabella ASCII o Unicode.

## See Also (Vedi Anche)
- GNU `tr` Manual: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- GNU `sed` Manual: https://www.gnu.org/software/sed/manual/sed.html
- ASCII Table: https://www.asciitable.com/
- Shell Parameter Expansion (per versioni più nuove di Bash): https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
