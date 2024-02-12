---
title:                "Estrazione di sottostringhe"
aliases:
- /it/bash/extracting-substrings.md
date:                  2024-01-20T17:45:16.425975-07:00
model:                 gpt-4-1106-preview
simple_title:         "Estrazione di sottostringhe"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Estrarre sottostringhe vuol dire prendere parti specifiche da una stringa. I programmatori lo fanno per manipolare e analizzare i dati, o per personalizzare l'output.

## Come fare:
Ecco alcuni esempi:

```Bash
#!/bin/bash
# Esempio 1: Uso del parametro expansion
stringa="Ciao, mondo!"
echo ${stringa:0:5} # Produce "Ciao,"

# Esempio 2: Taglio basato su carattere
echo ${stringa#*, } # Produce "mondo!"

# Esempio 3: Sostituzione di una sottostringa
echo ${stringa/Ciao/Buongiorno} # Produce "Buongiorno, mondo!"
```

Output:
```
Ciao,
mondo!
Buongiorno, mondo!
```

## Approfondimenti:
Estrarre sottostringhe è una pratica diffusa fin dagli albori della programmazione. In Bash, l'estrazione di sottostringhe è potente e piena di funzioni:

1. **Contesto storico**: La sintassi e le funzionalità di Bash derivano da shell più antiche, come la Bourne Shell (sh). L'estrazione di sottostringhe è migliorata nel tempo, rendendo più facile manipolare le stringhe.

2. **Alternative**: Ci sono altri modi per estrapolare sottostringhe in Bash, come `awk`, `sed`, o `cut`. Ecco un esempio con `cut`:
   ```Bash
   echo "Ciao, mondo!" | cut -d ' ' -f 2 # Produce "mondo!"
   ```

3. **Dettagli sull'implementazione**: L'estrazione di sottostringhe in Bash si avvale del Parameter Expansion, che è la tecnica tramite la quale si possono manipolare i valori delle variabili. Supporta diverse operazioni come l'estrazione di lunghezza, il slicing, e le sostituzioni.

## Vedi anche:
- Bash Manual: https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
