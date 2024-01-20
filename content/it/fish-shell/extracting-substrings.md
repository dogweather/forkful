---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

# Estrazione di Sottostringhe in Fish Shell

## Cos'è e Perché?
L'estrazione di sottostringhe consiste nel prelevare una specifica porzione di testo da una stringa più ampia. I programmatori la utilizzano perché permette di manipolare o analizzare specifiche parti di un input testuale in vari contesti.

## Come si fa:
Ecco un esempio su come estrarre una sottostringa in Fish Shell.

```Fish Shell
set stringa "Ciao, mondo!"
echo (string sub -l 5 $stringa)
```
Questo comando estrae i primi cinque caratteri: "Ciao," viene visualizzato.

Un altro esempio:

```Fish Shell
set stringa "Ciao, mondo!"
echo (string sub -s 7 -l 5 $stringa)
```

Questo estrarre i cinque caratteri a partire dalla posizione 7: "mondo" viene stampato.

## Approfondimento
**Contesto storico:** L'estrazione di sottostringhe ha una lunga storia fin dai primi giorni della programmazione come mezzo per manipolare dati testuali.

**Alternative:** Mentre il comando `string sub` è specifico di Fish Shell, esistono alternative simili in altri shell script come Bash (`${string:position:length}`) o scripting Python (`string[start:end]`).

**Dettagli di implementazione:** In Fish Shell, l'estrazione delle sottostringhe utilizza un sistema di indici basato su unità Unicode piuttosto che su byte, il che significa che funzionerà correttamente anche con caratteri non ASCII.

## Per ulteriori riferimenti
1. La documentazione ufficiale di Fish Shell su 'string': https://fishshell.com/docs/current/cmds/string.html
2. Tutorial su Stack Overflow sulla gestione delle stringhe in Fish Shell: https://stackoverflow.com/questions/tagged/fish