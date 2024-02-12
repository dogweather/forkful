---
title:                "Rimuovere le virgolette da una stringa"
aliases:
- it/bash/removing-quotes-from-a-string.md
date:                  2024-01-26T03:37:46.189108-07:00
model:                 gpt-4-0125-preview
simple_title:         "Rimuovere le virgolette da una stringa"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/bash/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Cosa e perché?
Rimuovere le virgolette da una stringa comporta l'eliminazione dei segni di citazione che racchiudono la stringa. I programmatori spesso desiderano farlo per sanificare i dati in input, preparare i dati a scopo di confronto, o aderire a un formato di dati specifico quando si interfacciano con altri programmi o sistemi.

## Come fare:
Bash offre diversi modi per rimuovere le virgolette dalle stringhe. Ecco alcuni esempi rapidi:

```Bash
#!/bin/bash

# Utilizzo della sostituzione di variabile per rimuovere sia le virgolette singole che doppie
STRING="\"Ciao, Mondo!\""
echo ${STRING//\"}

# Utilizzo di `tr` per eliminare le virgolette
STRING="'Ciao, Mondo!'"
echo $STRING | tr -d "\'"

# Utilizzo di `sed` per eliminare le virgolette
STRING="\"Ciao, Mondo!\""
echo $STRING | sed 's/"//g'
```

Output di esempio:

```
Ciao, Mondo!
Ciao, Mondo!
Ciao, Mondo!
```

## Approfondimento
Tempo fa, comandi Unix come `tr` e `sed` erano gli strumenti principali per l'elaborazione del testo. Sono ancora utilizzati oggi per la loro flessibilità e potenza nel gestire trasformazioni di testo come la rimozione delle virgolette. Costituiscono un pilastro nel toolkit di chi scrive script per la shell.

Bash di per sé si è evoluto nel tempo e la sostituzione delle variabili aggiunge un altro livello di semplicità per le manipolazioni di stringhe su piccola scala. Ti risparmia l'utilizzo di binari esterni, rendendo gli script un po' più efficienti.

Mentre `tr` è ottimo per eliminare caratteri, non gestisce modelli più complessi. `sed`, d'altro canto, utilizza espressioni regolari, quindi a volte è eccessivo e potrebbe essere più lento per operazioni semplici.

La scelta tra questi metodi dipende dal tuo caso specifico. Se devi rimuovere una varietà di virgolette e sei già nel contesto di uno script Bash, l'utilizzo della sostituzione delle variabili è una scelta ovvia per la sua semplicità. Ma se stai trasformando flussi di testo o dati su più linee, `tr` e `sed` sono i tuoi compagni ideali.

## Vedi anche:
- Il manuale GNU Bash, soprattutto le sezioni su Espansione dei Parametri e Espansione dei Parametri della Shell: https://www.gnu.org/software/bash/manual/
- Il manuale del comando `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- La panoramica dell'editor di flussi `sed`: https://www.gnu.org/software/sed/manual/sed.html
