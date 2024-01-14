---
title:    "Fish Shell: Capitalizzazione di una stringa"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Se stai iniziando a imparare il Fish Shell, potresti trovarti a dover manipolare le stringhe di testo. Una delle operazioni più comuni è la capitalizzazione di una stringa, ovvero rendere la prima lettera maiuscola.

Ma perché dovresti farlo? Ci sono molte ragioni, ad esempio per uniformare lo stile del testo, per rendere più leggibili i titoli o per rispettare alcune convenzioni di scrittura.

## Come fare

Se stai usando il Fish Shell, ci sono diverse opzioni per capitalizzare una stringa. Vediamo alcune di esse utilizzando l'operatore di sostituzione dei comandi ```sub```:

```
# Capitalizza solo la prima lettera
echo "ciao mondo" | sed 's/^\([a-z]\)/\U\1/'

# Capitalizza tutte le lettere
echo "ciao mondo" | sed 's/.*/\U&/'
```

Un'altra opzione è utilizzare la funzione ```string```, disponibile nel pacchetto Fish Shell's string.

```
# Capitalizza solo la prima lettera
echo (string upper --first "ciao mondo")

# Capitalizza tutte le lettere
echo (string upper "ciao mondo")
```

## Approfondimento

Se vuoi approfondire il tema della capitalizzazione delle stringhe nel Fish Shell, troverai una guida dettagliata sulla documentazione ufficiale (https://fishshell.com/docs/current/cmds/string.html). Inoltre, puoi esplorare ulteriori tecniche utilizzando l'operatore di sostituzione dei comandi ```sub```.

## Vedi anche

- Guida ufficiale alla documentazione del Fish Shell: https://fishshell.com/docs/current/cmds/string.html
- Comandi di sostituzione dei comandi Fish Shell: https://fishshell.com/docs/current/commands.html#substitutions