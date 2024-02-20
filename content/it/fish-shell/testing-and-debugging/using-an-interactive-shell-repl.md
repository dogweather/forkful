---
date: 2024-01-26 04:14:04.461405-07:00
description: "REPL, o Ciclo Leggi-Valuta-Stampa, \xE8 un ambiente di programmazione\
  \ interattivo che prende singoli input dell'utente, li esegue e restituisce il risultato.\u2026"
lastmod: 2024-02-19 22:05:02.932946
model: gpt-4-0125-preview
summary: "REPL, o Ciclo Leggi-Valuta-Stampa, \xE8 un ambiente di programmazione interattivo\
  \ che prende singoli input dell'utente, li esegue e restituisce il risultato.\u2026"
title: Utilizzo di un interprete interattivo (REPL)
---

{{< edit_this_page >}}

## Cosa e Perché?
REPL, o Ciclo Leggi-Valuta-Stampa, è un ambiente di programmazione interattivo che prende singoli input dell'utente, li esegue e restituisce il risultato. I programmatori lo usano per avere un feedback immediato, per il debugging e per l'esplorazione veloce dei concetti di codifica senza il peso di compilare ed eseguire un programma completo.

## Come fare:
In Fish, la shell interattiva è la modalità predefinita all'avvio. Ecco come appare in azione:

```Fish Shell
> set color blue
> echo "Il cielo è $color"
Il cielo è blue
```

Puoi anche eseguire funzioni incorporate e giocare con le sostituzioni di comandi:

```Fish Shell
> function cheer
      echo "Vai Fish $argv!"
  end
> cheer Coders
Vai Fish Coders!
```

Non solo definendo funzioni, puoi eseguire snippet di codice al volo e vedere l'output istantaneamente:

```Fish Shell
> math "40 / 2"
20
```

## Approfondimento
Il concetto di REPL risale al linguaggio di programmazione Lisp negli anni '60. Questa forma di programmazione interattiva ha stabilito il benchmark per ambienti come `ipython` di Python e `irb` di Ruby. Fish continua la tendenza con un focus sull'usabilità e sull'uso interattivo.

Fish si differenzia da altre shell come Bash in quanto è progettata con l'interattività in mente fin dall'inizio. Offre evidenziazione della sintassi, autosuggerimenti e completamenti automatici che lo rendono potente da utilizzare in un workflow in stile REPL. Meglio ancora, i tuoi comandi sono ricordati e ricercabili, rendendo il testing ripetuto un gioco da ragazzi.

Le alternative al REPL di Fish potrebbero essere `bash` o `zsh` quando abbinati a estensioni come `bash-completion` o `oh-my-zsh`, ma Fish tende ad offrire un'esperienza più ricca già pronta all'uso.

## Vedi Anche:
- Documentazione di Fish: https://fishshell.com/docs/current/index.html
- Un interessante confronto tra Fish e altre shell: https://www.slant.co/versus/2209/3686/~fish_vs_bash
- Un approfondimento sui REPL: https://en.wikipedia.org/wiki/Read–eval–print_loop
- Programmazione interattiva in Lisp, uno sguardo storico: http://www.paulgraham.com/ilisp.html
