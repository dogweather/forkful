---
date: 2024-01-20 17:55:48.477316-07:00
description: 'Come fare: Per accedere agli argomenti da riga di comando in Fish, usiamo
  `$argv`. Ecco un esempio semplice.'
lastmod: '2024-03-13T22:44:43.874819-06:00'
model: gpt-4-1106-preview
summary: Per accedere agli argomenti da riga di comando in Fish, usiamo `$argv`.
title: Lettura degli argomenti della riga di comando
weight: 23
---

## Come fare:
Per accedere agli argomenti da riga di comando in Fish, usiamo `$argv`. Ecco un esempio semplice:

```Fish Shell
# saluta.fish
for arg in $argv
    echo "Ciao, $arg!"
end
```

Esegui lo script con alcuni argomenti:
```bash
fish saluta.fish Mondo Programmatore
```

Risultato atteso:
```
Ciao, Mondo!
Ciao, Programmatore!
```

## Approfondimenti
Nei primi giorni dei sistemi Unix, gli argomenti della linea di comando davano agli utenti il controllo sugli script shell e i programmi. Nel Fish Shell, `$argv` è l'array che contiene questi argomenti, simile a `sys.argv` in Python o `ARGV` in Ruby. Altri shell come Bash o Zsh usano `$1`, `$2`, ecc., per accederli singolarmente o `$@`/`$*` per tutti. In Fish, puoi anche usare `(argparse)` e `(argparse -n ...)` per funzionalità più avanzate, come l'elaborazione delle opzioni.

## Vedi anche
- Documentazione ufficiale Fish per `$argv`: https://fishshell.com/docs/current/language.html#variables
- Fish Tutorial su Argparse: https://fishshell.com/docs/current/cmds/argparse.html
- Confronto tra shell: https://fishshell.com/docs/current/tutorial.html#tut_why_fish
