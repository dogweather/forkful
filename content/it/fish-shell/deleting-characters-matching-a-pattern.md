---
title:                "Cancellazione di caratteri corrispondenti ad un pattern"
html_title:           "Fish Shell: Cancellazione di caratteri corrispondenti ad un pattern"
simple_title:         "Cancellazione di caratteri corrispondenti ad un pattern"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Perché

Vuoi eliminare i caratteri che corrispondono ad un determinato modello? Magari vuoi fare un po' di pulizia nel tuo codice o semplicemente automatizzare un'operazione che fai spesso. In ogni caso, imparare come farlo nel Fish Shell può essere estremamente utile.

## Come fare

Innanzitutto, dobbiamo stabilire quale pattern vogliamo eliminare. Per esempio, vogliamo eliminare tutti i caratteri che corrispondono a una lettera maiuscola. Utilizzando il comando `string`, possiamo specificare il pattern e sostituirlo con una stringa vuota per eliminarlo:
```Fish Shell
string replace --all --erase 'A-Z' ''
```
L'opzione `--all` assicura che tutti i caratteri corrispondenti al pattern vengano eliminati, mentre `--erase` indica che vogliamo sostituirli con una stringa vuota.

In alternativa, possiamo utilizzare il comando `tr`, che ci permette di trasformare i caratteri in base alla loro codifica ASCII. Ad esempio, per eliminare tutte le lettere maiuscole possiamo digitare:
```Fish Shell
tr -d 'A-Z'
```
In questo caso, il comando `tr` sostituirà ogni carattere corrispondente a un codice ASCII nello spazio vuoto.

## Approfondimento

Quindi, quali pattern possiamo utilizzare con questi comandi? In realtà, le possibilità sono infinite. Ad esempio, possiamo eliminare tutte le vocali da una stringa utilizzando il pattern `aeiou`, oppure possiamo eliminare numeri e caratteri speciali utilizzando `0-9!@#$%^&*()_+`.

Una nota importante da tenere a mente è che il Fish Shell utilizza le espressioni regolari di default, quindi se si vuole utilizzare una sintassi diversa, è necessario specificarlo nella variabile di ambiente `FISH_REGEX`.

## Vedi anche

- [Guida all'utilizzo del Fish Shell](https://fishshell.com/docs/current/index.html)
- [Documentazione completa del comando `string`](https://fishshell.com/docs/current/cmds/string.html)
- [Documentazione completa del comando `tr`](https://fishshell.com/docs/current/cmds/tr.html)