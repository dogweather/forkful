---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Cos'è e perché?
Generare numeri casuali è un processo di creazione di numeri in un modo non deterministico, cioè che non è prevedibile logicamente. I programmatori lo fanno per vari motivi, come garantire la sicurezza attraverso la crittografia o dare un elemento di casualità ai giochi.

## Come fare:
Utilizzare il comando `random` in Fish Shell per generare numeri casuali. Ecco un esempio su come farlo:

```Fish Shell
# Genera un numero casuale tra 1 e 100
set -l num (random 1 100)
echo $num
```
L'output potrebbe essere qualcosa del genere:

```Fish Shell
57
```
## Approfondimento
La generazione di numeri casuali ha avuto un ruolo importante nella storia dell'informatica. In Fish Shell, `random` utilizza un generatore di numeri pseudocasuali, che non è veramente casuale, ma sufficientemente buono per la maggior parte degli usi.

Ci sono numerose alternative a `random` in Fish Shell, come l'uso di `/dev/random` o `/dev/urandom` su sistemi Unix. Questi metodi si basano sulla raccolta di variazioni casuali nel sistema operativo.

Un dettaglio implementativo interessante di `random` in Fish Shell è che usa l'algoritmo Mersenne Twister, che è noto per essere uno dei migliori generatori di numeri pseudocasuali disponibili.

## Vedi anche:
1. [Documentazione ufficiale Fish Shell 'random' Command](https://fishshell.com/docs/current/commands.html#random)