---
title:                "Generazione di numeri casuali"
html_title:           "Arduino: Generazione di numeri casuali"
simple_title:         "Generazione di numeri casuali"
programming_language: "Lua"
category:             "Lua"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

Generare numeri casuali in programmazione significa produrre valori non prevedibili in modo tale che ciascuno abbia le stesse probabilità di essere selezionato. I programmatori lo fanno per molteplici motivi, tra cui generare dati di test, simulare eventi aleatori o per implementare algoritmi di crittografia.

## Come fare:

È così semplice generare numeri casuali in Lua. Ecco un esempio di come farlo:

```Lua
math.randomseed(os.time())

local randNumber = math.random()
print(randNumber)
```
Quando esegui il codice sopra, otterrai una sorta di output casuale come:

```Lua
> 0.5493212677017
```

O, se vuoi generare un intero casuale tra due numeri, ad esempio 10 e 20, lo faresti così:

```Lua
math.randomseed(os.time())

local randInt = math.random(10, 20)
print(randInt)
```
Generando un output casuale tra 10 e 20:

```Lua
> 15
```

## Approfondimenti

La funzione math.random di Lua non produce veri numeri casuali, ma pseudocasuali. Questo perché si basa su un algoritmo deterministico. Questo è il motivo per cui usiamo la funzione math.randomseed con il tempo corrente per dare un "seme" diverso ad ogni esecuzione.

Anche se Lua offre la funzionalità di generazione di numeri casuali con la funzione math.random, ci sono altre librerie alternative come mersenne-twister o xoroshiro che offrono generatori di numeri pseudocasuali con periodi più lunghi e distribuzioni statistiche migliori.

## Scopri Di Più

Puoi approfondire la generazione di numeri casuali in Lua e in altri linguaggi di programmazione leggendo le seguenti risorse:

1. [Lua 5.2 Reference Manual](http://www.lua.org/manual/5.2/manual.html#6.7)
2. [Mersenne Twister in Lua](https://create.stephan-brumme.com/mersenne-twister/)

Non dimenticare che la pratica è la chiave per padroneggiare l'arte della programmazione!