---
date: 2024-01-26 03:40:31.203131-07:00
description: "Rimuovere le virgolette da una stringa significa spogliare il testo\
  \ di quei caratteri di virgoletta doppia o singola che lo abbracciano. I programmatori\u2026"
lastmod: '2024-03-13T22:44:43.546688-06:00'
model: gpt-4-0125-preview
summary: Rimuovere le virgolette da una stringa significa spogliare il testo di quei
  caratteri di virgoletta doppia o singola che lo abbracciano.
title: Rimuovere le virgolette da una stringa
weight: 9
---

## Che cosa e perché?
Rimuovere le virgolette da una stringa significa spogliare il testo di quei caratteri di virgoletta doppia o singola che lo abbracciano. I programmatori fanno ciò per sanificare gli input, per facilitare l'analisi, o per armonizzare dati che potrebbero essere quotati in modo inconsistente.

## Come fare:
Ecco come mandare al tappeto quelle virgolette in Lua:

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"Ciao, Mondo!"'))     -- Ciao, Mondo!
print(remove_quotes("'Addio, Virgolette!'"))  -- Addio, Virgolette!
```

Ecco fatto! Quelle virgolette sono scomparse come calzini in un'asciugatrice.

## Approfondimento
La gente ha eliminato le virgolette dalle stringhe da quando i linguaggi hanno potuto gestire il testo, il che è praticamente da sempre. In Lua, la funzione `gsub` fa gran parte del lavoro pesante, utilizzando pattern come un bisturi per rimuovere le virgolette. Alternative? Certo, potresti usare le espressioni regolari nei linguaggi che le supportano, o scrivere il tuo loop che si mangia ogni carattere (noioso, ma ehi, è il tuo tempo).

Il pattern matching di Lua ti dà la potenza di un'esperienza simile alle regex lite senza importare un'intera libreria. L'accento circonflesso (`^`) e il simbolo del dollaro (`$`) corrispondono rispettivamente all'inizio e alla fine della stringa; `%p` corrisponde a qualsiasi carattere di punteggiatura. Dopo avere tolto la punteggiatura iniziale e finale, catturiamo tutto il resto con `(.*),` e sostituiamo l'intero match con quel gruppo di cattura usando `" %1"`.

Ricorda che il pattern matching di Lua non è potente quanto i motori di espressioni regolari a pieno regime – per esempio, non può contare o fare backtracking. Questa semplicità è sia una benedizione sia una maledizione, a seconda delle virgolette che stai cercando di manipolare e di dove si nascondono.

## Vedi anche
Immergiti più a fondo nel pattern matching di Lua con il libro PiL (Programming in Lua): http://www.lua.org/pil/20.2.html

Per pura eleganza, vedi come lo fanno altri linguaggi per confronto, partendo da `str.strip` di Python: https://docs.python.org/3/library/stdtypes.html#str.strip
