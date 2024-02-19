---
aliases:
- /it/lua/interpolating-a-string/
date: 2024-01-20 17:51:10.944210-07:00
description: "L'interpolazione di stringhe permette di incorporare variabili direttamente\
  \ all'interno di una stringa, per una concatenazione elegante e leggibile. I\u2026"
lastmod: 2024-02-18 23:08:55.997968
model: gpt-4-1106-preview
summary: "L'interpolazione di stringhe permette di incorporare variabili direttamente\
  \ all'interno di una stringa, per una concatenazione elegante e leggibile. I\u2026"
title: Interpolazione di una stringa
---

{{< edit_this_page >}}

## What & Why?
L'interpolazione di stringhe permette di incorporare variabili direttamente all'interno di una stringa, per una concatenazione elegante e leggibile. I programmatori la utilizzano perché rende il codice più pulito e facile da mantenere.

## How to:
In Lua non esiste un operatore di interpolazione di stringa nativo, ma si può utilizzare la funzione `string.format`.

```Lua
local nome = "Luca"
local eta = 30
local stringa_interpolata = string.format("Ciao, mi chiamo %s e ho %d anni.", nome, eta)
print(stringa_interpolata) -- Ciao, mi chiamo Luca e ho 30 anni.
```

Per stringhe multilinea o con formattazione complessa, potete usare i cosiddetti "literals" (stringhe letterali):

```Lua
local prezzo = 19.99
local prodotto = "tastiera"
local messaggio = [=[
Gentile cliente,
Il prezzo della tua %s è di €%.2f.
]=]

print(messaggio:format(prodotto, prezzo))
-- Gentile cliente,
-- Il prezzo della tua tastiera è di €19.99.
```

## Deep Dive
La funzione di `string.format` in Lua è simile alla funzione sprintf in C e alle sue varianti in altri linguaggi, offrendo un modo familiare per i programmatori per lavorare con le stringhe.

In Lua 5.1, l'interpolazione di stringa non è un'operazione integrata, il che può sembrare una limitazione. Ogni volta che chiamate `string.format`, esplicitamente dichiarate il tipo di formato per ogni valore (ad esempio, `%s` per le stringhe e `%d` per i numeri interi).

Gli alternative, come le template strings in JavaScript o le f-strings in Python, non sono supportate nativamente in Lua, quindi `string.format` rimane la soluzione preferita.

A livello di implementazione, l'uso delle funzioni di stringa può rallentare leggermente il vostro programma se usate l'interpolazione in grandi cicli o in funzioni chiamate frequentemente. È importante considerare l'overhead di prestazione per le applicazioni che richiedono alta performance.

## See Also
- [Programming in Lua (official book)](https://www.lua.org/pil/contents.html)
- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/)
