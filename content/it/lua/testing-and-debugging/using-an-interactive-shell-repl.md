---
title:                "Utilizzo di un interprete interattivo (REPL)"
aliases:
- /it/lua/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:16:28.635415-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un interprete interattivo (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?
REPL sta per Read-Eval-Print Loop (Ciclo Leggi-Valuta-Stampa), un ambiente interattivo dove si può testare rapidamente del codice. I programmatori lo utilizzano per sperimentare, fare debug e apprendere le peculiarità di un linguaggio.

## Come fare:
Per accedere al REPL di Lua, basta inserire `lua` nel proprio terminale. Ecco un esempio di sessione:

```Lua
> x = 10
> print(x * 2)
20
> t = {'apple', 'banana', 'cherry'}
> table.insert(t, 'date')
> for i, fruit in ipairs(t) do print(i, fruit) end
1	apple
2	banana
3	cherry
4	date
>
```
Nella sessione, dichiariamo una variabile, eseguiamo operazioni aritmetiche di base, manipoliamo una tabella e cicliamo attraverso i suoi elementi.

## Approfondimento
La natura leggera di Lua rende il suo REPL ideale per la prototipazione. Esiste sin dall'inizio di Lua, nei primi anni '90, ispirato dalle precedenti shell interattive per linguaggi come Lisp. Alternative in altri linguaggi includono `irb` per Ruby e `python` per Python, ognuno con il proprio insieme di funzionalità. Il REPL di Lua è minimalista; quindi, potrebbe mancare di funzionalità avanzate presenti in altri, come strumenti di debug complessi. Per un’esperienza più ricca, strumenti come ZeroBrane Studio o LuaDist's LuaRocks offrono più del semplice REPL di base.

## Vedi anche
- [Manuale di riferimento di Lua 5.4 - L'interprete Lua Standalone](https://www.lua.org/manual/5.4/manual.html#6)
- [ZeroBrane Studio](https://studio.zerobrane.com/)
- [LuaRocks](https://luarocks.org/)
