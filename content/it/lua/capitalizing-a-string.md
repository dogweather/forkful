---
title:                "Capitalizzare una stringa"
html_title:           "Lua: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Che Cos'è e Perché?

Capitalizzare una stringa significa trasformare la prima lettera di una stringa o di ogni parola in maiuscolo. I programmatori lo fanno per questioni di leggibilità e formattazione.

## Come Fare:

Ecco un esempio semplice su come capitalizzare una stringa in Lua:

```Lua
stringa = 'ciao mondo'
stringa = stringa:gsub("(%l)(%w*)", function(a,b) return a:upper()..b end)
print(stringa)
```

Questo script ti stampa:

```Lua
'Ciao Mondo'
```

Ecco un altro esempio su come capitalizzare solo la prima lettera di una stringa:

```Lua
stringa = 'ciao mondo'
stringa = stringa:sub(1,1):upper() .. stringa:sub(2)
print(stringa)
```

Il risultato sarà:

```Lua
'Ciao mondo'
```

## Approfondimento

Una delle bellezze di Lua è che non esiste un "metodo ufficiale" per capitalizzare una stringa, forse perché questa operazione non è così comune nei programmi reali. Invece, tutto ciò che devi fare è manipolare le stringhe usando le funzioni di base di Lua.

Alternative? Ce ne sono molte. Con la funzione gsub, puoi fare pattern matching e operare su ogni parola separatamente come nel primo esempio. Il secondo esempio mostra come capitalizzare solo la prima lettera di una stringa. Esistono anche librerie di terze parti con funzioni di utilità stringa, se ne hai bisogno di più.

Che cosa succede sotto il cofano? Principalmente, queste operazioni si basano sulla tabella ASCII. Quando dici `a:upper()`, Lua cerca il corrispondente carattere maiuscolo nella tabella ASCII e lo sostituisce.

## Vedi Anche

Per ulteriori informazioni sulle stringhe e sulle funzioni delle stringhe in Lua, dai un'occhiata qui:

1. Programmazione in Lua: Stringhe (https://www.lua.org/pil/20.html)
2. Le funzioni della libreria stringa Lua (https://www.tutorialspoint.com/lua/lua_strings.htm)
3. Documentazione ufficiale Lua (https://www.lua.org/manual/5.4/manual.html#6.4)