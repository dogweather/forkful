---
title:                "Unire stringhe"
html_title:           "Lua: Unire stringhe"
simple_title:         "Unire stringhe"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che cos'è e perché? 
Concatenare le stringhe nel linguaggio di programmazione Lua significa unire più stringhe in una sola. Questa operazione è spesso utilizzata dai programmatori per creare stringhe più complesse e complete, come ad esempio un testo che include vari valori e variabili.

## Come fare:
In Lua, per concatenare due stringhe, è sufficiente utilizzare l'operatore ".." tra le due stringhe. Ad esempio:
```Lua
a = "ciao"
b = "mondo"
c = a .. b
print(c)
```
Questo codice produrrà l'output "ciaomondo".

Se si vuole aggiungere uno spazio tra le due stringhe, basta inserirlo tra virgolette all'interno dell'operatore "..". Per esempio:
```Lua
a = "ciao"
b = "mondo"
c = a .. " " .. b
print(c)
```
L'output sarà "ciao mondo".

È anche possibile concatenare più di due stringhe in una sola operazione, semplicemente aggiungendo l'operatore ".." tra tutte le stringhe. Come ad esempio:
```Lua
a = "hello"
b = "my"
c = "friend"
d = a .. " " .. b .. " " .. c
print(d)
```
L'output sarà "hello my friend".

## Approfondimenti:
Nel linguaggio di programmazione Lua, esistono anche altre modalità per unire le stringhe, come ad esempio utilizzare la funzione `string.format()` o la funzione `string.gsub()`. Tuttavia, l'utilizzo dell'operatore ".." è il metodo più semplice e veloce per concatenare le stringhe.

In passato, concatenare le stringhe era considerato un'operazione molto lenta nei linguaggi di programmazione, ma grazie alle ottimizzazioni dei moderni interpreti di Lua, questo non è più un problema.

## Vedi anche:
- [Documentazione ufficiale di Lua sulle stringhe (in inglese)](https://www.lua.org/manual/5.3/manual.html#6.4) 
- [Tutorial di concatenazione delle stringhe in Lua (in inglese)](https://www.tutorialspoint.com/lua/lua_strings.htm)