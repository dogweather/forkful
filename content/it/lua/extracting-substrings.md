---
title:                "Estrazione di sottosequenze"
html_title:           "Arduino: Estrazione di sottosequenze"
simple_title:         "Estrazione di sottosequenze"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/extracting-substrings.md"
---

{{< edit_this_page >}}

# Estrazione delle sottostringhe in Lua

## Cos'è e perché?

Estirpare sottostringhe, in sostanza, significa prendere una piccola porzione di una stringa più grande. Lo facciamo perché spesso vogliamo elaborare o esaminare solo una parte specifica di una stringa, non il tutto.

## Come farlo

Ecco qualche esempio di come estrarre sottostringhe in Lua:

```Lua
str = "Ciao, mondo!"

-- estrore dall'indice 1 al 4
sub = string.sub(str, 1, 4)
print(sub)  -- stampa: Ciao
```

```Lua
str = "Benvenuti su Lua"

-- estrore dall'indice 12 in poi
sub = string.sub(str, 12)
print(sub)  -- stampa: Lua
```

```Lua
str = "Siete stati fantastici"

-- estrore dall'indice -4 (da destra) al -1
sub = string.sub(str, -4, -1)
print(sub)  -- stampa: asti
``` 
 
## Approfondimento

L'estrazione delle sottostringhe è un concetto fondamentale della programmazione di stringhe fin dai primi tempi della programmazione, e come tale è un'operazione presente in praticamente tutti i linguaggi di programmazione. 

Esistono alternative per estrarre sottostringhe in Lua, ad esempio le espressioni di pattern di Lua, molto simili alle espressioni regolari negli altri linguaggi.

Relativamente all'implementazione, la funzione `string.sub` di Lua accetta tre argomenti: la stringa originale, l'indice di inizio e l'indice di fine (facoltativo). L'indice di fine default è la fine della stringa. Gli indici negativi partono dalla fine della stringa.

## Vedi Anche

1. [Manual Lua 5.1 – String Manipulation](https://www.lua.org/manual/5.1/manual.html#5.4)
2. [Lua-Users Wiki: Strings Tutorial](http://lua-users.org/wiki/StringsTutorial)
3. [Programming in Lua: Patterns](http://www.lua.org/pil/20.2.html)