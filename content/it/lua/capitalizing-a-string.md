---
title:                "Maiuscolizzare una stringa"
date:                  2024-01-19
html_title:           "Bash: Maiuscolizzare una stringa"
simple_title:         "Maiuscolizzare una stringa"

category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizzare una stringa in Lua significa trasformare tutte le lettere minuscole in maiuscole. I programmatori lo fanno per uniformità, enfasi, o per assicurarsi che una stringa sia interpretata in modo consistente (come le chiavi in un dizionario).

## How to:
La capitalizzazione di una stringa in Lua è semplice:

```Lua
function capitalize(str)
    return string.upper(str)
end

-- Uso della funzione
local myString = "ciao mondo!"
local capitalizedString = capitalize(myString)
print(capitalizedString)  -- Output: CIAO MONDO!
```

Se vuoi capitalizzare solo la prima lettera:

```Lua
function capitalizeFirst(str)
    return str:sub(1,1):upper() .. str:sub(2)
end

-- Uso della funzione
local greeting = "ciao mondo!"
local capitalizedGreeting = capitalizeFirst(greeting)
print(capitalizedGreeting)  -- Output: Ciao mondo!
```

## Deep Dive
La funzione `string.upper()` in Lua è direttamente collegata alla libreria standard C, che offre funzionalità di manipolazione delle stringhe. Prima delle versioni più moderne, i programmatori dovevano scrivere queste funzioni da zero o utilizzare librerie esterne.

Una alternativa è l'uso di pattern matching in Lua per capitalizzare ogni parola in una stringa:

```Lua
function capitalizeWords(str)
    return (str:gsub("%f[%a](%a)", string.upper))
end

-- Uso della funzione
local title = "il signore degli anelli"
local capitalizedTitle = capitalizeWords(title)
print(capitalizedTitle)  -- Output: Il Signore Degli Anelli
```

Qui, `%f[%a]` è un pattern che individua una transizione da non-lettera a lettera e `(%a)` matcha la prima lettera della parola.

## See Also
Ecco alcune risorse per approfondire:

- [Documentazione ufficiale Lua](https://www.lua.org/manual/5.4/)
- [Tutorial su stringhe Lua](https://www.tutorialspoint.com/lua/lua_strings.htm)
- Post di Stack Overflow su [come capitalizzare le stringhe](https://stackoverflow.com/questions/20284515/capitalize-first-letter-of-every-word-in-lua) in Lua.
