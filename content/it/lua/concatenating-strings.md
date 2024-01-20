---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/concatenating-strings.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Concatenando le stringhe, noi uniamo due o più frammenti di testo in un unico pezzo. I programmatori fanno questo per creare stringhe dinamiche o formattare l'output.

## Come si fa:

```Lua
string1 = "Ciao, "
string2 = "Mondo!"
print(string1 .. string2)
```

Output:

```Lua
"Ciao, Mondo!"
```

In Lua, usiamo l'operatore ".." per concatenare le stringhe.

## Approfondimenti

1. Storia: La concatenazione delle stringhe era una caratteristica della prima versione di Lua, risalente al 1993.
2. Alternative: la funzione string.format viene utilizzata per formattare le stringhe. 
  ``` Lua 
  string.format("%s %s", string1, string2)
  ```
3. Dettagli di implementazione: la concatenazione di stringhe in Lua non modifica le stringhe originali. Al contrario, crea una nuova stringa combinando quelle esistenti.

## Leggi di più

Per saperne di più sulla concatenazione di stringhe in Lua, consulta i seguenti link: 

1. Documentazione Lua: https://www.lua.org/manual/5.4/manual.html#3.4.5
2. Tutorial Lua: http://www.lua.org/pil/11.6.html