---
title:                "Verifica se una directory esiste"
html_title:           "Lua: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

L'azione di verificare l'esistenza di una directory è banale: ci permette di sapere se una specifica directory esiste sul file system. I programmatori lo fanno per prevenire errori durante operazioni quali la lettura o la scrittura di file.

## Come fare:

Per verificare l'esistenza di una directory in Lua, puoi usare la funzione `os.execute` con il comando `cd`. Ecco un esempio di come si fa:

```Lua
local function directory_exists(dir)
    local exists = os.execute("cd " .. dir)
    if exists == true then
        return true
    else
        return false
    end
end

-- Esempio di utilizzo
print(directory_exists("/home"))  -- true se la directory esiste, false in caso contrario 
```
Se esegui questo script, vedrai un output simile a questo:

```Lua
true
```

## Deep Dive

Dal punto di vista storico, i programmatori hanno usato vari metodi per verificare l'esistenza di una directory. Con il tempo, alcuni svantaggi dei metodi precedenti hanno portato alla creazione di nuove funzioni, come `os.execute`.

Un metodo alternativo per controllare se una directory esiste è usare la funzione `lfs.attributes`. L'uso di `lfs` richiede però l'inclusione del modulo `luafilesystem`, che non è incluso in Lua di default.

Per quanto riguarda i dettagli di implementazione, quando `os.execute("cd " .. dir)` viene chiamato, se la directory non esiste, il comando `cd` restituirà un errore. In questo caso, `os.execute` restituirà `nil`, il che significa che la directory non esiste.

## Vedi Anche 

1. **LuaFileSystem**: un modulo orientato alla manipolazione dei file system per Lua. https://keplerproject.github.io/luafilesystem/
2. **Documentazione Lua**: completa documentazione sul linguaggio Lua e le sue funzioni integrate. https://www.lua.org/manual/5.3/
3. **StackOverflow Lua**: una sezione ben strutturata per domande e risposte su Lua. https://stackoverflow.com/questions/tagged/lua