---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Cos'è e Perché?
Leggere un file di testo significa interpretare ed assegnare a variabili i dati presenti in un file di testo. I programmatori lo fanno per manipolare o utilizzare queste informazioni all'interno delle loro applicazioni.

# Come Fare:
Ecco un esempio sull'uso del metodo `io.open` integrato in Lua per leggere un file di testo.

```Lua
-- Aprire il file in modalità lettura
local file = io.open("testo.txt", "r")

-- Controllare se il file esiste
if file then
    -- Leggere il file
    local contenuto = file:read("*a")
    print(contenuto)

    -- Chiudere il file
    file:close()
else
    print("Il file non esiste")
end
```

Supponiamo che "testo.txt" contenga le seguenti righe:
```
Ciao Mondo! 
Benvenuto al mondo di Lua.
```

L'output del codice sopra sarebbe:
```
Ciao Mondo! 
Benvenuto al mondo di Lua.
```

# Approfondimento
Nel contesto storico, Lua all'inizio non era dotata della capacità di leggere i file di testo. È stata aggiunta solo nelle versioni successive per renderla più completa come linguaggio di scripting.

Un'alternativa per leggere un file in Lua è usando il modulo LFS (LuaFileSystem). Il modulo LFS offre più opzioni e funzionalità, ma richiede di essere installato separatamente.

In termini di implementazione, la funzione `io.open` restituisce un oggetto del file, dal quale è possibile chiamare il metodo `read`. Questo metodo legge il contenuto del file e lo restituisce come stringa.

# Vedi Anche
- Documentazione ufficiale di Lua: http://www.lua.org/docs.html
- Un tutorial su come leggere i file in Lua: http://lua-users.org/wiki/FileInputOutput
- LuaFileSystem: https://keplerproject.github.io/luafilesystem/