---
title:                "Leggere un file di testo"
html_title:           "Lua: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "Lua"
category:             "Lua"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Leggere un file di testo significa semplicemente aprire un file contenente del testo e accedere al suo contenuto. I programmatori fanno spesso uso di questa operazione per leggere e manipolare i dati contenuti nei file di testo, che sono una forma comune di archiviazione dei dati. 

## Come fare:

```Lua
-- apriamo il file di testo "example.txt" in modalità lettura
file = io.open("example.txt", "r")

-- leggiamo il contenuto del file e lo assegniamo alla variabile "content"
content = file:read("*all")

-- stampiamo il contenuto del file nella console
print(content)

-- chiudiamo il file
file:close()
```

```Lua
-- possiamo anche leggere il contenuto del file riga per riga usando un ciclo
-- in questo esempio, stamperemo ogni riga del file
-- apriamo il file in modalità lettura
file = io.open("example.txt", "r")

-- leggiamo e stampiamo ogni riga finché non arriviamo alla fine del file
while true do
  local line = file:read() -- leggiamo una riga
  if line == nil then break end -- se non ci sono più righe, usciamo dal ciclo
  print(line) -- stampiamo la riga
end

file:close() -- chiudiamo il file
```

## Approfondimento:

La lettura dei file di testo è una delle prime operazioni che i programmatori imparano quando iniziano ad utilizzare il computer. In passato, questa operazione era spesso utilizzata per leggere i file di configurazione dei computer, ma con l'avvento delle basi di dati, oggi viene meno spesso utilizzata. Tuttavia, è ancora una parte importante della programmazione e viene spesso utilizzata per leggere dati da file di log o per analizzare file di testo strutturati come CSV.

Per leggere file di testo in Lua, è anche possibile utilizzare la funzione `io.lines()` che restituirà un'iteratore sui dati del file. Inoltre, invece di utilizzare il metodo `read()` per leggere una sola riga alla volta, si può utilizzare il metodo `read("*all")` per leggere l'intero contenuto del file. 

Per approfondire la manipolazione dei file di testo in Lua, è possibile consultare la [documentazione ufficiale](https://www.lua.org/manual/5.3/manual.html#6.8) o [altre risorse online](https://www.tutorialspoint.com/lua/lua_standard_io_library.htm).

## Vedi anche:

- [Documentazione ufficiale di Lua](https://www.lua.org/)
- [Tutorial su Lua](https://www.tutorialspoint.com/lua/index.htm)
- [Altro approfondimento sull'uso dei file in Lua](https://www.tutorialspoint.com/lua/lua_files_io.htm)