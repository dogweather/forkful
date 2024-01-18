---
title:                "Lavorare con json"
html_title:           "Lua: Lavorare con json"
simple_title:         "Lavorare con json"
programming_language: "Lua"
category:             "Lua"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/working-with-json.md"
---

{{< edit_this_page >}}

## Cos'è e perché si usa?

Working with JSON (JavaScript Object Notation) significa manipolare dati in formato testo leggibile dai umani e facilmente interpretabile dai computer. I programmatori lo utilizzano per scambiare informazioni tra applicazioni diverse in modo efficiente e affidabile.

## Come fare:

Scopriamo come lavorare con JSON utilizzando il linguaggio di programmazione Lua.

Utilizzando la libreria standard `dkjson`, possiamo facilmente convertire un'oggetto Lua in JSON utilizzando la funzione `encode`. Ad esempio:

```Lua
local dkjson = require("dkjson")

local data = {
  name = "Mario",
  age = 35,
  city = "Roma"
}

local json = dkjson.encode(data)

print(json)
```

Output:

`{"name":"Mario","age":35,"city":"Roma"}`

Possiamo anche utilizzare la funzione `decode` per convertire un JSON in un oggetto Lua. Ad esempio:

```Lua
local dkjson = require("dkjson")

local json = '{"name":"Luigi","age":28,"city":"Milano"}'

local data = dkjson.decode(json)

print(data.name, data.age, data.city)
```

Output:

`Luigi 28 Milano`

## Approfondimenti:

Il formato JSON è stato creato nel 2001 da Douglas Crockford come un'alternativa ai formati di scambio di dati più complessi come XML. Oggi è diventato uno standard ampiamente utilizzato nelle comunicazioni tra applicazioni web.

Un'alternativa alla libreria `dkjson` è `luajson`, che implementa una specifica Lua di JSON piuttosto che utilizzare una libreria esterna come `cjson`.

Per ulteriori dettagli su come utilizzare JSON in Lua, si consiglia di consultare la documentazione di `dkjson` o di `luajson`.

## Vedi anche:

- Documentazione di dkjson [https://github.com/LuaDist/dkjson/blob/master/doc.md]
- Documentazione di luajson [https://github.com/harningt/luajson/blob/master/README.md]