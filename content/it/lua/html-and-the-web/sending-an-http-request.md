---
title:                "Inviare una richiesta http"
aliases: - /it/lua/sending-an-http-request.md
date:                  2024-01-20T18:00:02.414069-07:00
model:                 gpt-4-1106-preview
simple_title:         "Inviare una richiesta http"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?
Inviare una richiesta HTTP significa chiedere dati o servizi da un server web. I programmatori lo fanno per scaricare pagine, interagire con API o inviare dati a servizi online.

## Come fare:
Per inviare una richiesta HTTP in Lua, useremo `socket.http` proveniente dalla libreria `LuaSocket`. Prima, installa la libreria con `luarocks install luasocket`. Ecco un esempio di come fare una richiesta GET:

```Lua
local http = require("socket.http")
local body, code, headers, status = http.request("http://example.com")

print("Corpo della Risposta: " .. body)  -- il contenuto della risposta
print("Codice di Stato: " .. code)       -- codice di stato HTTP
```

## Immersione Profonda:
`LuaSocket` è una libreria Lua standard per la programmazione di rete. Ha introdotto l'interazione HTTP in Lua, però per caso siano necessari HTTPS o funzionalità più avanzate, si potrebbe utilizzare `LuaSec`. Altre alternative includono `HTTPClient` e `wget` tramite `os.execute()`. L'implementazione con `LuaSocket` è semplice ma efficace per script non complicati e richieste di base.

## Vedi Anche:
- Documentazione LuaSocket: http://w3.impa.br/~diego/software/luasocket/http.html
- LuaSec, per HTTPS: https://github.com/brunoos/luasec/wiki
- Tutorial Lua: http://lua-users.org/wiki/TutorialDirectory
- Documentazione Lua 5.4 (versione attuale): https://www.lua.org/manual/5.4/
