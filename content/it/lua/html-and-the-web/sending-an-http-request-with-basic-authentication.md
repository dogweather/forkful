---
date: 2024-01-20 18:02:18.746700-07:00
description: "How to: Le richieste HTTP con autenticazione di base sono una pratica\
  \ standard dalla nascita del web. Questo metodo \xE8 semplice ma meno sicuro rispetto\
  \ a\u2026"
lastmod: '2024-04-05T22:50:57.367475-06:00'
model: gpt-4-1106-preview
summary: Le richieste HTTP con autenticazione di base sono una pratica standard dalla
  nascita del web.
title: Inviare una richiesta http con autenticazione di base
weight: 45
---

## How to:
```Lua
-- Carichiamo il modulo necessario
local http = require("socket.http")
local ltn12 = require("ltn12")
local mime = require("mime")

-- Username e password per l'autenticazione
local username = "tuo_username"
local password = "tua_password"

-- Codifichiamo le credenziali in base64
local auth = "Basic " .. mime.b64(username .. ":" .. password)

-- La tua URL che richiede autenticazione
local url = "http://esempio.com/dati"

-- Impostiamo gli headers per includere le credenziali di autenticazione
local response_body = {}
local res, code, response_headers = http.request{
    url = url,
    method = "GET",
    headers = {
        ["Authorization"] = auth
    },
    sink = ltn12.sink.table(response_body)
}

-- Controlliamo se la richiesta è andata a buon fine
if code == 200 then
    print("Accesso riuscito!")
    print(table.concat(response_body))
else
    print("Errore: " .. (code or "nessuna risposta"))
end
```

Output di esempio:
```
Accesso riuscito!
{...dati recuperati...}
```

## Deep Dive:
Le richieste HTTP con autenticazione di base sono una pratica standard dalla nascita del web. Questo metodo è semplice ma meno sicuro rispetto a meccanismi più moderni come OAuth. Comunque, è ancora usato per l'accesso a vecchie API o per test rapidi. Lua non offre funzionalità native di alto livello per le richieste web, quindi spesso si utilizza il modulo `socket.http` tramite `LuaSocket`, che fornisce funzionalità di rete. Per l'autenticazione, le credenziali vengono codificate in base64 e poi passate nell'header della richiesta. Alcune alternative per gli script Lua che richiedono sicurezza avanzata includono l'uso di moduli esterni per supportare HTTPS o l'uso di token invece delle credenziali.

## See Also:
- [LuaSocket's GitHub repository](https://github.com/diegonehab/luasocket)
- [HTTP authentication: Basic and Digest Access Authentication](https://tools.ietf.org/html/rfc2617)
- [Base64 Encoding Specifications](https://tools.ietf.org/html/rfc4648)
- [LuaSec for secured communication](https://github.com/brunoos/luasec)
