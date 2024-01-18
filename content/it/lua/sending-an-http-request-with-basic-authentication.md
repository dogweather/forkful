---
title:                "Inviare una richiesta HTTP con autenticazione di base"
html_title:           "Lua: Inviare una richiesta HTTP con autenticazione di base"
simple_title:         "Inviare una richiesta HTTP con autenticazione di base"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Invio di una richiesta HTTP con autenticazione di base è un modo per accedere a risorse protette su internet. I programmatori lo fanno per garantire che solo utenti autorizzati possano accedere alle risorse sensibili.

## Come Fare:
Ecco un esempio di codice Lua per inviare una richiesta HTTP con autenticazione di base:

```
local http = require("socket.http")
local mime = require("mime")

-- definisci i parametri della richiesta
local url = "https://www.example.com/"
local username = "utente"
local password = "password"

-- crea una stringa di autenticazione codificata
local auth = mime.b64(username .. ":" .. password)

-- invia la richiesta con l'intestazione di autenticazione
local result, status = http.request{
    url = url,
    headers = {["Authorization"] = "Basic " .. auth},
}

-- stampa il risultato e lo stato della richiesta
print("Risultato: " .. result)
print("Stato: " .. status)
```

Ecco un esempio di output se la richiesta è avvenuta con successo:

```
Risultato: <!DOCTYPE html>
<html>
<head>
    <title>Example Domain</title>
    ...
</head>
<body>
    <h1>HTTP Basic Authentication eseguita con successo!</h1>
</body>
</html>
Stato: 200 OK
```

## Approfondimento:
L'autenticazione di base HTTP è stata introdotta nelle specifiche HTTP 1.0 come un modo semplice di autenticazione. Tuttavia, è considerata poco sicura poiché le credenziali sono inviate in chiaro. Alcune alternative più sicure includono l'utilizzo di HTTPS o l'autenticazione a due fattori. L'implementazione della richiesta HTTP con autenticazione di base può variare a seconda della libreria utilizzata.

## Vedi Anche:
- Documentazione sulla libreria LuaSocket: https://w3.impa.br/~diego/software/luasocket/http.html
- Spiegazione sull'autenticazione di base HTTP: https://blogs.oracle.com/workmen/http-basic-authentication - Che cos'è l'autenticazione a due fattori: https://www.cloudflare.com/it-it/learning/access-management/what-is-two-factor-authentication/