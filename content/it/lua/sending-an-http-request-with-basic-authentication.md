---
title:                "Inviare una richiesta http con autenticazione di base"
html_title:           "Bash: Inviare una richiesta http con autenticazione di base"
simple_title:         "Inviare una richiesta http con autenticazione di base"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?

Inviar un HTTP request con l'autenticazione di base in Lua significa trasmettere un pacchetto di dati a un server web usando un codice di accesso predefinito. I programmatori lo fanno per interfacciarsi con API esterne, accedere a database remoti, o intraprendere comunicazioni tra server.

## Come si fa:

Per inviare una richiesta HTTP con autenticazione di base in Lua, utilizziamo il modulo `luasocket` e `luasec`.

Qui di seguito, un esempio di codice:

```Lua
local http = require('socket.http')
local ltn12 = require('ltn12')

local url = 'http://mywebsite.com'
local user = 'myusername'
local password = 'mypassword'

local auth = 'Basic ' .. (user .. ':' .. password):gsub('(.-)', function(s) return string.format('%02x', s:byte()) end)
local response_body = {}

http.request{
    url = url,
    sink = ltn12.sink.table(response_body),
    headers = {
        authorization = auth
    },
}
```

In questo esempio, `socket.http` gestisce la connessione, `ltn12` gestisce il flusso di dati, e la sezione headers contiene l'autenticazione.

## Approfondimento:

L'autenticazione di base è un metodo di autenticazione HTTP reso possibile a partire dalla versione HTTP/1.0. È considerato un sistema rapido ed efficiente, ma è anche sensibile alla sicurezza date le credenziali inviate come testo in chiaro. Per questo, capacità di crittografia come SSL/TLS vengono raccomandate quando si usano queste tecniche.

Esistono anche alternative a questo metodo, come Digest Authentication, o autenticazioni basate su token, ma l'autenticazione di base rimane la più utilizzata grazie alla sua semplicità.

Per quanto riguarda i dettagli implementativi, è importante capire che dovremmo usare sempre connessioni sicure quando inviamo le nostre credenziali utilizzando l'autenticazione di base.

## Altre fonti:

2. [Modulo LuaSocket - Documentazione](http://w3.impa.br/~diego/software/luasocket/http.html)
3. [Modulo LuaSec - Documentazione](https://github.com/brunoos/luasec/wiki)