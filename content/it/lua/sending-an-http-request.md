---
title:                "Inviare una richiesta HTTP"
html_title:           "Lua: Inviare una richiesta HTTP"
simple_title:         "Inviare una richiesta HTTP"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Cosa & perché?
L'invio di una richiesta HTTP è un'operazione fondamentale nella programmazione, che consente ai programmatori di comunicare con i server web e di ricevere dati da loro. È un processo importante per lo sviluppo di applicazioni che richiedono l'accesso a informazioni da fonti esterne.

# Come fare:
In Lua, l'invio di una richiesta HTTP è relativamente semplice utilizzando la libreria standard "socket". Di seguito è riportato un esempio di codice che invia una richiesta GET al server "www.example.com" e stampa la risposta ricevuta.

```Lua
local socket = require("socket")

local host = "www.example.com"
local path = "/"
local c = assert(socket.connect(host, 80))

c:send("GET " .. path .. " HTTP/1.0\r\n\r\n")

local response = ""

while true do
  local s, status, partial = c:receive("*l")
  response = response .. (s or partial)
  if status == "closed" then break end
end

print(response)
```

L'output del codice dovrebbe essere una stringa contenente l'intera risposta del server.

# Approfondimento:
L'invio di richieste HTTP è diventato un processo comune nella programmazione web moderna, grazie all'aumento delle applicazioni basate su internet. In passato, venivano utilizzate altre tecnologie come il protocollo FTP, che è ancora utilizzato per il trasferimento di file da e verso un server.

Esistono anche alcune alternative alla libreria "socket" per l'invio di richieste HTTP in Lua, come ad esempio la libreria "LuaCURL". Ci sono anche molte librerie esterne disponibili per semplificare ancora di più il processo di invio e ricezione di richieste HTTP.

Per ottenere un maggiore controllo sul processo di invio di una richiesta HTTP, è possibile utilizzare la funzione "connect" della libreria "socket" per stabilire una connessione TCP con il server e quindi utilizzare le funzioni "send" e "receive" per inviare e ricevere i dati.

# Vedi anche:
- Documentazione ufficiale Lua socket: https://www.lua.org/manual/5.4/manual.html#8.2
- Libreria LuaCURL: https://github.com/Lua-cURL/Lua-cURLv3