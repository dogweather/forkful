---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/lua/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Scaricare una pagina web con Lua

## Cos'è e Perché?
Scaricare una pagina web è l'atto di prelevare i dati HTML di una pagina web per utilizzarli nel proprio codice. I programmatori lo fanno per estrarre dati, per monitorare le modifiche o per analizzare la struttura del sito.

## Come Fare:
Lua usa il modulo `socket.http` per scaricare pagine web. Vediamo un esempio semplice:

```Lua
local http = require("socket.http")

-- URL della pagina web da scaricare
local url = "http://www.google.com"

-- Scaricare la pagina web
local body, status = http.request(url)

-- Controllare se la richiesta è stata riuscita
if status == 200 then
  print(body)  -- Stampa il contenuto della pagina
else
  print("Errore durante il download: " .. status)
end
```

Quando esegui questo script, otterrai l'HTML di Google.com stampato sulla tua console.

## Approfondimento
### Contesto storico
Lua non nasce con il supporto per scaricare le pagine web. Questa caratteristica è stata aggiunta più tardi grazie al modulo opzionale 'luasocket'. 

### Alternative
Esistono altre librerie per scaricare pagine web in Lua, come 'lua-requests' o il più potente 'luacurl'. Questi moduli forniscono funzionalità più avanzate come il supporto per richieste multipart, l'autenticazione HTTP e molto altro.

### Dettagli implementativi
Per scaricare una pagina web, Lua apre una connessione TCP con il server remoto. Una volta instaurata la connessione, Lua invia una richiesta HTTP GET e poi legge la risposta del server. Tutti questi dettagli sono nascosti all'utente dal modulo `socket.http`.

## Vedere Anche
Per ulteriori informazioni o per approfondire gli argomenti trattati, consulta questi link:

- [LuaSocket HTTP client tutorial (Inglese)](http://w3.impa.br/~diego/software/luasocket/http.html)
- [Lua-requests su Github (Inglese)](https://github.com/JakobGreen/lua-requests)
- [LuaCurl su Github (Inglese)](https://github.com/Lua-cURL/Lua-cURLv3)