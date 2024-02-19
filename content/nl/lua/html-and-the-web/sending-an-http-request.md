---
aliases:
- /nl/lua/sending-an-http-request/
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:07:41.154699-07:00
description: "Een HTTP-verzoek verzenden betekent een verzoek doen aan een externe\
  \ server voor gegevens of actie. Programmeurs doen dit om te interageren met\u2026"
lastmod: 2024-02-18 23:09:01.987875
model: gpt-4-0125-preview
summary: "Een HTTP-verzoek verzenden betekent een verzoek doen aan een externe server\
  \ voor gegevens of actie. Programmeurs doen dit om te interageren met\u2026"
title: Een HTTP-verzoek verzenden
---

{{< edit_this_page >}}

## Wat & Waarom?

Een HTTP-verzoek verzenden betekent een verzoek doen aan een externe server voor gegevens of actie. Programmeurs doen dit om te interageren met webservices, bronnen op te halen of te communiceren met API's.

## Hoe te:

Lua heeft geen ingebouwde ondersteuning voor HTTP, dus gebruiken we bibliotheken. Een gangbare keuze is `lua-requests`. Hier is een snel voorbeeld:

```lua
local requests = require('requests')

-- GET verzoek
local response = requests.get('https://api.example.com/data')
print(response.status_code)
print(response.text)

-- POST verzoek met wat gegevens
local post_response = requests.post('https://api.example.com/post', {data = {key1 = 'waarde1', key2 = 'waarde2'}})
print(post_response.status_code)
print(post_response.text)
```

Een voorbeelduitvoer kan er als volgt uitzien:

```lua
200
"{\"data\":\"Hier zijn de gegevens waar je om vroeg!\"}"

201
"{\"success\":true,\"message\":\"Gegevens ontvangen!\"}"
```

## Diepere Duik

Lua's eenvoud over HTTP wordt niet standaard ondersteund, wat is waar bibliotheken inspringen. `lua-requests` spiegelt de functionaliteit van de Python Requests-bibliotheek, waardoor het een fluitje van een cent is voor degenen die bekend zijn met Python.

Andere alternatieven zijn `LuaSocket` voor lager-niveau HTTP-werk en `luasocket.http` voor meer controle. Lua heeft ook bindings voor `libcurl` (via `Lua-cURL`) voor complexe HTTP-operaties.

Historisch gezien weerspiegelt het ontbreken van ingebouwde HTTP-ondersteuning Lua's roots in embedded systemen waar netwerkprogrammering geen prioriteit was. De evolutie door externe bibliotheken illustreert de aanpasbaarheid van de gemeenschap en de uitbreidbaarheid van de taal.

Wat betreft implementatie, wanneer je een HTTP-verzoek verzendt, gaat dit via het netwerk naar de gespecificeerde server. De server verwerkt het en antwoordt. Lua-bibliotheken abstraheren de socketprogrammering die nodig is, ze beheren alle details van netwerkcommunicatie zodat je je kunt focussen op het daadwerkelijke verzoek en antwoord.

## Zie Ook

- [lua-requests GitHub-repository](https://github.com/JakobGreen/lua-requests)
- [LuaSocket Referentiehandleiding](http://w3.impa.br/~diego/software/luasocket/http.html)
